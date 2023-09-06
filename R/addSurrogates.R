#' Add surrogate information to a tree list.
#'
#' This function adds surrogate variables and adjusted agreement values to a forest that was created by [getTreeranger].
#'
#' @param RF A [ranger::ranger] object which was created with `keep.inbag = TRUE`.
#' @param trees List of trees created by [getTreeranger].
#' @param s Predefined number of surrogate splits (it may happen that the actual number of surrogate splits differs in individual nodes).
#' @param Xdata data without the dependent variable.
#' @param num.threads (Default: [parallel::detectCores()]) Number of threads to spawn for parallelization.
#' @param preschedule.threads (Default: TRUE) Passed as `mc.preschedule` to [parallel::mclapply()].
#'
#' @returns A list of trees.
#' A list of trees containing of lists of nodes with the elements:
#' * `nodeID`: ID of the respective node (important for left and right daughters in the next columns)
#' * `leftdaughter`: ID of the left daughter of this node
#' * `rightdaughter`: ID of the right daughter of this node
#' * `splitvariable`: ID of the split variable
#' * `splitpoint`: splitpoint of the split variable
#' * `status`: `0` for terminal and `1` for non-terminal
#' * `layer`: layer information (`0` means root node, `1` means 1 layer below root, etc)
#' * `surrogate_i`: numbered surrogate variables (number depending on s)
#' * `adj_i`: adjusted agreement of variable i
#'
#' @export
addSurrogates <- function(
    RF,
    trees,
    s,
    Xdata,
    num.threads = parallel::detectCores(),
    preschedule.threads = TRUE
) {
  if (!inherits(RF, "ranger")) {
    stop("`RF` must be a ranger object.")
  }

  if (!inherits(trees, "RangerTrees")) {
    stop("`trees` must be a `getTreeranger` `RangerTrees` object.")
  }

  num.trees <- RF$num.trees

  if (num.trees != length(trees)) {
    stop("Number of trees in ranger model `RF` does not match number of extracted trees in `trees`.")
  }

  ncat <- sapply(sapply(Xdata, levels), length) # determine number of categories (o for continuous variables)
  names(ncat) <- colnames(Xdata)

  if (any(ncat) > 0) {
    Xdata[, which(ncat > 0)] <- sapply(Xdata[, which(ncat > 0)], unclass)
  }

  # variables to find surrogates (control file similar as in rpart)
  controls <- list(maxsurrogate = as.integer(s), sur_agree = 0)

  trees.surr <- parallel::mclapply(
    X = mapply(list, .a = trees, .b = RF$inbag.counts, SIMPLIFY = FALSE),
    FUN = getSurrgate2,

    mc.cores = num.threads,
    mc.preschedule = preschedule.threads,

    Xdata = Xdata,
    controls = controls,
    s = s,
    ncat = ncat
  )

  class(trees.surr) <- c(class(trees), "SurrogateTrees")

  return(trees.surr)
}

#' getSurrogate2
#'
#' This is an internal function
#'
#' @param x List of length `num.trees`
#'
#' @keywords internal
getSurrgate2 <- function(
  x, # list of length num.trees with [1] tree and [2] inbag.counts
  Xdata,
  controls,
  s,
  ncat
) {
  tree <- x[[1]]
  lapply(
    X = seq_len(nrow(tree)),
    FUN = SurrTree,

    tree = tree,
    wt = x[[2]],
    Xdata = Xdata,
    controls = controls,
    column.names = colnames(tree),
    maxsurr = s,
    ncat = ncat
  )
}

#' getSurrogate
#'
#' This is an internal function
#'
#' @keywords internal
getSurrogate <- function(surr.par, k = 1, maxsurr) {
  # weights and trees are extracted
  tree <- surr.par$trees[[k]]
  column.names <- colnames(tree)
  n.nodes <- nrow(tree)
  wt <- surr.par$inbag.counts[[k]]
  tree.surr <- lapply(1:n.nodes,
    SurrTree,
    wt = wt,
    Xdata = surr.par$Xdata,
    controls = surr.par$controls,
    column.names, tree, maxsurr,
    ncat = surr.par$ncat
  )
}

#' SurrTree
#'
#' This is an internal function
#'
#' @useDynLib RFSurrogates, .registration = TRUE
#'
#' @keywords internal
SurrTree <- function(j, wt, Xdata, controls, column.names, tree, maxsurr, ncat) {
  node <- tree[j, ]
  # for non-terminal nodes get surrogates
  if (node["status"] == 1) {
    # Handover to C
    var <- as.numeric(node[4]) # extract split variable

    if (ncat[var] == 0) { # extract split information: split point for continuous variables and directions for qualitative variables
      split <- as.numeric(node[5])
    } else {
      right <- as.numeric(strsplit(as.character(node[5]), ",")[[1]])
      directions <- rep(-1, ncat[var])
      directions[right] <- 1
      split <- as.numeric(c(ncat[var], directions))
    }

    surrogate.parameters <- .Call("getSurrogates",
      ncat = as.integer(ncat),
      wt = as.numeric(wt),
      X = as.matrix(Xdata),
      controls = as.integer(unlist(controls)),
      var = as.integer(var), # node variables
      split = as.numeric(split)
    ) # split info

    if (nrow(surrogate.parameters$isplit) > 1) {
      surrogates <- surrogate.parameters$isplit[2:nrow(surrogate.parameters$isplit), 1]
      surr.adj <- round(surrogate.parameters$dsplit[2:nrow(surrogate.parameters$dsplit), 1], 2)
      node.new <- data.frame(matrix(nrow = 1, ncol = 7 + length(surrogates) + length(surr.adj)))
      node.new[, 1:7] <- node[1:7]
      node.new[, 8:(7 + length(surrogates) + length(surr.adj))] <- c(surrogates, surr.adj)
      surrogate.names <- NULL
      adj.names <- NULL
      surrogate.names <- sapply(1:length(surrogates), name.surr, surrogate.names)
      adj.names <- sapply(1:length(surrogates), name.adj, adj.names)
      names(node.new) <- c(column.names, surrogate.names, adj.names)
    }

    if (nrow(surrogate.parameters$isplit) == 1) {
      node.new <- node
    }
  }
  if (node["status"] == 0) {
    node.new <- node
  }
  return(node.new)
}

#' name.surr
#'
#' This is an internal function
#'
#' @keywords internal
name.surr <- function(i, surrogate.names) {
  surrogate.names <- c(surrogate.names, paste0("surrogate_", i))
  return(surrogate.names)
}

#' name.adj
#'
#' This is an internal function
#'
#' @keywords internal
name.adj <- function(i, adj.names) {
  adj.names <- c(adj.names, paste0("adj_", i))
  return(adj.names)
}
