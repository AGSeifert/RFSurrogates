#' Get a list of structured trees from a ranger object.
#'
#' This functions creates a list of trees for ranger objects similar as getTree function does for random Forest objects.
#'
#' @param RF A [`ranger::ranger`] object which was created with `keep.inbag = TRUE`.
#' @param num.trees (Deprecated) Number of trees to convert (Default: `RF$num.trees`).
#' @param add_layer (Default: `FALSE`) Whether to [addLayer()] in the same loop.
#' @param num.threads (Default: 1) Number of threads to spawn for parallelization.
#'
#' @returns A list of tree data frames of length `RF$num.trees`.
#' Each row of the tree data frames corresponds to a node of the respective tree and the columns correspond to:
#' * `nodeID`: ID of the respective node (important for left and right daughters in the next columns)
#' * `leftdaughter`: ID of the left daughter of this node
#' * `rightdaughter`: ID of the right daughter of this node
#' * `splitvariable`: ID of the split variable
#' * `splitpoint`: Split point of the split variable.
#'    For categorical variables this is a comma separated lists of values, representing the factor levels (in the original order) going to the right.
#' * `status`: `0` for terminal (`splitpoint` is `NA`) and `1` for non-terminal.
#' * `layer`: If `add_layer` is `TRUE`, see [addLayer()]
#'
#' @export
getTreeranger <- function(RF, num.trees = RF$num.trees, add_layer = FALSE, num.threads = 1) {
  trees <- parallel::mclapply(1:num.trees, getsingletree,
    mc.cores = num.threads,
    RF = RF,
    add_layer = add_layer
  )

  class(trees) <- "RangerTrees"
  if (add_layer) {
    class(trees) <- c(class(trees), "LayerTrees")
  }

  return(trees)
}

#' getsingletree
#'
#' This is an internal function
#'
#' @param RF A [`ranger::ranger`] object.
#' @param k Tree index to convert.
#' @param add_layer
#'
#' @returns A tree data frame for the `k`th tree in `RF`.
#' Each row of the tree data frames corresponds to a node of the respective tree and the columns correspond to:
#' * `nodeID`: ID of the respective node (important for left and right daughters in the next columns)
#' * `leftdaughter`: ID of the left daughter of this node
#' * `rightdaughter`: ID of the right daughter of this node
#' * `splitvariable`: ID of the split variable
#' * `splitpoint`: Split point of the split variable.
#'    For categorical variables this is a comma separated lists of values, representing the factor levels (in the original order) going to the right.
#' * `status`: `0` for terminal (`splitpoint` is `NA`) and `1` for non-terminal.
#'
#' @keywords internal
getsingletree <- function(RF, k = 1, add_layer = FALSE) {
  # here we use the treeInfo function of the ranger package to create extract the trees, in an earlier version this was done with a self implemented function
  tree.ranger <- ranger::treeInfo(RF, tree = k)
  ktree <- data.frame(
    nodeID = as.numeric(tree.ranger$nodeID + 1),
    leftdaughter = as.numeric(tree.ranger$leftChild + 1),
    rightdaughter = as.numeric(tree.ranger$rightChild + 1),
    splitvariable = as.numeric(tree.ranger$splitvarID + 1),
    splitpoint = tree.ranger$splitval,
    status = as.numeric(!tree.ranger$terminal)
  )
  if (is.factor(ktree[, "splitpoint"])) {
    ktree[, "splitpoint"] <- as.character(levels(ktree[, "splitpoint"]))[ktree[, "splitpoint"]]
  }

  ktree[, 2:4][is.na(ktree[, 2:4])] <- 0

  if (add_layer) {
    ktree <- add_layer_to_tree(ktree)
  }

  return(ktree)
}
