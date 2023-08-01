#' Add layer information to a forest that was created by getTreeranger
#'
#' This functions adds the layer information to each node in a list with trees that was obtained by getTreeranger.
#' You should use [`getTreeranger()`] with `add_layer = TRUE` instead.
#'
#' @param trees The output of [`getTreeranger()`].
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
#' * `layer`: Tree layer depth information, starting at 0 (root node) and incremented for each layer.
#'
#' @export
addLayer <- function(trees, num.threads = 1) {
  parallel::mclapply(trees, add_layer_to_tree, mc.cores = num.threads)
}

#' Internal function
#'
#' This function adds the respective layer to the different nodes in a tree.
#' The tree has to be prepared by getTree function.
#'
#' @param tree A tree data frame from [getTreeranger()].
#'
#' @returns A tree data frame with `layer` added.
#'
#' @seealso [addLayer()]
#'
#' @keywords internal
add_layer_to_tree <- function(tree) {
  layer <- rep(NA, nrow(tree))
  layer[1] <- 0
  t <- 1
  while (anyNA(layer)) {
    r <- unlist(tree[which(layer == (t - 1)), 2:3])
    layer[r] <- t
    t <- t + 1
  }
  tree <- cbind(tree, layer)
  tree <- tree[order(as.numeric(tree[, "layer"])), ]

  return(tree)
}
