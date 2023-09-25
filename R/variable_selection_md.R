#' Variable selection with Minimal Depth (MD)
#'
#' This function executes MD applying \link[ranger]{ranger} for random forests generation and is a reimplementation of \link[randomForestSRC]{var.select} from randomForestSRC package.
#'
#' @param x data.frame of predictor variables with variables in
#'   columns and samples in rows. (Note: missing values are not allowed)
#' @param y vector with values of phenotype variable (Note: will be converted to factor if
#'   classification mode is used). For survival forests this is the time variable.
#' @param num.trees Number of trees. Default is 500.
#' @param mtry Number of variables to possibly split at in each node. Default is no. of variables^(3/4) as recommended by Ishwaran.
#' @param type Mode of prediction ("regression","classification" or "survival"). Default is regression.
#' @param min.node.size Minimal node size. Default is 1.
#' @param num.threads number of threads used for parallel execution. Default is number of CPUs available.
#' @param status status variable, only applicable to survival data. Use 1 for event and 0 for censoring.
#' @param save.ranger Set TRUE if ranger object should be saved. Default is that ranger object is not saved (FALSE).
#' @param create.forest Default: TRUE if `forest` is NULL, FALSE otherwise. Whether to create or use an existing forest.
#' @param forest the random forest that should be analyzed.
#' @param save.memory Use memory saving (but slower) splitting mode. No effect for survival and GWAS data. Warning: This option slows down the tree growing, use only if you encounter memory problems. (This parameter is transfered to ranger)
#' @param case.weights Weights for sampling of training observations. Observations with larger weights will be selected with higher probability in the bootstrap (or subsampled) samples for the trees.
#'
#' @return List with the following components:
#'  * `info`: list with results from [mindep()] function:
#'    * `depth`: mean minimal depth for each variable.
#'    * `selected`: variables has been selected (1) or not (0).
#'    * `threshold`: the threshold that is used for the selection. (deviates slightly from the original implementation)
#'  * `var`: vector of selected variables.
#'  * `forest`: a list containing:
#'    * `trees`: list of trees that was created by [getTreeranger()], [addLayer()], and [addSurrogates()] functions and that was used for surrogate minimal depth variable importance.
#'    * `allvariables`: all variable names of the predictor variables that are present in x.
#'  * `ranger`: ranger object
#'
#' @examples
#'
#' \donttest{
#' data("SMD_example_data")
#' set.seed(42)
#' res <- var.select.md(
#'   x = SMD_example_data[, 2:ncol(SMD_example_data)],
#'   y = SMD_example_data[, 1], num.trees = 10, num.threads = 1
#' )
#' res$var
#' }
#'
#' @references
#' * Ishwaran, H. et al. (2011) Random survival forests for high-dimensional data. Stat Anal Data Min, 4, 115–132. <https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.10103>
#' * Ishwaran, H. et al. (2010) High-Dimensional Variable Selection for Survival Data. J. Am. Stat. Assoc., 105, 205–217. <http://www.ccs.miami.edu/~hishwaran/papers/IKGML.JASA.2010.pdf>
#'
#' @export
var.select.md <- function(x = NULL, y = NULL, num.trees = 500, type = "regression", mtry = NULL, min.node.size = 1, num.threads = NULL,
                          status = NULL, save.ranger = FALSE, create.forest = is.null(forest), forest = NULL, save.memory = FALSE, case.weights = NULL) {
  results.smd <- var.select.smd(
    x = x, y = y, num.trees = num.trees, type = type, mtry = mtry, min.node.size = min.node.size, num.threads = num.threads,
    status = status, save.ranger = save.ranger, s = 0, create.forest = create.forest, forest = forest,
    save.memory = save.memory, case.weights = case.weights
  )
  if (save.ranger) {
    results <- list(info = results.smd$info, var = results.smd$var, forest = results.smd$forest, ranger = results.smd$ranger)
  } else {
    results <- list(info = results.smd$info, var = results.smd$var, forest = results.smd$forest)
  }
  return(results)
}
