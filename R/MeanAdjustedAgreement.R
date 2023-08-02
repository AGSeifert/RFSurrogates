#' Investigate variable relations of a specific variable with mean adjusted agreement
#'
#' This function uses the mean adjusted agreement to select variables that are related to a defined variable using a threshold T.
#' The parameter t is used to calculate T: t=1 means that every variable with higher probability than "by chance" is identified
#' as "important". t=2 means the probability has to be twice, etc.
#' Based on the threshold a vector is created containing the related variables.
#'
#' @param RFS A `RandomForestSurrogates` object.
#' @param t Used to calculate threshold. (Default: 5)
#' @param variables Vector of variable names for **which related variables should be searched**. (Default: All variables used to create the random forest.)
#' @param candidates Vector of variable names that **are candidates to be related to the variables**. (Default: All variables used to create the random forest.)
#' @param related (Default: `TRUE`) Whether related variables should be identified.
#' @param num.threads (Default: 1) Number of threads used for determination of relations.
#'
#' @return A `MeanAdjustedAgreement` list object:
#'  * `RFS`: The original [`RandomForestSurrogates`] object.
#'  * `relations`: Matrix with mean adjusted agreement values
#'    * Rows: `variables`.
#'    * Columns: `candidates`.
#'  * `threshold`: the threshold used to select related variables.
#'  * `related`: A list of vectors for each `variable` containing related `candidates`. Only present if `related = TRUE` (Default).
#'
#' @examples
#' \donttest{
#' data("SMD_example_data")
#' rfs <- RandomForestSurrogates(
#'   x = SMD_example_data[, -1]
#'   y = SMD_example_data[, 1]
#'   s = 10,
#'   seed = 42,
#'   num.trees = 10,
#'   num.threads = 1
#' )
#' maa <- MeanAdjustedAgreement(
#'   rfs,
#'   variables = c("X7", "X1"),
#'   candidates = colnames(SMD_example_data)[2:101],
#'   t = 5,
#'   num.threads = 1
#' )
#' }
#'
#' @keywords varrel
#' @export
MeanAdjustedAgreement <- function(
  RFS,
  t = 5,
  variables = RFS$ranger$forest$independent.variable.names,
  candidates = RFS$ranger$forest$independent.variable.names,
  related = TRUE,
  num.threads = 1
) {
  if (!inherits(RFS, "RandomForestSurrogates")) {
    stop("`RFS` must be a `RandomForestSurrogates` object.")
  }

  all_variables <- RFS$ranger$forest$independent.variable.names

  if (!all(candidates %in% all_variables)) {
    stop("`candidates` contains variables not in `RFS$ranger$forest$independent.variable.names`")
  }
  if (!all(variables %in% all_variables)) {
    stop("`variables` contains variables not in `RFS$ranger$forest$independent.variable.names`")
  }

  s <- count.surrogates(RFS$trees)

  maa <- meanAdjAgree(
    trees = RFS$trees,
    variables = variables,
    allvariables = all_variables,
    candidates = candidates,
    t = t,
    s.a = s$s.a,
    select.var = related,
    num.threads = num.threads
  )

  results = list(
    RFS = RFS,
    relations = maa$surr.res,
    threshold = maa$threshold
  )
  class(results) <- "MeanAdjustedAgreement"

  if (related) {
    surr.var <- maa$surr.var
    varlist <- list()
    for (i in 1:nrow(surr.var)) {
      surr.var.var <- surr.var[i, ]
      if (anyNA(surr.var.var)) {
        surr.var.var <- surr.var.var[-which(is.na(surr.var.var))]
      }
      var <- names(surr.var.var[surr.var.var == 1])
      name <- variables[i]
      varlist[[name]] <- var
    }

    results$related <- varlist
  }

  return(results)
}
