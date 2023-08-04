#' Create a random forest with surrogates.
#'
#' The object created by this functions serves as the basis of most further analysis,
#' such as [SurrogateMinimalDepth()] and [MeanAdjustedAgreement()].
#'
#' @inheritParams ranger::ranger
#' @inheritDotParams ranger::ranger importance min.bucket max.depth replace sample.fraction case.weights class.weights splitrule num.random.splits alpha minprop split.select.weights always.split.variables scale.permutation.importance local.importance regularization.factor regularization.usedepth inbag holdout quantreg oob.error save.memory verbose
#'
#' @param x,y Predictor data and dependent variables.
#'
#' @param s.pct,s Number of surrogate splits.
#'   This can be defined either by setting `s.pct` to a number between
#'   0 and 1, or providing an exact value for `s`.
#'   - `s.pct`: Percentage of variables to use for `s`. (Default: 0.01)
#'   - `s`:  Number of surrogate splits. (Default: Number of variables
#'     multiplied by `s.pct`, which defaults to 0.01; If `s.pct` is
#'     less than or equal to zero, or greater than 1: 0.01 is used instead.)
#'
#' @param mtry Number of variables to possibly split at in each node.
#'   Default is the (rounded down) number of variables to the power
#'   of three quarters (Ishwaran, 2011).
#'   Alternatively, a single argument function returning an integer,
#'   given the number of independent variables.
#'
#' @param type The type of random forest to create with ranger.
#'   One of `"regression"` (Default), `"classification"` or `"survival"`.
#'
#' @param status If `type = "regression"`: Survival forest status variable.
#'   Use 1 for event and 0 for censoring. Length must match `y`.
#'
#' @param min.node.size Minimal node size to split at. (Default: 1)
#'
#' @param permutate Enable to permutate `x` for [MutualForestImpact()] (Default: FALSE).
#'
#' @param seed RNG seed. It is strongly recommended that you set this value.
#'
#' @param num.threads Number of threads to parallelize with. (Default: 1)
#'
#' @returns A RandomForestSurrogates S3 object.
#'   * `trees`: List of all trees with surrogate analysis. (Class: `SurrogateTrees`, `LayerTrees`, `RangerTrees`)
#'   * `ranger`: The [ranger::ranger] model used to obtain the trees.
#'   * `s`: The number of surrogates investigated.
#'
#' @keywords prep
#' @export
RandomForestSurrogates <- function(
    x = NULL, y = NULL,
    s.pct = 0.01,
    s = ceiling(ncol(x) * ifelse(s.pct > 0 && s.pct <= 1, s.pct, 0.01)),
    mtry = c("^3/4", "sqrt", "0.5"),
    type = c("regression", "classification", "survival"),
    status = NULL,
    num.trees = 500,
    num.threads = 1,
    min.node.size = 1,
    permutate = FALSE,
    seed = NULL,
    ...) {
  if (length(y) != nrow(x)) {
    stop(paste0("Different numbers of response variables and observations.\nFound: nrow(x) = ", nrow(x), ", length(y) = ", length(y), ", expected them to be equal."))
  }
  if (any(is.na(x))) {
    stop("`x` contains missing values.")
  }
  if (is.null(seed)) {
    warning("`seed` was not set. Your results may not be reproducible.")
  }

  if (permutate) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    x <- permutate_feature_values(x)
  }

  mtry_match_args <- c("^3/4", "sqrt", "0.5")

  if (is.null(mtry) || is.character(mtry)) {
    mtry <- match.arg(mtry, mtry_match_args)
    mtry <- switch(mtry,
      `^3/4` = function(nvar) floor((nvar)^(3 / 4)),
      `sqrt` = function(nvar) floor(sqrt(nvar)),
      `0.5` = function(nvar) floor(nvar * 0.5)
    )
  }
  if (!is.numeric(mtry) && !is.function(mtry)) {
    stop(paste0("`mtry` must be one of ", paste(paste0("\"", mtry_match_args, "\""), collapse = ", "), ", a numeric or a function."))
  }

  nvar <- ncol(x)
  if (s > (nvar - 1)) {
    warning(paste0("`s` was set to the maximum number that is reasonable (set to ", nvar - 1, ", was ", s, ")."))
    s <- nvar - 1
  }

  type_match_args <- c("regression", "classification", "survival")
  type <- match.arg(type, type_match_args)

  if (type == "classification") {
    y <- as.factor(y)
    if (length(levels(y)) > 15) {
      warning(paste0("Found ", length(levels(y)), " levels in `y`. Is `type = \"classification\"` the right choice?"))
    }
  }

  if (type == "regression" && inherits(y, "factor")) {
    stop("`y` must not be a factor with `type = \"regression\"`.")
  }
  if (any(c("y", "status") %in% colnames(x))) {
    stop("`x` must not contain columns named `y` or `status`.")
  }

  if (type == "survival") {
    if (length(y) != length(status)) {
      stop(paste0("Different numbers of response variables and status variables.\nFound: length(status) = ", length(status), ", length(y) = ", length(y), ", expected them to be equal."))
    }
    if (!all(status %in% c(0, 1))) {
      stop("`status` must contain only 1 or 0. Use 1 for event and 0 for censoring.")
    }
    data <- data.frame(x, y, status)

    RF <- ranger::ranger(
      data = data,
      dependent.variable.name = "y",
      status.variable.name = "status",
      mtry = mtry,
      keep.inbag = TRUE,
      respect.unordered.factors = "partition",
      num.trees = num.trees,
      num.threads = num.threads,
      min.node.size = min.node.size,
      ...
    )
  } else if (type == "classification" || type == "regression") {
    data <- data.frame(x, y)

    RF <- ranger::ranger(
      data = data,
      dependent.variable.name = "y",
      mtry = mtry,
      keep.inbag = TRUE,
      respect.unordered.factors = "partition",
      classification = type == "classification",
      num.trees = num.trees,
      num.threads = num.threads,
      min.node.size = min.node.size,
      ...
    )
  } else {
    stop(paste0("`type` must be one of ", paste(paste0("\"", type_match_args, "\""), collapse = ", "), "."))
  }

  trees <- getSurrogateTrees(
    RF = RF,
    s = s,
    x = x,
    num.threads = num.threads
  )

  result <- list(
    trees = trees,
    ranger = RF,
    s = s
  )
  class(result) <- "RandomForestSurrogates"

  return(result)
}

#' @returns A `SurrogateTrees`, `LayerTrees`, `RangerTrees` object.
#'
#' @keywords internal
getSurrogateTrees <- function(RF, s, x, num.threads = parallel::detectCores()) {
  if (!inherits(RF, "ranger")) {
    stop("`RF` must be a `ranger` object.")
  }

  addSurrogates(
    RF = RF,
    trees = getTreeranger(
      RF = RF,
      add_layer = TRUE,
      num.threads = num.threads
    ),
    s = s,
    Xdata = x,
    num.threads = num.threads
  )
}

#' @keywords internal
permutate_feature_values <- function(x) {
  perm_names <- paste(colnames(x), "perm", sep = "_")
  x <- data.frame(lapply(1:ncol(x), permute.variable, x = x))
  colnames(x) <- perm_names

  return(x)
}
