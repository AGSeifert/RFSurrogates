# Create a random forest with surrogates.

The object created by this functions serves as the basis of most further
analysis, such as \[SurrogateMinimalDepth()\] and
\[MeanAdjustedAgreement()\].

## Usage

``` r
RandomForestSurrogates(
  x = NULL,
  y = NULL,
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
  preschedule.threads = TRUE,
  ...
)
```

## Arguments

- x, y:

  Predictor data and dependent variables.

- s.pct, s:

  Number of surrogate splits. This can be defined either by setting
  \`s.pct\` to a number between 0 and 1, or providing an exact value for
  \`s\`. - \`s.pct\`: Percentage of variables to use for \`s\`.
  (Default: 0.01) - \`s\`: Number of surrogate splits. (Default: Number
  of variables multiplied by \`s.pct\`, which defaults to 0.01; If
  \`s.pct\` is less than or equal to zero, or greater than 1: 0.01 is
  used instead.)

- mtry:

  Number of variables to possibly split at in each node. Default is the
  (rounded down) number of variables to the power of three quarters
  (Ishwaran, 2011). Alternatively, a single argument function returning
  an integer, given the number of independent variables.

- type:

  The type of random forest to create with ranger. One of
  \`"regression"\` (Default), \`"classification"\` or \`"survival"\`.

- status:

  If \`type = "regression"\`: Survival forest status variable. Use 1 for
  event and 0 for censoring. Length must match \`y\`.

- num.trees:

  Number of trees.

- num.threads:

  Number of threads to parallelize with. (Default: 1)

- min.node.size:

  Minimal node size to split at. (Default: 1)

- permutate:

  Enable to permutate \`x\` for \[MutualForestImpact()\] (Default:
  FALSE).

- seed:

  RNG seed. It is strongly recommended that you set this value.

- preschedule.threads:

  (Default: TRUE) Passed as \`mc.preschedule\` to
  \[parallel::mclapply()\] in \[addSurrogates()\].

- ...:

  Arguments passed on to
  [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md)

  `importance`

  :   Variable importance mode, one of 'none', 'impurity',
      'impurity_corrected', 'permutation'. The 'impurity' measure is the
      Gini index for classification, the variance of the responses for
      regression and the sum of test statistics (see `splitrule`) for
      survival.

  `min.bucket`

  :   Minimal terminal node size. No nodes smaller than this value can
      occur. Default 3 for survival and 1 for all other tree types. For
      classification, this can be a vector of class-specific values.

  `max.depth`

  :   Maximal tree depth. A value of NULL or 0 (the default) corresponds
      to unlimited depth, 1 to tree stumps (1 split per tree).

  `replace`

  :   Sample with replacement.

  `sample.fraction`

  :   Fraction of observations to sample. Default is 1 for sampling with
      replacement and 0.632 for sampling without replacement. For
      classification, this can be a vector of class-specific values.

  `case.weights`

  :   Weights for sampling of training observations. Observations with
      larger weights will be selected with higher probability in the
      bootstrap (or subsampled) samples for the trees.

  `class.weights`

  :   Weights for the outcome classes (in order of the factor levels) in
      the splitting rule (cost sensitive learning). Classification and
      probability prediction only. For classification the weights are
      also applied in the majority vote in terminal nodes.

  `splitrule`

  :   Splitting rule. For classification and probability estimation
      "gini", "extratrees" or "hellinger" with default "gini". For
      regression "variance", "extratrees", "maxstat", "beta" or
      "poisson" with default "variance". For survival "logrank",
      "extratrees", "C" or "maxstat" with default "logrank".

  `num.random.splits`

  :   For "extratrees" splitrule.: Number of random splits to consider
      for each candidate splitting variable.

  `alpha`

  :   For "maxstat" splitrule: Significance threshold to allow
      splitting.

  `minprop`

  :   For "maxstat" splitrule: Lower quantile of covariate distribution
      to be considered for splitting.

  `split.select.weights`

  :   Numeric vector with weights between 0 and 1, used to calculate the
      probability to select variables for splitting. Alternatively, a
      list of size num.trees, containing split select weight vectors for
      each tree can be used.

  `always.split.variables`

  :   Character vector with variable names to be always selected in
      addition to the `mtry` variables tried for splitting.

  `scale.permutation.importance`

  :   Scale permutation importance by standard error as in (Breiman
      2001). Only applicable if permutation variable importance mode
      selected.

  `local.importance`

  :   Calculate and return local importance values as in (Breiman 2001).
      Only applicable if `importance` is set to 'permutation'.

  `regularization.factor`

  :   Regularization factor (gain penalization), either a vector of
      length p or one value for all variables.

  `regularization.usedepth`

  :   Consider the depth in regularization.

  `inbag`

  :   Manually set observations per tree. List of size num.trees,
      containing inbag counts for each observation. Can be used for
      stratified sampling.

  `holdout`

  :   Hold-out mode. Hold-out all samples with case weight 0 and use
      these for variable importance and prediction error.

  `quantreg`

  :   Prepare quantile prediction as in quantile regression forests
      (Meinshausen 2006). Regression only. Set `keep.inbag = TRUE` to
      prepare out-of-bag quantile prediction.

  `oob.error`

  :   Compute OOB prediction error. Set to `FALSE` to save computation
      time, e.g. for large survival forests.

  `save.memory`

  :   Use memory saving (but slower) splitting mode. No effect for
      survival and GWAS data. Warning: This option slows down the tree
      growing, use only if you encounter memory problems.

  `verbose`

  :   Show computation status and estimated runtime.

## Value

A RandomForestSurrogates S3 object. \* \`trees\`: List of all trees with
surrogate analysis. (Class: \`SurrogateTrees\`, \`LayerTrees\`,
\`RangerTrees\`) \* \`ranger\`: The \[ranger::ranger\] model used to
obtain the trees. \* \`s\`: The number of surrogates investigated.
