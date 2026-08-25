# Mutual Forest Impact shortcut function (recommended).

This method corrects the mean adjusted agreement by a permutation
approach and generates the relation parameter mutual forest impact.
Subsequently p-values are determined and related variables are selected.

## Usage

``` r
MFI(variables, candidates, num.threads = 1, ...)
```

## Arguments

- variables:

  Vector of variable names for \*\*which related variables should be
  searched\*\*.

- candidates:

  Vector of variable names that \*\*are candidates to be related to the
  variables\*\*.

- num.threads:

  Number of threads to parallelize with. (Default: 1)

- ...:

  Arguments passed on to
  [`RandomForestSurrogates`](https://agseifert.github.io/RFSurrogates/reference/RandomForestSurrogates.md)

  `x,y`

  :   Predictor data and dependent variables.

  `s.pct,s`

  :   Number of surrogate splits. This can be defined either by setting
      \`s.pct\` to a number between 0 and 1, or providing an exact value
      for \`s\`. - \`s.pct\`: Percentage of variables to use for \`s\`.
      (Default: 0.01) - \`s\`: Number of surrogate splits. (Default:
      Number of variables multiplied by \`s.pct\`, which defaults to
      0.01; If \`s.pct\` is less than or equal to zero, or greater than
      1: 0.01 is used instead.)

  `mtry`

  :   Number of variables to possibly split at in each node. Default is
      the (rounded down) number of variables to the power of three
      quarters (Ishwaran, 2011). Alternatively, a single argument
      function returning an integer, given the number of independent
      variables.

  `type`

  :   The type of random forest to create with ranger. One of
      \`"regression"\` (Default), \`"classification"\` or
      \`"survival"\`.

  `status`

  :   If \`type = "regression"\`: Survival forest status variable. Use 1
      for event and 0 for censoring. Length must match \`y\`.

  `min.node.size`

  :   Minimal node size to split at. (Default: 1)

  `permutate`

  :   Enable to permutate \`x\` for \[MutualForestImpact()\] (Default:
      FALSE).

  `seed`

  :   RNG seed. It is strongly recommended that you set this value.

  `preschedule.threads`

  :   (Default: TRUE) Passed as \`mc.preschedule\` to
      \[parallel::mclapply()\] in \[addSurrogates()\].

  `num.trees`

  :   Number of trees.

## Value

A \[MutualForestImpact()\] list object. \* \`REL\`: The
\[MeanAdjustedAgreement()\] object. \* \`PERM\`: The permutated
\[MeanAdjustedAgreement()\] object. \* \`relations\`: Matrix of
determined relations (rows: investigated variables, columns: candidate
variables).

## See also

\[MutualForestImpact()\]

## Examples

``` r
# \donttest{
data("SMD_example_data")
mfi <- MFI(
  x = SMD_example_data[, -1], y = SMD_example_data[, 1],
  s = 10, num.trees = 50, num.threads = 1,
  variables = c("X7", "X1"), candidates = colnames(SMD_example_data)[2:101]
)
#> Warning: `seed` was not set. Your results may not be reproducible.
#> Warning: `seed` was not set. Your results may not be reproducible.
# }
```
