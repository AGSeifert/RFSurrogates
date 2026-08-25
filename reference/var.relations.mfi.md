# Investigate variable relations of a specific variable with mutual forest impact (corrected mean adjusted agreement).

This function corrects the mean adjusted agreement by a permutation
approach and generates the relation parameter mutual forest impact.
Subsequently p-values are determined and related variables are selected.

## Usage

``` r
var.relations.mfi(
  x = NULL,
  y = NULL,
  num.trees = 500,
  type = "regression",
  s = NULL,
  mtry = NULL,
  min.node.size = 1,
  num.threads = NULL,
  status = NULL,
  save.ranger = FALSE,
  create.forest = is.null(forest),
  forest = NULL,
  save.memory = FALSE,
  case.weights = NULL,
  variables,
  candidates,
  p.t = 0.01,
  select.rel = TRUE,
  method = "janitza"
)
```

## Arguments

- x:

  data.frame of predictor variables with variables in columns and
  samples in rows (Note: missing values are not allowed)

- y:

  vector with values of phenotype variable (Note: will be converted to
  factor if classification mode is used). For survival forests this is
  the time variable.

- num.trees:

  number of trees. Default is 500.

- type:

  mode of prediction ("regression", "classification" or "survival").
  Default is regression.

- s:

  predefined number of surrogate splits (it may happen that the actual
  number of surrogate splits differs in individual nodes). Default is 1
  percent of no. of variables.

- mtry:

  number of variables to possibly split at in each node. Default is no.
  of variables^(3/4) ("^3/4") as recommended by (Ishwaran 2011). Also
  possible is "sqrt" and "0.5" to use the square root or half of the no.
  of variables.

- min.node.size:

  minimal node size. Default is 1.

- num.threads:

  number of threads used for determination of relations. Default is
  number of CPUs available.

- status:

  status variable, only applicable to survival data. Use 1 for event and
  0 for censoring.

- save.ranger:

  set TRUE if ranger object should be saved. Default is that ranger
  object is not saved (FALSE).

- create.forest:

  Default: TRUE if \`forest\` is NULL, FALSE otherwise. Whether to
  create or use an existing forest.

- forest:

  the random forest that should be analyzed

- save.memory:

  Use memory saving (but slower) splitting mode. No effect for survival
  and GWAS data. Warning: This option slows down the tree growing, use
  only if you encounter memory problems. (This parameter is transfered
  to ranger)

- case.weights:

  Weights for sampling of training observations. Observations with
  larger weights will be selected with higher probability in the
  bootstrap (or subsampled) samples for the trees.

- variables:

  variable names (string) for which related variables should be searched
  for (has to be contained in allvariables)

- candidates:

  vector of variable names (strings) that are candidates to be related
  to the variables (has to be contained in allvariables)

- p.t:

  p.value threshold for selection of related variables. Default is 0.01.

- select.rel:

  set False if only relations should be calculated and no related
  variables should be selected.

- method:

  Method to compute p-values. Use "janitza" for the method by Janitza et
  al. (2016) or "permutation" to utilize permuted relations.

## Value

A list containing: \* \`variables\`: the variables to which relations
are investigated. \* \`surr.res\`: a matrix with the mutual forest
impact values with variables in rows and candidates in columns. \*
\`surr.perm\`: a matrix with the mutual forest impact values of the
permuted variables with variables in rows and candidates in columns. \*
\`p.rel\`: a list with the obtained p-values for the relation analysis
of each variable. \* \`var.rel\`: a list with vectors of related
variables for each variable. \* \`ranger\`: ranger objects. \*
\`method\`: Method to compute p-values: "janitza" or "permutation". \*
\`p.t\`: p.value threshold for selection of related variables

## Examples

``` r
# \donttest{
data("SMD_example_data")
x <- SMD_example_data[, 2:ncol(SMD_example_data)]
y <- SMD_example_data[, 1]
# calculate variable relations
set.seed(42)
res <- var.relations.mfi(
  x = x,
  y = y,
  s = 10,
  num.trees = 10,
  variables = c("X1", "X7"),
  candidates = colnames(x)[1:100],
  num.threads = 1
)
#> Warning: Relations for 10 original variables were not calculated because they were never used as a primary split.
#>             Affected relations are set to 0. 
#> Warning: Relations for 12 permuted variables were not calculated because they were not used as a primary split.
#>             Affected relations are set to 0. 
res$var.rel[[1]]
#>  [1] "cp1_1"  "cp1_2"  "cp1_3"  "cp1_4"  "cp1_5"  "cp1_6"  "cp1_7"  "cp1_8" 
#>  [9] "cp1_9"  "cp1_10"
# }
```
