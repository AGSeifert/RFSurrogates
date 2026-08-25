# Investigate variable relations of a specific variable with mean adjusted agreement

This function uses the mean adjusted agreement to select variables that
are related to a defined variable using a threshold T. The parameter t
is used to calculate T: t=1 means that every variable with higher
probability than "by chance" is identified as "important". t=2 means the
probability has to be twice, etc. Based on the threshold a vector is
created containing the related variables.

## Usage

``` r
var.relations(
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
  t = 5,
  select.rel = TRUE
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

- t:

  variable to calculate threshold. Default is 5.

- select.rel:

  set False if only relations should be calculated and no related
  variables should be selected.

## Value

A list containing: \* \`variables\`: the variables to which relations
are investigated. \* \`surr.res\`: a matrix with mean adjusted agreement
values with variables in rows and candidates in columns. \*
\`threshold\`: the threshold used to select related variables. \*
\`var\`: a list with one vector for each variable containing related
variables. \* \`ranger\`: ranger object.

## Examples

``` r
# \donttest{
data("SMD_example_data")
x <- SMD_example_data[, 2:ncol(SMD_example_data)]
y <- SMD_example_data[, 1]
set.seed(42)
res <- var.relations(
  x = x,
  y = y,
  s = 10,
  num.trees = 10,
  variables = c("X1", "X7"),
  candidates = colnames(x)[1:100],
  t = 5,
  num.threads = 1
)
res$var
#> $X1
#>  [1] "cp1_1"  "cp1_2"  "cp1_3"  "cp1_4"  "cp1_5"  "cp1_6"  "cp1_7"  "cp1_8" 
#>  [9] "cp1_9"  "cp1_10" "cp8_1" 
#> 
#> $X7
#>  [1] "cp7_1"  "cp7_2"  "cp7_3"  "cp7_4"  "cp7_5"  "cp7_6"  "cp7_7"  "cp7_8" 
#>  [9] "cp7_9"  "cp7_10"
#> 
# }
```
