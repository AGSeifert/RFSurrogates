# Variable selection with Surrogate Minimal Depth (SMD) (MAIN FUNCTION)

This function executes SMD applying
[ranger](http://imbs-hl.github.io/ranger/reference/ranger.md) for random
forests generation and a modified version of
[rpart](https://rdrr.io/pkg/rpart/man/rpart.html) to find surrogate
variables.

## Usage

``` r
var.select.smd(
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
  case.weights = NULL
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

  number of threads used for parallel execution. Default is number of
  CPUs available.

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

## Value

list with the following components: \* \`info\`: list with results from
surrmindep function: \* \`depth\`: mean surrogate minimal depth for each
variable. \* \`selected\`: variables has been selected (1) or not (0).
\* \`threshold\`: the threshold that is used for the selection. \*
\`var\`: vector of selected variables. \* \`s\`: list with the results
of count.surrogate function: \* \`s.a\`: total average number of
surrogate variables. \* \`s.l\`: average number of surrogate variables
in the respective layers. \* \`forest\`: a list containing: \*
\`trees\`: list of trees that was created by getTreeranger, addLayer,
and addSurrogates functions and that was used for surrogate minimal
depth variable importance. \* \`allvariables\`: all variable names of
the predictor variables that are present in x \* \`ranger\`: ranger
object.

## References

\* Seifert, S. et al. (2019) Surrogate minimal depth as an importance
measure for variables in random forests. Bioinformatics, 35, 3663–3671.
\<https://academic.oup.com/bioinformatics/article/35/19/3663/5368013\>
\* Ishwaran, H. et al. (2011) Random survival forests for
high-dimensional data. Stat Anal Data Min, 4, 115–132.
\<https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.10103\> \*
Ishwaran, H. et al. (2010) High-Dimensional Variable Selection for
Survival Data. J. Am. Stat. Assoc., 105, 205–217.
\<http://www.ccs.miami.edu/~hishwaran/papers/IKGML.JASA.2010.pdf\>

## Examples

``` r
# \donttest{
data("SMD_example_data")
# select variables (usually more trees are needed)
set.seed(42)
res <- var.select.smd(
  x = SMD_example_data[, 2:ncol(SMD_example_data)],
  y = SMD_example_data[, 1], s = 10, num.trees = 10, num.threads = 1
)
res$var
#>  [1] "X2"      "X3"      "X4"      "X5"      "X6"      "X8"      "cp1_1"  
#>  [8] "cp1_4"   "cp1_5"   "cp1_6"   "cp1_8"   "cp1_9"   "cp2_2"   "cp2_3"  
#> [15] "cp2_4"   "cp2_5"   "cp2_10"  "cp3_3"   "cp3_4"   "cp3_5"   "cp3_6"  
#> [22] "cp7_1"   "cp8_2"   "cp8_5"   "cgn_4"   "cgn_19"  "cgn_47"  "cgn_49" 
#> [29] "cgn_62"  "cgn_75"  "cgn_121"
# }
```
