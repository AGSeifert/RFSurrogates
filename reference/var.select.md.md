# Variable selection with Minimal Depth (MD)

This function executes MD applying
[ranger](http://imbs-hl.github.io/ranger/reference/ranger.md) for random
forests generation and is a reimplementation of var.select from
randomForestSRC package.

## Usage

``` r
var.select.md(
  x = NULL,
  y = NULL,
  num.trees = 500,
  type = "regression",
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
  samples in rows. (Note: missing values are not allowed)

- y:

  vector with values of phenotype variable (Note: will be converted to
  factor if classification mode is used). For survival forests this is
  the time variable.

- num.trees:

  Number of trees. Default is 500.

- type:

  Mode of prediction ("regression","classification" or "survival").
  Default is regression.

- mtry:

  Number of variables to possibly split at in each node. Default is no.
  of variables^(3/4) as recommended by Ishwaran.

- min.node.size:

  Minimal node size. Default is 1.

- num.threads:

  number of threads used for parallel execution. Default is number of
  CPUs available.

- status:

  status variable, only applicable to survival data. Use 1 for event and
  0 for censoring.

- save.ranger:

  Set TRUE if ranger object should be saved. Default is that ranger
  object is not saved (FALSE).

- create.forest:

  Default: TRUE if \`forest\` is NULL, FALSE otherwise. Whether to
  create or use an existing forest.

- forest:

  the random forest that should be analyzed.

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

List with the following components: \* \`info\`: list with results from
\[mindep()\] function: \* \`depth\`: mean minimal depth for each
variable. \* \`selected\`: variables has been selected (1) or not (0).
\* \`threshold\`: the threshold that is used for the selection.
(deviates slightly from the original implementation) \* \`var\`: vector
of selected variables. \* \`forest\`: a list containing: \* \`trees\`:
list of trees that was created by \[getTreeranger()\], \[addLayer()\],
and \[addSurrogates()\] functions and that was used for surrogate
minimal depth variable importance. \* \`allvariables\`: all variable
names of the predictor variables that are present in x. \* \`ranger\`:
ranger object

## References

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
set.seed(42)
res <- var.select.md(
  x = SMD_example_data[, 2:ncol(SMD_example_data)],
  y = SMD_example_data[, 1], num.trees = 10, num.threads = 1
)
res$var
#>  [1] "X2"      "X3"      "X4"      "X5"      "X6"      "X8"      "cp1_4"  
#>  [8] "cp2_1"   "cp2_3"   "cp2_5"   "cp2_10"  "cp3_2"   "cp3_3"   "cp3_4"  
#> [15] "cp3_5"   "cp3_6"   "cp7_7"   "cp7_10"  "cp8_1"   "cp8_7"   "cp8_10" 
#> [22] "cp9_4"   "cp9_7"   "cp9_9"   "cp9_10"  "cgn_3"   "cgn_4"   "cgn_6"  
#> [29] "cgn_15"  "cgn_16"  "cgn_17"  "cgn_20"  "cgn_24"  "cgn_35"  "cgn_43" 
#> [36] "cgn_44"  "cgn_47"  "cgn_48"  "cgn_49"  "cgn_51"  "cgn_53"  "cgn_55" 
#> [43] "cgn_58"  "cgn_59"  "cgn_63"  "cgn_68"  "cgn_69"  "cgn_72"  "cgn_78" 
#> [50] "cgn_79"  "cgn_81"  "cgn_82"  "cgn_91"  "cgn_93"  "cgn_94"  "cgn_95" 
#> [57] "cgn_99"  "cgn_101" "cgn_107" "cgn_108" "cgn_110" "cgn_112" "cgn_113"
#> [64] "cgn_115" "cgn_116" "cgn_117" "cgn_120" "cgn_122" "cgn_125" "cgn_126"
#> [71] "cgn_128" "cgn_131"
# }
```
