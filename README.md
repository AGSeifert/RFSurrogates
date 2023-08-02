# RFSurrogates

In this R package, several functions are provided for applying approaches based on random forest. Minimal depth (MD), Surrogate minimal depth (SMD) and mutual impurity reduction (MIR), which is a corrected approach of SMD, can be applied to assess the importance of variables and to select important variables. In addition, the parameters mean adjusted agreement and mutual forest impact (MFI), a corrected approach of the previous, can be applied to investigate variable relations based on surrogate variables. 

Please cite the following manuscripts if you use the package:  

[1] S. Seifert, S. Gundlach, S. Szymczak, Surrogate minimal depth as an importance measure for variables in random forests, Bioinformatics 2019, 35, 3663-3671. [[doi:10.1093/bioinformatics/btz149](https://doi.org/10.1093/bioinformatics/btz149)]

[2] publication about MFI/MIR under preparation [[arXiv Preprint](https://doi.org/10.48550/ARXIV.2304.02490)]

# Installation

```r
devtools::install_github("AGSeifert/RFSurrogates")
```

# Example Data [![DOI](https://www.fdr.uni-hamburg.de/badge/DOI/10.25592/uhhfdm.12620.svg)](https://doi.org/10.25592/uhhfdm.12620)

The package contains an example data set which consists of a single replicate of the simulation study 1 in publication [1]. Please refer to the paper and the documentation of the SMD_example_data for further details on the simulation scenario. The R script for the simulation is published [here](https://doi.org/10.25592/uhhfdm.12620).

# Usage

First the package and the example data are loaded:

```r
library(RFSurrogates)
data("SMD_example_data")
dim(SMD_example_data)
# [1] 100 201

head(SMD_example_data[, 1:5])
#            y          X1         X2         X3           X4
# 1  1.8222421 -0.02768266 -1.1019154  2.2659401  0.008021516
# 2 -1.0401813  0.73258486 -0.4107975  0.7587792 -0.718752746
# 3  2.7139607 -0.05399936  1.1851261  0.9743160 -2.563176970
# 4 -0.7081372 -0.84838121 -0.8975802  0.5247899  1.180683275
# 5 -1.0264429 -0.42219003  0.5439467 -0.1626504  0.682333020
# 6  3.1871209  0.91722598  0.1974106  0.9571554  0.351634641
```

The data set has 100 observations in the rows and the columns contain the continuous outcome variable y and 200 continuous predictor variables in the columns.

## Minimal Depth (MD)

First, we perform variable selection based on minimal depth using 1000 trees in the random forest. To make the analysis reproducible we set the seed first.

```r
data("SMD_example_data")
md <- MinimalDepth(RandomForestSurrogates(
    x = SMD_example_data[, -1], y = SMD_example_data[, 1],
    num.trees = 1000, s = 0, seed = 42
))

md$selected
#  [1] "X2"     "X3"     "X4"     "X5"     "X6"     "cp1_2" 
#  [7] "cp1_8"  "cp2_1"  "cp2_6"  "cp2_7"  "cp3_4"  "cp3_6" 
# [13] "cp8_5"  "cgn_72" "cgn_81"
```

The selected variables are stored in `md$var`. In this analysis the relevant basic variables `"X2"` to `"X6"`, as well as the relevant variables `"cp1_2"`, `"cp1_8"`, `"cp2_1"`, `"cp2_6"`, `"cp2_7"`, `"cp3_4"`, `"cp3_6"`, `"cp8_5"`, and the non-relevant variables `"cgn_72"`, and `"cgn_81"` are selected.

The MD values for each predictor variable and the threshold to select variables can be extracted as follows:

```r
head(md$depth)
#    X1    X2    X3    X4    X5    X6 
# 9.641 7.629 5.964 6.328 6.655 6.613

md$threshold
# [1] 9.229029
```

We can see that variables `"X2"`, …, `"X6"` have MD values smaller than the threshold in contrast to `"X1"`.

## Surrogate Minimal Depth (SMD)

Now we would like to analyze the example data with surrogate minimal depth which works similarly. However, we need to specify an additional parameter `s`, the number of surrogate variables that should be considered. In this analysis we use `s = 10`. Based on our simulation studies we recommend to set this parameter to approximately 1% of the predictor variables in larger data sets.

Variable selection with `SurrogateMinimalDepth()` is conducted:

```r
smd <- SurrogateMinimalDepth(RandomForestSurrogates(
    x = SMD_example_data[, -1], y = SMD_example_data[, 1],
    num.trees = 1000, s = 10, seed = 42
))

smd$selected
#  [1] "X1"     "X2"     "X3"     "X4"     "X5"     "X6"    
#  [7] "X8"     "cp1_1"  "cp1_2"  "cp1_3"  "cp1_4"  "cp1_5" 
# [13] "cp1_6"  "cp1_7"  "cp1_8"  "cp1_9"  "cp1_10" "cp2_1" 
# [19] "cp2_3"  "cp2_4"  "cp2_6"  "cp2_7"  "cp2_9"  "cp2_10"
# [25] "cp3_4" 
```

The selected variables are stored in `smd$selected`. In this analysis the relevant basic variables `X1` to `X6`, `X8`, as well as the relevant variables related variables of `cp1`, `cp2`, and `cp3_4` are selected. Compared to MD more of the relevant variables and none of the non-relevant variables are selected.

The SMD values for each predictor variable and the threshold to select variables can be extracted as follows:

```
head(smd$depth)
#    X1    X2    X3    X4    X5    X6 
# 2.050 2.035 1.922 2.118 2.109 2.042 

smd$threshold
# [1] 2.666462
```

We can see that variables `"X1"`, …, `"X6"` have SMD values smaller than the threshold.

## Variable relations based on the mean adjusted agreement of surrogate variables

Now we want to investigate the relations of variables. We would like to identify which of the first 100 predictor variables are related to `"X1"` and `"X7"`. We simulated 10 correlated predictor variables for each of these two basic variables.
One possibility to investigate variable relations is to use the results from `var.select.smd()`. Hence, first SMD is conducted like in the [previous section](#surrogate-minimal-depth-smd).

Subsequently, variable relations are analyzed with `var.relations()`. The parameter `t` can be adapted to either focus on strongly related variables only (high numbers) or to include also moderately related variables (low numbers):

```r
candidates <- colnames(SMD_example_data)[2:101]
rel <- var.relations(
  forest = res.smd$forest, 
  variables = c("X1", "X7"), candidates = candidates, 
  t = 5)

rel$var
# $X1
#  [1] "cp1_1"  "cp1_2"  "cp1_3"  "cp1_4"  "cp1_5"  "cp1_6" 
#  [7] "cp1_7"  "cp1_8"  "cp1_9"  "cp1_10"
# 
# $X7
#  [1] "cp7_1"  "cp7_2"  "cp7_3"  "cp7_4"  "cp7_5"  "cp7_6" 
#  [7] "cp7_7"  "cp7_8"  "cp7_9"  "cp7_10"
```

All of the variables that are correlated to `"X1"` are correctly identified as related to `"X1"` and all of the variables that are correlated to `"X7"` are correctly identified as related to `"X7"`. 

## Variable relations based on Mutual Forest Impact (MFI)

MFI is a corrected relation parameter calculated by the mean adjusted agreement of the variables and permuted versions of them. Related variables are selected by p-values obtained from a null distribution either determined by negative relation scores (based on the Janitza approach) or by permuted relations. 
We use the default parameters for the selection here, which is a p-values threshold of 0.01 and the Janitza approach. 

```r
set.seed(42)
rel.mfi <- var.relations.mfi(
  x = SMD_example_data[, -1], y = SMD_example_data[, 1], 
  s = 10, num.trees = 1000, variables = c("X1","X7"),
  candidates = colnames(SMD_example_data)[2:101], 
  p.t = 0.01, method = "janitza", num.threads = 1)

rel.mfi$var.rel
# $X1
#  [1] "cp1_1"  "cp1_2"  "cp1_3"  "cp1_4"  "cp1_5"  "cp1_6" 
#  [7] "cp1_7"  "cp1_8"  "cp1_9"  "cp1_10"
# 
# $X7
#  [1] "cp7_1"  "cp7_2"  "cp7_3"  "cp7_4"  "cp7_5"  "cp7_6" 
#  [7] "cp7_7"  "cp7_8"  "cp7_9"  "cp7_10"
```

Also by MFI, all of the variables that are correlated to `"X1"` are correctly identified as related to `"X1"` and all of the variables that are correlated to `"X7"` are correctly identified as related to `"X7"`. 
Also the matrix of determined relation (`surr.res`), permuted relations (`surr.perm`) and determined p-values (`p.rel`) can be extracted as follows: 

```r
MFI <- rel.mfi$surr.res
surr.perm <- rel.mfi$surr.perm
p.rel <- rel.mfi$p.rel
```

## Mutual Impurity Reduction (MIR)

Now we would like to analyze the example data with MIR, which determines the variable importance by the actual impurity reduction combined with the relations determined by MFI. Different to MD and SMD, this approach calculates p-values for the selection of important variables. For this, the null distribution is obtained in a similar way as for MFI, either by negative importance scores called the Janitza approach or by permutation. Since this example data set is comparatively small, we use the permutation approach. As a threshold for selection a value of 0.01 is applied (`p.t.sel = 0.01`).

```r
set.seed(42)
res.mir <- var.select.mir(
  x = SMD_example_data[, -1], y = SMD_example_data[, 1], 
  s = 10, num.trees = 1000, method.sel = "permutation",
  p.t.sel = 0.01, num.threads = 1)

res.mir$var
#  [1] "X1"     "X2"     "X3"     "X4"     "X5"     "X6"    
#  [7] "cp1_1"  "cp1_2"  "cp1_3"  "cp1_4"  "cp1_5"  "cp1_6" 
# [13] "cp1_7"  "cp1_8"  "cp1_9"  "cp1_10" "cp2_1"  "cp2_3" 
# [19] "cp2_4"  "cp2_6"  "cp2_7"  "cp2_10" "cp3_1"  "cp3_4" 
# [25] "cp3_5"  "cgn_72" "cgn_81"
```
The selected variables are stored in `res.mir$var`. Here, the relevant variables `"cp1_1"` to `"cp1_10"`, `"cp2_1"`, `"cp2_3"`, `"cp2_4"`, `"cp2_6"`, `"cp2_7"`, `"cp2_10"`, `"cp3_1"`, `"cp3_4"`, `"cp3_5"`, as well as the non-relevant variables `"cgn_72"` and `"cgn_81"` are selected. 

The MIR values and p-values can be extracted as follows:

```
mir <- res.mir$info$MIR

head(mir)
#       X1       X2       X3       X4       X5       X6 
# 10.68243 15.95674 27.09036 20.50233 23.16293 21.15731

pvalues <- res.mir$info$pvalue

head(pvalues)
# X1 X2 X3 X4 X5 X6 
#  0  0  0  0  0  0 
```

We can see that variables `"X1"`, …, `"X6"` have a p-value of 0 and are selected.

Since this approach is based on the actual impurity reduction combined with the relations determined by MFI, both of these can also be extracted from the results:

```
air <- res.mir$info$AIR

head(air)
#        X1        X2        X3        X4        X5        X6 
#  1.072849 13.133904 26.444900 19.155187 22.718355 20.782305 
 
res.mfi <- res.mir$info$relations
```

`res.mfi` contains the results of `var.relations.mfi()` conducted in MIR. 
