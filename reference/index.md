# Package index

## Random Forest Surrogates

The basic object used in most further analysis functions.

- [`RandomForestSurrogates()`](https://agseifert.github.io/RFSurrogates/reference/RandomForestSurrogates.md)
  : Create a random forest with surrogates.

## Surrogate Minimal Depth

Surrogate Minimal Depth (SMD) enables the identification of multiple
features from the same metabolites and reveals meaningful biological
relations, proving its high potential for the comprehensive utilization
of high-dimensional metabolomics data. Reference:
[10.3390/metabo12010005](https://doi.org/10.3390/metabo12010005)

- [`SurrogateMinimalDepth()`](https://agseifert.github.io/RFSurrogates/reference/SurrogateMinimalDepth.md)
  : Variable selection with Surrogate Minimal Depth (SMD)
- [`MinimalDepth()`](https://agseifert.github.io/RFSurrogates/reference/MinimalDepth.md)
  : Variable selection with Minimal Depth (MD).
- [`MeanAdjustedAgreement()`](https://agseifert.github.io/RFSurrogates/reference/MeanAdjustedAgreement.md)
  : Investigate variable relations of a specific variable with mean
  adjusted agreement

## Mutual Impact of Features

Mutual forest impact (MFI) is a relation parameter that evaluates the
mutual association of the featurs to the outcome and, hence, goes beyond
the analysis of correlation coefficients. Mutual impurity reduction
(MIR) is an importance measure that combines this relation parameter
with the importance of the individual features. Reference:
[10.48550/ARXIV.2304.02490](https://doi.org/10.48550/ARXIV.2304.02490)

- [`MutualForestImpact()`](https://agseifert.github.io/RFSurrogates/reference/MutualForestImpact.md)
  : Mutual Forest Impact (Corrected Mean Adjusted Agreement).
- [`MFI()`](https://agseifert.github.io/RFSurrogates/reference/MFI.md) :
  Mutual Forest Impact shortcut function (recommended).
- [`MutualImpurityReduction()`](https://agseifert.github.io/RFSurrogates/reference/MutualImpurityReduction.md)
  : Mutual Impurity Reduction (MIR)
- [`MutualForestImpactVariableSelection()`](https://agseifert.github.io/RFSurrogates/reference/MutualForestImpactVariableSelection.md)
  : Variable selection for MutualForestImpact.
- [`MutualImpurityReductionVariableSelection()`](https://agseifert.github.io/RFSurrogates/reference/MutualImpurityReductionVariableSelection.md)
  : Variable selection for Mutual Impurity Reduction.

## Data

Example data sets shipped with this package.

- [`SMD_example_data`](https://agseifert.github.io/RFSurrogates/reference/SMD_example_data.md)
  : Example data set for the package SurrogateMinimalDepth

## Version 0.3.x functions

The original functions from prior versions remain available for backward
compatability.

- [`var.relations()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.md)
  : Investigate variable relations of a specific variable with mean
  adjusted agreement
- [`var.relations.mfi()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.mfi.md)
  : Investigate variable relations of a specific variable with mutual
  forest impact (corrected mean adjusted agreement).
- [`var.select.md()`](https://agseifert.github.io/RFSurrogates/reference/var.select.md.md)
  : Variable selection with Minimal Depth (MD)
- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md)
  : Variable selection with Surrogate Minimal Depth (SMD) (MAIN
  FUNCTION)
- [`var.select.mir()`](https://agseifert.github.io/RFSurrogates/reference/var.select.mir.md)
  : Variable selection with mutual impurity reduction (MIR).

## Other functions

Additional functions published as part of version 0.3.x and earlier
versions. These will remain available for backward compatability.

- [`addLayer()`](https://agseifert.github.io/RFSurrogates/reference/addLayer.md)
  : Add layer information to a forest that was created by getTreeranger
- [`addSurrogates()`](https://agseifert.github.io/RFSurrogates/reference/addSurrogates.md)
  : Add surrogate information to a tree list.
- [`count.surrogates()`](https://agseifert.github.io/RFSurrogates/reference/count.surrogates.md)
  : Count surrogate variables
- [`getTreeranger()`](https://agseifert.github.io/RFSurrogates/reference/getTreeranger.md)
  : Get a list of structured trees from a ranger object.
- [`meanAdjAgree()`](https://agseifert.github.io/RFSurrogates/reference/meanAdjAgree.md)
  : Calculate mean adjusted agreement to investigate variables relations
- [`mindep()`](https://agseifert.github.io/RFSurrogates/reference/mindep.md)
  : Execute minimal depth variable importance
- [`reduce.surrogates()`](https://agseifert.github.io/RFSurrogates/reference/reduce.surrogates.md)
  : Reduce surrogate variables in a random forest.
- [`surrmindep()`](https://agseifert.github.io/RFSurrogates/reference/surrmindep.md)
  : Execute surrogate minimal depth variable importance
