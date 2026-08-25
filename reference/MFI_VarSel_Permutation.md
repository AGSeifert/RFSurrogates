# Permutation method for MFI.

Permutation method for MFI.

## Usage

``` r
MFI_VarSel_Permutation(MFI, candidates, variables, p.threshold = 0.01)
```

## Arguments

- MFI:

  \[\`MutualForestImpact()\`\] object.

- candidates:

  Vector of variable names that \*\*are candidates to be related to the
  variables\*\*.

- variables:

  Vector of variable names for \*\*which related variables should be
  searched\*\*.

- p.threshold:

  (Default = 0.01) P-value threshold

## Value

A list: \* \`selected\`: A list of vectors containing selected
candidates for each investigated variable. \* \`p.values\`: A list of
numeric vectors containing p-values for each candidate's relation to
each investigated variable.
