# Janitza method for MFI.

Janitza method for MFI.

## Usage

``` r
MFI_VarSel_Janitza(MFI, variables, candidates, p.threshold = 0.01)
```

## Arguments

- MFI:

  \[\`MutualForestImpact()\`\] object.

- variables:

  Vector of variable names for \*\*which related variables should be
  searched\*\*.

- candidates:

  Vector of variable names that \*\*are candidates to be related to the
  variables\*\*.

- p.threshold:

  (Default = 0.01) P-value threshold

## Value

A list: \* \`selected\`: A list of vectors containing selected
candidates for each investigated variable. \* \`p.values\`: A list of
numeric vectors containing p-values for each candidate's relation to
each investigated variable.
