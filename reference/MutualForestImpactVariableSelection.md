# Variable selection for MutualForestImpact.

Variable selection for MutualForestImpact.

## Usage

``` r
MutualForestImpactVariableSelection(
  MFI,
  variables,
  candidates,
  p.threshold = 0.01,
  method = c("Janitza", "Permutation")
)
```

## Arguments

- MFI:

  \[MutualForestImpact()\] object.

- variables:

  Vector of variable names for \*\*which related variables should be
  searched\*\*.

- candidates:

  Vector of variable names that \*\*are candidates to be related to the
  variables\*\*.

- p.threshold:

  (Default = 0.01) P-value threshold

- method:

  The method to use. One of: \`"Janitza"\` or \`"Permutation"\`.

## Value

A list: \* \`method\`: The method used. \* \`selected\`: A list of
vectors containing selected candidates for each investigated variable.
\* \`p.values\`: A list of numeric vectors containing p-values for each
candidate's relation to each investigated variable.
