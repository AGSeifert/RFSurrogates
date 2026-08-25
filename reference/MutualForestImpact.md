# Mutual Forest Impact (Corrected Mean Adjusted Agreement).

It is recommended to use the simpler \[MFI()\] method.

## Usage

``` r
MutualForestImpact(REL, PERM, variables, candidates)
```

## Arguments

- REL:

  A \[MeanAdjustedAgreement()\] object.

- PERM:

  A permutated \[MeanAdjustedAgreement()\] object.

- variables:

  Vector of variable names for \*\*which related variables should be
  searched\*\*.

- candidates:

  Vector of variable names that \*\*are candidates to be related to the
  variables\*\*.

## Value

A \`MutualForestImpact\` list object. \* \`REL\`: The
\[MeanAdjustedAgreement()\] object. \* \`PERM\`: The permutated
\[MeanAdjustedAgreement()\] object. \* \`relations\`: Matrix of
determined relations (rows: investigated variables, columns: candidate
variables).

## See also

\[MFI()\]
