# Variable selection for Mutual Impurity Reduction.

Variable selection for Mutual Impurity Reduction.

## Usage

``` r
MutualImpurityReductionVariableSelection(
  MIR,
  p.threshold = 0.01,
  method = c("Janitza", "Permutation"),
  permutation.num = 100,
  permutation.MeanAdjustedAgreement = NULL
)
```

## Arguments

- MIR:

  \[MutualImpurityReduction()\] object.

- p.threshold:

  (Default = 0.01) P-value threshold

- method:

  The method to use. One of: \`"Janitza"\` or \`"Permutation"\`.

- permutation.num:

  If method is \`"Permutation"\`: Number of AIR permutations to
  determine p-value. (Default: 100)

- permutation.MeanAdjustedAgreement:

  If method is \`"Permutation"\` and \`MIR\` used a
  \[MeanAdjustedAgreement()\] object: A \[MeanAdjustedAgreement()\]
  created with a \`permutate = TRUE\` \[RandomForestSurrogates()\]
  object.

## Value

A list: \* \`method\`: The method used. \* \`selected\`: A list of
vectors containing selected candidates for each investigated variable.
\* \`p.values\`: A list of numeric vectors containing p-values for each
candidate's relation to each investigated variable.
