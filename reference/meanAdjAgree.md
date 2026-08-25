# Calculate mean adjusted agreement to investigate variables relations

This is the main function of var.relations function.

## Usage

``` r
meanAdjAgree(
  trees,
  variables,
  allvariables,
  candidates,
  t,
  s.a,
  select.var,
  num.threads = NULL,
  round_digits = 2
)
```

## Arguments

- trees:

  list of trees created by \[getTreeranger()\], \[addLayer()\] and
  \[addSurrogates()\].

- variables:

  vector of variable names.

- allvariables:

  vector of all variable names (strings)

- candidates:

  vector of variable names (strings) that are candidates to be related
  to the variables (has to be contained in allvariables)

- t:

  variable to calculate threshold. Used if \`select.var = TRUE\`.

- s.a:

  average number of surrogate variables (ideally calculated by
  count.surrogates function).

- select.var:

  set False if only relations should be calculated and no related
  variables should be selected.

- num.threads:

  number of threads used for parallel execution. Default is number of
  CPUs available.

- round_digits:

  (Default: 2) Round mean adjusted agreement to this many digits in
  \[mean.index\].

## Value

A list containing: \* \`variables\`: the variables to which relations
are investigated \* \`surr.res\`: matrix with mean adjusted agreement
values and variables investigated in rows and candidate variables in
columns \* \`threshold\`: the threshold used to create surr.var from
surr.res \* \`surr.var\`: binary matrix showing if the variables are
related (1) or non-related (0) with variables in rows and candidates in
columns.
