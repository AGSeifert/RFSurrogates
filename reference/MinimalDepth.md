# Variable selection with Minimal Depth (MD).

This is identical to \[SurrogateMinimalDepth()\], except it requires
\`s\` to be 0.

## Usage

``` r
MinimalDepth(RFS)
```

## Arguments

- RFS:

  A \[RandomForestSurrogates()\] object with \`s\` equal to 0.

## Value

A \`MinimalDepth\` S3 list object: - \`RFS\`: The original
\[RandomForestSurrogates()\] object. - \`selected\`: A character vector
of the selected variable names. - \`depth\`: A numeric vector of the
surrogate minimal depth for each variable. - \`threshold\`: The depth
threshold used to select variables. - \`surrogates\`: - \`average\`:
Total average number of surrogate variables. - \`layer\`: Average number
of surrogate variables by layer (Named numeric of length 1000).

## References

\- Ishwaran, H. et al. (2011) Random survival forests for
high-dimensional data. Stat Anal Data Min, 4, 115–132.
\<https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.10103\> -
Ishwaran, H. et al. (2010) High-Dimensional Variable Selection for
Survival Data. J. Am. Stat. Assoc., 105, 205–217.
\<http://www.ccs.miami.edu/~hishwaran/papers/IKGML.JASA.2010.pdf\>

## Examples

``` r
# \donttest{
data("SMD_example_data")

res.md <- MinimalDepth(RandomForestSurrogates(
  x = SMD_example_data[, -1],
  y = SMD_example_data[, 1],
  num.trees = 10,
  s = 0,
  num.threads = 1
))
#> Warning: `seed` was not set. Your results may not be reproducible.
# }
```
