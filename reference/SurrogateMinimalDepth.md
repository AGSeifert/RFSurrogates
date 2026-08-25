# Variable selection with Surrogate Minimal Depth (SMD)

Variable selection with Surrogate Minimal Depth (SMD)

## Usage

``` r
SurrogateMinimalDepth(RFS)
```

## Arguments

- RFS:

  A \[RandomForestSurrogates()\] object.

## Value

A \`SurrogateMinimalDepth\` S3 list object: - \`RFS\`: The original
\[RandomForestSurrogates()\] object. - \`selected\`: A character vector
of the selected variable names. - \`depth\`: A numeric vector of the
surrogate minimal depth for each variable. - \`threshold\`: The depth
threshold used to select variables. - \`surrogates\`: - \`average\`:
Total average number of surrogate variables. - \`layer\`: Average number
of surrogate variables by layer (\`Named num \[1:1000\]\`).

## References

\- Seifert, S. et al. (2019) Surrogate minimal depth as an importance
measure for variables in random forests. Bioinformatics, 35, 3663–3671.
\<https://academic.oup.com/bioinformatics/article/35/19/3663/5368013\> -
Ishwaran, H. et al. (2011) Random survival forests for high-dimensional
data. Stat Anal Data Min, 4, 115–132.
\<https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.10103\> -
Ishwaran, H. et al. (2010) High-Dimensional Variable Selection for
Survival Data. J. Am. Stat. Assoc., 105, 205–217.
\<http://www.ccs.miami.edu/~hishwaran/papers/IKGML.JASA.2010.pdf\>

## Examples

``` r
# \donttest{
data("SMD_example_data")

res.smd <- SurrogateMinimalDepth(RandomForestSurrogates(
  x = SMD_example_data[, -1], y = SMD_example_data[, 1],
  num.trees = 10, s = 10, num.threads = 1
))
#> Warning: `seed` was not set. Your results may not be reproducible.
# }
```
