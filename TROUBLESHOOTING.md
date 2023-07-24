# Troubleshooting common issues

## `Error in var.relations: allvariables do not contain the candidate variables`

This guard clause is intended to catch cases where you are attempting to look at candidate variables not included in the data set.
Firstly, make sure that your `candidates` argument only contains variables present in your indepedent variables `x`:

```r
all(candidates %in% colnames(x)) # Must evaluate to TRUE
```

However, if you are using a data set with number-like feature identifiers, these will be automatically coerced into `X<number>` when the data frame is created, causing this mismatch.
To fix this, change your argument by adding `paste0("X", candidates)`.
Make sure to do the same for the `variables` argument too.

## `var.select.smd` is stuck at `Growing trees.. Progress`

The output `Growing trees..` is made by `ranger` while a large forest is being built. The main part of the function however does not emit any progress messages, causing it to appear stalled.
You can verify the function is still running by checking your CPU usage.
This issue is especially relevant for large data sets.

This should be improved since the rework (Version 0.3.0).
It is still recommended to use a high performance computer with large amounts of RAM for larger data sets.
