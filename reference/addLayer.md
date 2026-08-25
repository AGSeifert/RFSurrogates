# Add layer information to a forest that was created by getTreeranger

This functions adds the layer information to each node in a list with
trees that was obtained by getTreeranger. You should use
[`getTreeranger()`](https://agseifert.github.io/RFSurrogates/reference/getTreeranger.md)
with `add_layer = TRUE` instead.

## Usage

``` r
addLayer(trees, num.threads = 1)
```

## Arguments

- trees:

  The output of
  [`getTreeranger()`](https://agseifert.github.io/RFSurrogates/reference/getTreeranger.md).

- num.threads:

  (Default: 1) Number of threads to spawn for parallelization.

## Value

A list of tree data frames of length `RF$num.trees`. Each row of the
tree data frames corresponds to a node of the respective tree and the
columns correspond to:

- `nodeID`: ID of the respective node (important for left and right
  daughters in the next columns)

- `leftdaughter`: ID of the left daughter of this node

- `rightdaughter`: ID of the right daughter of this node

- `splitvariable`: ID of the split variable

- `splitpoint`: Split point of the split variable. For categorical
  variables this is a comma separated lists of values, representing the
  factor levels (in the original order) going to the right.

- `status`: `0` for terminal (`splitpoint` is `NA`) and `1` for
  non-terminal.

- `layer`: Tree layer depth information, starting at 0 (root node) and
  incremented for each layer.
