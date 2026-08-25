# Get a list of structured trees from a ranger object.

This functions creates a list of trees for ranger objects similar as
getTree function does for random Forest objects.

## Usage

``` r
getTreeranger(RF, num.trees = RF$num.trees, add_layer = FALSE, num.threads = 1)
```

## Arguments

- RF:

  A
  [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  object which was created with `keep.inbag = TRUE`.

- num.trees:

  (Deprecated) Number of trees to convert (Default: `RF$num.trees`).

- add_layer:

  (Default: `FALSE`) Whether to
  [`addLayer()`](https://agseifert.github.io/RFSurrogates/reference/addLayer.md)
  in the same loop.

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

- `layer`: If `add_layer` is `TRUE`, see
  [`addLayer()`](https://agseifert.github.io/RFSurrogates/reference/addLayer.md)
