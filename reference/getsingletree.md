# getsingletree

This is an internal function

## Usage

``` r
getsingletree(RF, k = 1, add_layer = FALSE)
```

## Arguments

- RF:

  A
  [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  object.

- k:

  Tree index to convert.

- add_layer:

  Boolean.

## Value

A tree data frame for the `k`th tree in `RF`. Each row of the tree data
frames corresponds to a node of the respective tree and the columns
correspond to:

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
