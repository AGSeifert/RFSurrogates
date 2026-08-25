# Changelog

## RFSurrogates 0.4.4

- Remove unused `linkcomm` dependency and associated `build.clusters`
  function.
- *Internal*: Change to snapshot tests.
- Minor changes to documentation.

## RFSurrogates 0.4.3

- Fixed an issue preventing the installation of the package with a
  system that uses gcc 14+.
  ([\#17](https://github.com/AGSeifert/RFSurrogates/issues/17))

## RFSurrogates 0.4.2

- Added new optional parameter to `MeanAdjustedAgreement` and
  `meanAdjAgree`: `round_digits` defaulting to 2. This offers the same
  behaviour as before, but allows rounding to different amounts of
  decimal places if desired.

## RFSurrogates 0.4.1

- Fix `SurrogateMinimalDepth`’s result `$selected` returning modified
  names ([\#13](https://github.com/AGSeifert/RFSurrogates/issues/13))

## RFSurrogates 0.4.0

**Added
[`RandomForestSurrogates()`](https://agseifert.github.io/RFSurrogates/reference/RandomForestSurrogates.md).**

- This functions aims to replace the first section of the variable
  selection and relation functions by creating a single reusable object
  which contains the random forest
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  model, as well as the `trees` list with layers and surrogates added.
- Returns a `RandomForestSurrogates` object, which serves as the base
  object for later analysis.
- Additional `...` params are passed directly to
  [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md).
- `s.pct` is a helper for calculating the number of surrogates as a
  fraction of number of variables (Default: 0.01). `s` can be set to
  overwrite this default.
- `mtry` supports the following values:
  - One of the documented `string` values, which will cause the `mtry`
    passed to
    [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
    to be a function accepting the number of variables, and returning
    the specific transformation after flooring the result.
  - A `function` which takes the number of variables as its first and
    only param, and returns the value of `mtry`.
  - A `numeric` value for `mtry`.
  - The default is `"^3/4"`.
- `type` also uses
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) and still
  defaults to `"regression"`.
- `num.threads` defaults to 1.
- `permutate` will, if set to `TRUE`, apply random permutation to the
  data in each feature. (This is used in permutation importance
  approaches.)
- `seed` is now a strongly recommended optional parameter (issuing a
  warning whenever it is not set).
  - Setting `seed` will cause a call to
    [`set.seed()`](https://rdrr.io/r/base/Random.html) when permutating.
    It is also used as the `seed` param of the
    [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
    call.
  - Requiring `seed` as a function parameter is preferred because it
    does not rely on global, non-reproducible state of the random number
    generator, if it was not seeded immediately before the function
    call.
- The inner call to
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  includes the following defaults:
  - `keep.inbag = TRUE`
  - `respect.unordered.factors = "partition"`
  - Data is passed as a data.frame with the special column `y`, and the
    optional special column `status` for survival forests.
    - `x` must not contain the column names `y` or `status`, as this may
      lead to unexpected behavior.
- In general, input parameters are more strictly validated.
- The function uses `num.threads` to also parallelize creating the list
  of trees with layers.

**Added
[`SurrogateMinimalDepth()`](https://agseifert.github.io/RFSurrogates/reference/SurrogateMinimalDepth.md)**
as a replacement for
[`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md).
It takes just a `RandomForestSurrogates` object as a param, and returns
a `SurrogateMinimalDepth` list object.

Value:

- `RFS`: The `RandomForestSurrogates` object used.
- `selected`: The selected variables (previously `var`).
- `depth`: The surrogate minimal depth of each variable (previously
  `info$depth`).
- `threshold`: The threshold used to select variables (previously
  `info$threshold`).
- `surrogates`: Details on the average number of surrogates in the
  forest, and per layer.
  - `average`: Previously `s$s.a`.
  - `layer`: Previously `s$s.l`.

**Added
[`MeanAdjustedAgreement()`](https://agseifert.github.io/RFSurrogates/reference/MeanAdjustedAgreement.md)**
as a replacement for
[`var.relations()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.md).
It takes a `RandomForestSurrogates` object as a param, and returns a
`MeanAdjustedAgreement` list object.

Value:

- `RFS`: The `RandomForestSurrogates` object used.
- `relations`: The matrix of mean adjusted agreements for the
  investigated variables (rows) and the possible candidates (columns)
  (previously `surr.res`).
- `threshold`: The threshold used to select related variables.
- `related`: A list of vectors for each investigated variable containing
  the related candidate variables (previously `var`).

**Added
[`MFI()`](https://agseifert.github.io/RFSurrogates/reference/MFI.md)**
as replacement for
[`var.relations.mfi()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.mfi.md).
[`MFI()`](https://agseifert.github.io/RFSurrogates/reference/MFI.md)
takes the same arguments as
[`RandomForestSurrogates()`](https://agseifert.github.io/RFSurrogates/reference/RandomForestSurrogates.md),
with the additional `variables` and `candidates` params. Perform
variable selection using
[`MutualForestImpactVariableSelection()`](https://agseifert.github.io/RFSurrogates/reference/MutualForestImpactVariableSelection.md).

**Added
[`MutualImpurityReduction()`](https://agseifert.github.io/RFSurrogates/reference/MutualImpurityReduction.md)**
as a replacement for
[`var.select.mir()`](https://agseifert.github.io/RFSurrogates/reference/var.select.mir.md),
where variable selection is performed with
[`MutualImpurityReductionVariableSelection()`](https://agseifert.github.io/RFSurrogates/reference/MutualImpurityReductionVariableSelection.md).

## RFSurrogates 0.3.4

- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md),
  [`var.select.md()`](https://agseifert.github.io/RFSurrogates/reference/var.select.md.md),
  [`var.relations()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.md),
  [`var.relations.mfi()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.mfi.md):
  Made several improvements to developer experience:
  - `create.forest` now defaults to `is.null(forest)`, so it will
    automatically be `TRUE` if no forest is provided, and `FALSE`
    otherwise.
  - `x` is no longer required if `create.forest` is `FALSE`.
  - (Internal) Inverted some nested guard clauses for readability.
- [`addLayer()`](https://agseifert.github.io/RFSurrogates/reference/addLayer.md):
  Refactor for-loop to lapply.
  - Add `num.threads` param to enable parallelization using
    [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html).
    It defaults to 1 for backward compatability.
- [`getTreeranger()`](https://agseifert.github.io/RFSurrogates/reference/getTreeranger.md):
  Refactor [`lapply()`](https://rdrr.io/r/base/lapply.html) to
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html).
  - Add `num.threads` param (passed to `mc.cores` in
    [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)).
    It defaults to 1 for backward compatability.
  - Add `add_layer` param to include the effect of `addLayer` within the
    same loop. Defaults to `FALSE` for backward compatability.
  - (Internal)
    [`getsingletree()`](https://agseifert.github.io/RFSurrogates/reference/getsingletree.md):
    Add `add_layer` param to enable adding layers within the same loop.
- [`addSurrogates()`](https://agseifert.github.io/RFSurrogates/reference/addSurrogates.md):
  - Clarified default value for `num.threads` to be
    [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
    by adding it as a default to the parameter
  - Added assertion that `RF` is a `ranger` object.
  - Added assertion that `RF$num.trees` and `length(trees)` are equal.
    This is not considered a breaking change since these values should
    always be equal when the function is used correctly.
- Added S3 classes to the `trees` list objects.
  - [`getTreeranger()`](https://agseifert.github.io/RFSurrogates/reference/getTreeranger.md)
    now returns a `RangerTrees` list.
  - [`addLayer()`](https://agseifert.github.io/RFSurrogates/reference/addLayer.md)
    and `getTreeranger(add_layer = TRUE)` add the `LayerTrees` class to
    the list (indicating presence of the `layer` list item). It now
    requires that its `trees` param inherits `RangerTrees`.
  - [`addSurrogates()`](https://agseifert.github.io/RFSurrogates/reference/addSurrogates.md)
    now adds the `SurrogateTrees` class. It now requires that its
    `trees` param inherits `RangerTrees`.

## RFSurrogates 0.3.3

- Fixed
  [`meanAdjAgree()`](https://agseifert.github.io/RFSurrogates/reference/meanAdjAgree.md)
  bug which caused mean adjusted agreement pairings to be set to NA
  incorrectly when `variables` was a subset or differently ordered than
  `candidates`.

## RFSurrogates 0.3.2

- Moved to new repository:
  [AGSeifert/RFSurrogates](https://github.com/AGSeifert/RFSurrogates)
- Fixed
  [`mean.index()`](https://agseifert.github.io/RFSurrogates/reference/mean.index.md)
  bug which caused the return value to be of incorrect length in some
  cases ([\#4](https://github.com/AGSeifert/RFSurrogates/issues/4)).

------------------------------------------------------------------------

> This is the legacy changelog of SurrogateMinimalDepth, previously
> located at `Version_info.txt`, adapted to Tidyverse Style for news.
> Future releases should begin following [Semantic
> Versioning](https://semver.org/spec/v2.0.0.html), and changes should
> be easier to track using [Conventional
> Commits](https://www.conventionalcommits.org/en/v1.0.0/).

------------------------------------------------------------------------

## RFSurrogates 0.3.1 (RFSurrogates [0.3.1](https://github.com/StephanSeifert/RFSurrogates/tree/v0.3.1))

- Renamed package `SurrogateMinimalDepth` to `RFSurrogates`
- Renamed parameter `ntree` to `num.trees` throughout the package.

## RFSurrogates 0.3.0 (SurrogateMinimalDepth [0.3.0](https://github.com/StephanSeifert/RFSurrogates/tree/v0.3.0))

- Round Mean Adjusted Agreement to 2 digits.
- [`meanAdjAgree()`](https://agseifert.github.io/RFSurrogates/reference/meanAdjAgree.md):
  Sped up.
- Changed that relations to variables not used as primary split are set
  as NA (before it was 0).
- `var.relations.corr()` renamed to
  [`var.relations.mfi()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.mfi.md)
  (as named in the paper).
- Bugfix for MFI and MIR.
- Permutation approach to determine p-values was optimized for the
  selection of important and related variables.
- Default approach for p-value calculation was set to `"permutation"`.

## RFSurrogates 0.2.1 (SurrogateMinimalDepth 0.2.1)

- Included possibility to analyze categorial variables.

## RFSurrogates 0.2.0 (SurrogateMinimalDepth [0.2.0](https://github.com/StephanSeifert/RFSurrogates/tree/0.2.0))

- New
  [`var.select.mir()`](https://agseifert.github.io/RFSurrogates/reference/var.select.mir.md)
  to select important variables based on Mutual Impurity Reduction.
- New `var.relations.corr()` to calculated unbiased relations.
- Added multicore calculation of variable relations.
- [`var.relations()`](https://agseifert.github.io/RFSurrogates/reference/var.relations.md):
  Included possibility to only calculate relations and not select
  variables.
- Included the possibility to set case weights.

## RFSurrogates 0.1.10 (SurrogateMinimalDepth [0.1.10](https://github.com/StephanSeifert/RFSurrogates/tree/v0.1.10))

- Fixed bug not including first and last surrogate.
- Added random selection of surrogates when adjusted agreement are the
  same.
- Fixed bug in `var.relation()` example.
- New `build.clusters()` to obtain variable groups.

## RFSurrogates 0.1.9 (SurrogateMinimalDepth [0.1.9](https://github.com/StephanSeifert/RFSurrogates/tree/v0.1.9))

- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md):
  Some specifics to set `s` were adapted.
- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md),
  [`var.select.md()`](https://agseifert.github.io/RFSurrogates/reference/var.select.md.md):
  Added `save.memory` parameter (to build the forst with ranger).

## RFSurrogates 0.1.8 (SurrogateMinimalDepth 0.1.8)

- [`var.select.md()`](https://agseifert.github.io/RFSurrogates/reference/var.select.md.md)
  now executes
  [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md)
  with `s = 0` instead of using a separate function.
- New
  [`reduce.surrogates()`](https://agseifert.github.io/RFSurrogates/reference/reduce.surrogates.md)
  function added.
- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md):
  Added parameters `create.forest` and `forest` to use existing forests
  (e.g. created by
  [`reduce.surrogates()`](https://agseifert.github.io/RFSurrogates/reference/reduce.surrogates.md)).
- Renamed parameter `trees` to `forest` containing trees and variable
  names.
- The C-code is updated to enable multicore analysis.

## RFSurrogates 0.1.7 (SurrogateMinimalDepth 0.1.7)

- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md),
  [`var.select.md()`](https://agseifert.github.io/RFSurrogates/reference/var.select.md.md):
  Added parameter. `save.ranger` to save ranger object.

## RFSurrogates 0.1.6 (SurrogateMinimalDepth 0.1.6)

- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md):
  Adapted threshold for low depth trees.

## RFSurrogates 0.1.5 (SurrogateMinimalDepth 0.1.5)

- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md):
  Added `s` as a parameter.
- [`var.select.smd()`](https://agseifert.github.io/RFSurrogates/reference/var.select.smd.md),
  [`var.select.md()`](https://agseifert.github.io/RFSurrogates/reference/var.select.md.md):
  Implemented survival function.

## RFSurrogates 0.1.4 (SurrogateMinimalDepth 0.1.4)

- All errors and comments from coauthors implemented: first version
  uploaded on github.

## RFSurrogates 0.1.3 (SurrogateMinimalDepth 0.1.3)

No changelog available.

## RFSurrogates 0.1.2 (SurrogateMinimalDepth 0.1.2)

No changelog available.

## RFSurrogates 0.1.1 (SurrogateMinimalDepth 0.1.1)

No changelog available.

## RFSurrogates 0.1.0 (SurrogateMinimalDepth 0.1.0)

No changelog available.
