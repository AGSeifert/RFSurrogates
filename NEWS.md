# RFSurrogates (development version)

<!-- News Style-guide: https://style.tidyverse.org/news.html -->

* Moved to new repository: [AGSeifert/RFSurrogates](https://github.com/AGSeifert/RFSurrogates)
* Fixed `mean.index()` bug which caused the return value to be of incorrect length in some cases.

---

> This is the legacy changelog of SurrogateMinimalDepth, previously located at `Version_info.txt`, adapted to Tidyverse Style for news.
> Future releases should begin following [Semantic Versioning](https://semver.org/spec/v2.0.0.html), and changes should be easier to track using [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

---

# RFSurrogates 0.3.1 (RFSurrogates [0.3.1](https://github.com/StephanSeifert/RFSurrogates/tree/v0.3.1))

* Renamed package `SurrogateMinimalDepth` to `RFSurrogates`
* Renamed parameter `ntree` to `num.trees` throughout the package.

# RFSurrogates 0.3.0 (SurrogateMinimalDepth [0.3.0](https://github.com/StephanSeifert/RFSurrogates/tree/v0.3.0))

* Round Mean Adjusted Agreement to 2 digits.
* `meanAdjAgree()`: Sped up.
* Changed that relations to variables not used as primary split are set as NA (before it was 0).
* `var.relations.corr()` renamed to `var.relations.mfi()` (as named in the paper).
* Bugfix for MFI and MIR.
* Permutation approach to determine p-values was optimized for the selection of important and related variables.
* Default approach for p-value calculation was set to `"permutation"`.

# RFSurrogates 0.2.1 (SurrogateMinimalDepth 0.2.1)

* Included possibility to analyze categorial variables.

# RFSurrogates 0.2.0 (SurrogateMinimalDepth [0.2.0](https://github.com/StephanSeifert/RFSurrogates/tree/0.2.0))

* New `var.select.mir()` to select important variables based on Mutual Impurity Reduction.
* New `var.relations.corr()` to calculated unbiased relations.
* Added multicore calculation of variable relations.
* `var.relations()`: Included possibility to only calculate relations and not select variables.
* Included the possibility to set case weights.

# RFSurrogates 0.1.10 (SurrogateMinimalDepth [0.1.10](https://github.com/StephanSeifert/RFSurrogates/tree/v0.1.10))

* Fixed bug not including first and last surrogate.
* Added random selection of surrogates when adjusted agreement are the same.
* Fixed bug in `var.relation()` example.
* New `build.clusters()` to obtain variable groups.

# RFSurrogates 0.1.9 (SurrogateMinimalDepth [0.1.9](https://github.com/StephanSeifert/RFSurrogates/tree/v0.1.9))

* `var.select.smd()`: Some specifics to set `s` were adapted.
* `var.select.smd()`, `var.select.md()`: Added `save.memory` parameter (to build the forst with ranger).

# RFSurrogates 0.1.8 (SurrogateMinimalDepth 0.1.8)

* `var.select.md()` now executes `var.select.smd()` with `s = 0` instead of using a separate function.
* New `reduce.surrogates()` function added.
* `var.select.smd()`: Added parameters `create.forest` and `forest` to use existing forests (e.g. created by `reduce.surrogates()`).
* Renamed parameter `trees` to `forest` containing trees and variable names.
* The C-code is updated to enable multicore analysis.

# RFSurrogates 0.1.7 (SurrogateMinimalDepth 0.1.7)

* `var.select.smd()`, `var.select.md()`: Added parameter. `save.ranger` to save ranger object.

# RFSurrogates 0.1.6 (SurrogateMinimalDepth 0.1.6)

* `var.select.smd()`: Adapted threshold for low depth trees.

# RFSurrogates 0.1.5 (SurrogateMinimalDepth 0.1.5)

* `var.select.smd()`: Added `s` as a parameter.
* `var.select.smd()`, `var.select.md()`: Implemented survival function.

# RFSurrogates 0.1.4 (SurrogateMinimalDepth 0.1.4)

* All errors and comments from coauthors implemented: first version uploaded on github.

# RFSurrogates 0.1.3 (SurrogateMinimalDepth 0.1.3)

No changelog available.

# RFSurrogates 0.1.2 (SurrogateMinimalDepth 0.1.2)

No changelog available.

# RFSurrogates 0.1.1 (SurrogateMinimalDepth 0.1.1)

No changelog available.

# RFSurrogates 0.1.0 (SurrogateMinimalDepth 0.1.0)

No changelog available.

