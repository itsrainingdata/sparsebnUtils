# sparsebnUtils 0.0.6

## Features

* Added `nchar` argument to improve output of show.parents with long node names (#13)

# sparsebnUtils 0.0.5

## Features

* `openCytoscape` method added for compatibility with Cytoscape app (sparsebn #4)
* `select.parameter` now works with discrete data
* Added `summary` generics for `sparsebnPath`, `sparsebnFit`, `sparsebnData`, and
`edgeList` objects
* Added `plot` generic for `sparsebnData` objects
* Added `specify.prior` method to simplify construction of black lists
* Improve output when estimate.parameters is singular (#9)

## Bug fixes

* Fixed bug which caused `permute.nodes` to throw an error when passing in a user-specified node ordering
* Correct error message for mixed data (#8)
* Update documentation for conversion methods (#11)

# sparsebnUtils 0.0.4

## Features

* Added new `random.graph` method to generate random `edgeList`s

## Notes

* Conversion to and from sparse matrices using `as.sparse` is now significantly faster and supports `Matrix` input

# sparsebnUtils 0.0.3

## Major changes

* `get.solution()` has been renamed `select()` (for consistency with `select.parameter()`)

## Features

* Added a `NEWS.md` file to track changes to the package
* `select()` now uses fuzzy matching by default
* Added new method `get.nodes()` to return node names from a `sparsebn` object
* Improved output and print method for `estimate.parameters()` when using discrete data

## Bug fixes

* Graphics layout for `plot.sparsebnPath()` now accounts for omitted null graph
* `random.dag()` now correctly accepts `FUN` for user-specification of RNG

# sparsebnUtils 0.0.2

* Initial stable release

