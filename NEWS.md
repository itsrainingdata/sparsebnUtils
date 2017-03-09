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

