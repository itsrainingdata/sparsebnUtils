# sparsebnUtils 0.0.2

## Major changes

* `get.solution()` has been renamed `select()` (for consistency with `select.parameter()`)

## Features

* Added a `NEWS.md` file to track changes to the package
* `select()` now uses fuzzy matching by default
* Added new method `get.nodes()` to return node names from a `sparsebn` object

## Bug fixes

* Graphics layout for `plot.sparsebnPath()` now accounts for ommitted null graph
* `random.dag()` now correctly accepts `FUN` for user-specification of RNG

# sparsebnUtils 0.0.1

* Initial release

