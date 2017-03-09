## Test environments
* local OS X install, R 3.3.3
* ubuntu 12.04.5 (travis-ci: oldrel, devel, and release)
* win-builder (devel and release)
* r-hub (devel)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Reverse dependencies

* ccdrAlgorithm, discretecdAlgorithm, and sparsebn: You will receive updates to these packages that fixes these issues very soon from myself and Jiaying. Each of these packages depend on the current submission for basic functionality.

## Re-submission notes

- Thanks, are all your examples wrapped in `\dontrun{}`? This way they are ot checked by R CMD check, is there some reason?

I have unwrapped most of the examples; the two examples that are still wrapped in `\dontrun{}` depend on a dataset from a dependent package and hence are not run. This dataset is important for the examples.

The issues with win-builder have also been resolved. The package has also been tested and CHECK has passed on R 3.3.3.
