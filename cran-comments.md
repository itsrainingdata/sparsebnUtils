## Test environments
* local OS X install, R 3.4.3
* ubuntu 12.04.5 (travis-ci: oldrel, devel, and release)
* win-builder (devel and release)
* r-hub (devel)

## R CMD check results
There were no ERRORs or WARNINGs. There was 1 NOTE:

checking dependencies in R code ... NOTE
Unexported object imported by a ':::' call: ‘igraph:::as_graphnel’

- This is due to a known bug in the igraph package in which the method
as_graphnel was mistakenly not exported, and has already been
patched (https://github.com/igraph/rigraph/pull/222). Once this patch
makes it to CRAN, we will update our package according to match the 
idiomatic use of :: instead of :::.

## Reverse dependencies

* ccdrAlgorithm, discretecdAlgorithm, and sparsebn: The changes in this update
do not affect any of these packages. Reverse dependencies have been tested and 
checked, everything passes.

