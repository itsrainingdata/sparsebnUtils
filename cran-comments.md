## Test environments
* local OS X install, R 3.4.1
* ubuntu 12.04.5 (travis-ci: oldrel, devel, and release)
* win-builder (devel and release)
* r-hub (devel)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

The errors for v0.0.4 at https://cran.rstudio.com/web/checks/check_results_sparsebnUtils.html have all been fixed. These were caused by failing to properly check for Suggested packages in the unit tests, and have been resolved. 

## Reverse dependencies

* ccdrAlgorithm, discretecdAlgorithm, and sparsebn: You will receive updates to these packages that fixes these issues very soon from myself and Jiaying. Each of these packages depend on the current submission for basic functionality.

