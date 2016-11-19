# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.2 (2016-10-31) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (1.0.44)             |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/New_York             |
|date     |2016-11-18                   |

## Packages

|package       |*  |version |date       |source                                  |
|:-------------|:--|:-------|:----------|:---------------------------------------|
|sparsebnUtils |*  |0.0.3   |2016-11-19 |local (itsrainingdata/sparsebnUtils@NA) |

# Check results
3 packages

## ccdrAlgorithm (0.0.1)
Maintainer: Bryon Aragam <sparsebn@gmail.com>  
Bug reports: https://github.com/itsrainingdata/ccdrAlgorithm/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  
  Type 'demo()' for some demos, 'help()' for on-line help, or
  'help.start()' for an HTML browser interface to help.
  Type 'q()' to quit R.
  
  > library(testthat)
  > library(ccdrAlgorithm)
  > 
  > test_check("ccdrAlgorithm")
  Error: is.data.frame(X) is not TRUE
  testthat results ================================================================
  OK: 0 SKIPPED: 0 FAILED: 0
  Execution halted
```

## discretecdAlgorithm (0.0.1)
Maintainer: Jiaying Gu <gujy.lola@gmail.com>

0 errors | 0 warnings | 0 notes

## sparsebn (0.0.1)
Maintainer: Bryon Aragam <sparsebn@gmail.com>  
Bug reports: https://github.com/itsrainingdata/sparsebn/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: ccdrAlgorithm
Loading required package: discretecdAlgorithm

sparsebn v0.0.1, Copyright (c) 2016
	Bryon Aragam, University of California, Los Angeles
	Jiaying Gu, University of California, Los Angeles
	Dacheng Zhang, University of California, Los Angeles
... 8 lines ...
Number of blocks allowed: 3
Note: method with signature 'ddiMatrix#dMatrix' chosen for function '-',
 target signature 'ddiMatrix#dtCMatrix'.
 "diagonalMatrix#triangularMatrix" would also be valid
A list of interventions was not specified: Assuming data is purely observational.
Number of blocks allowed: 3
Number of blocks allowed: 5886
Quitting from lines 144-153 (sparsebn-vignette.Rmd) 
Error: processing vignette 'sparsebn-vignette.Rmd' failed with diagnostics:
$ operator is invalid for atomic vectors
Execution halted
```

