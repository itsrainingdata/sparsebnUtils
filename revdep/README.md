# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.3 (2017-03-06) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (1.0.44)             |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/New_York             |
|date     |2017-03-08                   |

## Packages

|package       |*  |version |date       |source                                  |
|:-------------|:--|:-------|:----------|:---------------------------------------|
|sparsebnUtils |*  |0.0.4   |2017-03-08 |local (itsrainingdata/sparsebnUtils@NA) |

# Check results
3 packages

## ccdrAlgorithm (0.0.2)
Maintainer: Bryon Aragam <sparsebn@gmail.com>  
Bug reports: https://github.com/itsrainingdata/ccdrAlgorithm/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Complete output:
  > library(testthat)
  > library(ccdrAlgorithm)
  > 
  > test_check("ccdrAlgorithm")
  Error: 'cor_vector' is not an exported object from 'namespace:sparsebnUtils'
  testthat results ================================================================
  OK: 0 SKIPPED: 0 FAILED: 0
  Execution halted
```

## discretecdAlgorithm (0.0.2)
Maintainer: Jiaying Gu <gujy.lola@gmail.com>

0 errors | 0 warnings | 0 notes

## sparsebn (0.0.2)
Maintainer: Bryon Aragam <sparsebn@gmail.com>  
Bug reports: https://github.com/itsrainingdata/sparsebn/issues

0 errors | 1 warning  | 0 notes

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'estimate.dag.Rd':
  ‘[discretecdAlgorithm]{cd.run}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

