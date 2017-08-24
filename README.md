sparsebnUtils
=============

[![Project Status: Active The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Travis-CI Build Status](https://travis-ci.org/itsrainingdata/sparsebnUtils.svg?branch=master)](https://travis-ci.org/itsrainingdata/sparsebnUtils) [![](http://www.r-pkg.org/badges/version/sparsebnUtils)](http://www.r-pkg.org/pkg/sparsebnUtils) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/sparsebnUtils)](http://www.r-pkg.org/pkg/sparsebnUtils)

A set of tools for representing and estimating sparse Bayesian networks from continuous and discrete data.

Overview
--------

This package provides various S3 classes for making it easy to estimate graphical models from data:

-   `sparsebnData` for managing experimental data with interventions.
-   `sparsebnFit` for representing the output of a DAG learning algorithm.
-   `sparsebnPath` for representing a solution path of estimates.

The package also provides methods for manipulating these objects and for estimating parameters in graphical models:

-   `estimate.parameters` for directed graphs.
-   `get.precision` for undirected graphs.
-   `get.covariance` for covariance matrices.
