#
#  s3-generics.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Generics
#
#   CONTENTS:
#

### Generics for sparsebnData
#' @export
sparsebnData <- function(x, ...) UseMethod("sparsebnData", x)

#' @export
as.sparsebnData <- function(x) UseMethod("as.sparsebnData", x)

### Generics for sparsebnPath
#' @export
sparsebnPath <- function(x) UseMethod("sparsebnPath", x)

#' @export
as.sparsebnPath <- function(x) UseMethod("as.sparsebnPath", x)

### Generics for sparsebnFit
#' @export
sparsebnFit <- function(x) UseMethod("sparsebnFit", x)

#' @export
as.sparsebnFit <- function(x) UseMethod("as.sparsebnFit", x)

### Generics for sparse
sparse <- function(x) UseMethod("sparse", x)
as.sparse <- function(x) UseMethod("as.sparse", x)

### Generics for various exported utility functions

#' get.adjacency.matrix
#'
#' Extracts the adjacency matrix of the associated graph object.
#'
#' @return
#' \code{matrix}
#'
#' @export
get.adjacency.matrix <- function(x) UseMethod("get.adjacency.matrix", x)

#' lambda.grid
#'
#' Extracts the lambda values from a \code{\link{sparsebnPath}} object.
#'
#' @export
lambda.grid <- function(x) UseMethod("lambda.grid", x)

#' num.nodes
#'
#' Extracts the number of nodes of the associated graph object.
#'
#' @export
num.nodes <- function(x) UseMethod("num.nodes", x)

#' num.edges
#'
#' Extracts the number of edges of the associated graph object.
#'
#' @export
num.edges <- function(x) UseMethod("num.edges", x)

#' num.samples
#'
#' Extracts the number of samples of the associated object.
#'
#' @export
num.samples <- function(x) UseMethod("num.samples", x)

#' is.zero
#'
#' Determines whether or not the object is the same as the null or zero object from its class.
#'
#' @export
is.zero <- function(x) UseMethod("is.zero", x)

#' Estimate the parameters of a Bayesian network
#'
#' Given the structure of a Bayesian network, estimate the parameters (weights) using ordinary least
#' squares (for Gaussian data) or logistic regression (for discrete data).
#'
#' The low-level fitting method is \code{\link{fit_dag}}.
#'
#' @param fit A fitted object containing the Bayesian network structure to fit.
#' @param data Data to use for fitting.
#'
#' @export
estimate.parameters <- function(fit, data, ...) UseMethod("estimate.parameters", fit)

# Internal generics
pick_family <- function(x) UseMethod("pick_family", x)
reIndexC <- function(x) UseMethod("reIndexC", x)
reIndexR <- function(x) UseMethod("reIndexR", x)
.num_edges <- function(x) UseMethod(".num_edges", x)
# to_B <- function(x) UseMethod("to_B", x)

