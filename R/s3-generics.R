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

### Generics for sparsebnData ----------------------------------------
#' @export
sparsebnData <- function(data, ...) UseMethod("sparsebnData", data)

#' @export
as.sparsebnData <- function(data) UseMethod("as.sparsebnData", data)

### Generics for sparsebnPath ----------------------------------------
#' @export
sparsebnPath <- function(path) UseMethod("sparsebnPath", path)

#' @export
as.sparsebnPath <- function(path) UseMethod("as.sparsebnPath", path)

### Generics for sparsebnFit ----------------------------------------
#' @export
sparsebnFit <- function(fit) UseMethod("sparsebnFit", fit)

#' @export
as.sparsebnFit <- function(fit) UseMethod("as.sparsebnFit", fit)

### Generics for sparse ---------------------------------------------
#' @export
sparse <- function(x) UseMethod("sparse", x)

#' @export
as.sparse <- function(x) UseMethod("as.sparse", x)

### Generics for edgeList ---------------------------------------------
#' @export
edgeList <- function(x) UseMethod("edgeList", x)

#' @export
as.edgeList <- function(x) UseMethod("as.edgeList", x) # NOTE: Right now this is extended (only) in ccdrAlgorithm,

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

#' get.covariance
#'
#' Computes the implied covariance (or concentration) matrix of the associated graph object.
#'
#' @return
#' \code{matrix}
#'
#' @export
get.covariance <- function(x, ...) UseMethod("get.covariance", x)

#' @rdname get.covariance
#' @export
get.precision <- function(x, ...) UseMethod("get.precision", x)

#' estimate.covariance
#'
#' Estimates the covariance (or concentration) matrix implied by the associated graph object.
#'
#' @return
#' \code{matrix}
#'
#' @export
estimate.covariance <- function(fit, data, ...) UseMethod("estimate.covariance", fit)

#' @rdname estimate.covariance
#' @export
estimate.precision <- function(x, ...) UseMethod("estimate.precision", x)

# Internal generics
pick_family <- function(x) UseMethod("pick_family", x)

#' @export
reIndexC <- function(x) UseMethod("reIndexC", x)

#' @export
reIndexR <- function(x) UseMethod("reIndexR", x)

.num_edges <- function(x) UseMethod(".num_edges", x)
# to_B <- function(x) UseMethod("to_B", x)

