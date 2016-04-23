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

#' Internal generics for fitted objects and data
#'
#' Internal methods provided by \code{sparsebnUtils} for development.
#'
#' @name sparsebn-classes
#' @rdname sparsebn-classes
NULL


### Generics for sparsebnData ----------------------------------------
#' @rdname sparsebnData
#' @export
sparsebnData <- function(data, ...) UseMethod("sparsebnData", data)

#' @rdname sparsebnData
#' @export
as.sparsebnData <- function(data) UseMethod("as.sparsebnData", data)

### Generics for sparsebnPath ----------------------------------------
#' @rdname sparsebnPath
#' @export
sparsebnPath <- function(path) UseMethod("sparsebnPath", path)

#' @rdname sparsebnPath
#' @export
as.sparsebnPath <- function(path) UseMethod("as.sparsebnPath", path)

### Generics for sparsebnFit ----------------------------------------
#' @rdname sparsebnFit
#' @export
sparsebnFit <- function(fit) UseMethod("sparsebnFit", fit)

#' @rdname sparsebnFit
#' @export
as.sparsebnFit <- function(fit) UseMethod("as.sparsebnFit", fit)

### Generics for sparse ---------------------------------------------
#' @rdname sparse
#' @export
sparse <- function(x) UseMethod("sparse", x)

#' @rdname sparse
#' @export
as.sparse <- function(x) UseMethod("as.sparse", x)

### Generics for edgeList ---------------------------------------------
#' @rdname edgeList
#' @export
edgeList <- function(x) UseMethod("edgeList", x)

#' @rdname edgeList
#' @export
as.edgeList <- function(x) UseMethod("as.edgeList", x) # NOTE: Right now this is extended (only) in ccdrAlgorithm,

#' Conversion to edgeList object
#'
#' \code{to_edgeList} converts an object to an \code{\link{edgeList}} object. Works on both fitted
#' objects and graphs themselves. In the first case, every underlying 'edges' component is converted to
#' \code{\link{edgeList}}. In the second, the conversion applies directly to the object.
#'
#' @param x An object of type \code{\link{sparsebnPath}}, \code{\link{sparsebnFit}}, \code{\link[graph]{graphNEL-class}},
#' \code{\link[igraph]{igraph}}, or \code{\link[network]{network}}.
#'
#' @export
to_edgeList <- function(x) UseMethod("to_edgeList", x)

### Generics for various exported utility functions

#' get.adjacency.matrix
#'
#' Extracts the adjacency matrix of the associated graph object.
#'
#' @param x any \code{R} object.
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
#' @param x a \code{\link{sparsebnPath}} object.
#'
#' @return
#' Vector of \code{numeric} lambda values in fitted object.
#'
#' @export
lambda.grid <- function(x) UseMethod("lambda.grid", x)

#' num.nodes
#'
#' Extracts the number of nodes of the associated graph object.
#'
#' @param x a \code{\link{sparsebnFit}} or \code{\link{sparsebnPath}} object.
#'
#' @return
#' Number of nodes as \code{integer}.
#'
#' @export
num.nodes <- function(x) UseMethod("num.nodes", x)

#' num.edges
#'
#' Extracts the number of edges of the associated graph object.
#'
#' @param x a \code{\link{sparsebnFit}} or \code{\link{sparsebnPath}} object.
#'
#' @return
#' Number of edges as \code{integer}.
#'
#' @export
num.edges <- function(x) UseMethod("num.edges", x)

#' num.samples
#'
#' Extracts the number of samples used to estimate the associated object.
#'
#' @param x a \code{\link{sparsebnFit}} or \code{\link{sparsebnPath}} object.
#'
#' @return
#' Number of samples as \code{integer}.
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

#' @rdname sparsebn-functions
#' @export
reIndexC <- function(x) UseMethod("reIndexC", x)

#' @rdname sparsebn-functions
#' @export
reIndexR <- function(x) UseMethod("reIndexR", x)

.num_edges <- function(x) UseMethod(".num_edges", x)
# to_B <- function(x) UseMethod("to_B", x)

