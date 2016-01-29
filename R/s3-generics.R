#
#  s3-generics.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#
# PACKAGE CCDR: Generics
#
#   CONTENTS:
#

# Generics for ccdrPath
ccdrPath <- function(x) UseMethod("ccdrPath", x)
as.ccdrPath <- function(x) UseMethod("as.ccdrPath", x)

# Generics for ccdrFit
ccdrFit <- function(x) UseMethod("ccdrFit", x)
as.ccdrFit <- function(x) UseMethod("as.ccdrFit", x)

# Generics for SparseBlockMatrixR
SparseBlockMatrixR <- function(x) UseMethod("SparseBlockMatrixR", x)
as.SparseBlockMatrixR <- function(x) UseMethod("as.SparseBlockMatrixR", x)

# Generics for sparse
sparse <- function(x) UseMethod("sparse", x)
as.sparse <- function(x) UseMethod("as.sparse", x)

# Generics for various exported utility functions

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
#' Extracts the lambda values from a \code{\link{ccdrPath-class}} object.
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

# Internal generics
reIndexC <- function(x) UseMethod("reIndexC", x)
reIndexR <- function(x) UseMethod("reIndexR", x)
.num_edges <- function(x) UseMethod(".num_edges", x)
to_B <- function(x) UseMethod("to_B", x)

