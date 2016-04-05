#
#  s3-sparsebnPath.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# sparsebnPath S3 Class for R
#------------------------------------------------------------------------------#

#
# sparsebnPath S3 class skeleton
#
# Data
# * <wrapper for a list>
#
# Methods
# * is.sparsebnPath
# * sparsebnPath.list
# * print.sparsebnPath
# * as.list.sparsebnPath
# * num.nodes.sparsebnPath
# * num.edges.sparsebnPath
# * num.samples.sparsebnPath
# * lambda.grid.sparsebnPath
# * get.adjacency.matrix.sparsebnPath
#

#' sparsebnPath class
#'
#' Convenience wrapper class for output of CCDr algorithm: Represents the entire solution path
#' of the CCDr algorithm. Its components are of type \code{\link{sparsebnFit}}. Also inherits
#' from \code{\link{list}}.
#'
#' Each value of lambda in the (discrete) solution path corresponds to a single DAG estimate, which
#' is of the form (Phi, Rho) (see \href{http://arxiv.org/abs/1401.0852}{Aragam and Zhou (2015), JMLR} for details).
#' Internally, this estimate is represented by a \code{\link{sparsebnFit}} object. The full solution
#' path is then represented as a \code{\link{list}} of \code{\link{sparsebnFit}} objects: This class is essentially a wrapper for this list.
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}}, \code{\link{lambda.grid}},
#' \code{\link{num.nodes}}, \code{\link{num.edges}}, \code{\link{num.samples}}
#'
#' @docType class
#' @name sparsebnPath
NULL

is.sparsebnPath <- function(path){
    inherits(path, "sparsebnPath")
} # END IS.sparsebnPath

# sparsebnPath constructor
sparsebnPath.list <- function(li){
    if(!check_list_class(li, "sparsebnFit")){
        stop("Some component is not of type sparsebnPath -- sparsebnPath objects must consist of sparsebnFit components only.")
    }

    ### Note that we still allow these objects to inherit from the base list class
    structure(li, class = c("sparsebnPath", "list"))
} # END sparsebnPath.LIST

#' print.sparsebnPath
#'
#' Prints the contents of a \code{\link{sparsebnPath}} object neatly.
#'
#' @param verbose If \code{TRUE}, then each estimate in the solution path is printed separately. Do not use for
#'        large graphs or large solution paths. (default = \code{FALSE})
#'
print.sparsebnPath <- function(path, verbose = FALSE){
    if(verbose){
        print.default(path) # default generic reverts to list => separate calls to print.sparsebnFit for each component
    } else{
        cat("CCDr solution path\n",
            length(path), " estimates for lambda in [", min(lambda.grid(path)), ",", max(lambda.grid(path)), "]\n",
            "Number of edges per solution: ", paste(num.edges(path), collapse = "-"), "\n",
            num.nodes(path), " nodes\n",
            num.samples(path), " observations\n",
            sep = "")
    }
} # END PRINT.SPARSEBNPATH

as.list.sparsebnPath <- function(path){
    class(path) <- "list"
    path
} # END AS.LIST.SPARSEBNPATH

#' @export
#' @describeIn num.nodes Extracts the number of nodes of \link{sparsebnPath} object.
num.nodes.sparsebnPath <- function(path){
    unique(unlist(lapply(path, function(x) x$pp)))
} # END NUM.NODES.SPARSEBNPATH

#' @export
#' @describeIn num.edges Extracts the number of edges of \link{sparsebnPath} object.
num.edges.sparsebnPath <- function(path){
    ### unique(.) not needed since different estimates should have different # of edges
    unlist(lapply(path, function(x) x$nedge))
} # END NUM.EDGES.SPARSEBNPATH

#' @export
#' @describeIn num.samples Extracts the number of samples of \link{sparsebnPath} object.
num.samples.sparsebnPath <- function(path){
    unique(unlist(lapply(path, function(x) x$nn)))
} # END NUM.SAMPLES.SPARSEBNPATH

#' Extract regularization path from solution path
#'
#' Returns a vector of lambda values defining the solution path of a \code{\link{sparsebnPath}} object.
#'
lambda.grid.sparsebnPath <- function(path){
    lambdas <- unlist(lapply(path, function(x){ x$lambda}))
    names(lambdas) <- NULL

    lambdas
} # END LAMBDA.GRID.sparsebnPath

#' @describeIn get.adjacency.matrix Retrieves all \code{edges} slots in the solution path, converts to an adjacency matrix, and returns as a list
get.adjacency.matrix.sparsebnPath <- function(path){
    lapply(path, get.adjacency.matrix)
} # END GET.ADJACENCY.MATRIX.sparsebnPath
