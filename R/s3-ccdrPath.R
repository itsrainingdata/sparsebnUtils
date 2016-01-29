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
#' of the CCDr algorithm. Its components are of type \code{\link{sparsebnFit-class}}. Also inherits
#' from \code{\link{list}}.
#'
#' Each value of lambda in the (discrete) solution path corresponds to a single DAG estimate, which
#' is of the form (Phi, Rho) (see \href{http://arxiv.org/abs/1401.0852}{Aragam and Zhou (2015), JMLR} for details).
#' Internally, this estimate is represented by a \code{\link{sparsebnFit-class}} object. The full solution
#' path is then represented as a \code{\link{list}} of \code{\link{sparsebnFit-class}} objects: This class is essentially a wrapper for this list.
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}}, \code{\link{lambda.grid}},
#' \code{\link{num.nodes}}, \code{\link{num.edges}}, \code{\link{num.samples}}
#'
#' @docType class
#' @name sparsebnPath-class
NULL

#' @export
is.sparsebnPath <- function(cp){
    inherits(cp, "sparsebnPath")
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
#' Prints the contents of a \code{\link{sparsebnPath-class}} object neatly.
#'
#' @param verbose If \code{TRUE}, then each estimate in the solution path is printed separately. Do not use for
#'        large graphs or large solution paths. (default = \code{FALSE})
#'
#' @export
print.sparsebnPath <- function(cp, verbose = FALSE){
    if(verbose){
        print.default(cp) # default generic reverts to list => separate calls to print.sparsebnFit for each component
    } else{
        cat("CCDr solution path\n",
            length(cp), " estimates for lambda in [", min(lambda.grid(cp)), ",", max(lambda.grid(cp)), "]\n",
            "Number of edges per solution: ", paste(num.edges(cp), collapse = "-"), "\n",
            num.nodes(cp), " nodes\n",
            num.samples(cp), " observations\n",
            sep = "")
    }
} # END PRINT.sparsebnPath

#' @export
as.list.sparsebnPath <- function(cp){
    class(cp) <- "list"
    cp
} # END AS.LIST.sparsebnPath

#' @export
#' @describeIn num.nodes
num.nodes.sparsebnPath <- function(cp){
    unique(unlist(lapply(cp, function(x) x$pp)))
} # END NUM.NODES.sparsebnPath

#' @export
#' @describeIn num.edges
num.edges.sparsebnPath <- function(cp){
    ### unique(.) not needed since different estimates should have different # of edges
    unlist(lapply(cp, function(x) x$nedge))
} # END NUM.EDGES.sparsebnPath

#' @export
#' @describeIn num.samples
num.samples.sparsebnPath <- function(cp){
    unique(unlist(lapply(cp, function(x) x$nn)))
} # END NUM.SAMPLES.sparsebnPath

#' lambda.grid.sparsebnPath
#'
#' @export
lambda.grid.sparsebnPath <- function(cp){
    lambdas <- unlist(lapply(cp, function(x){ x$lambda}))
    names(lambdas) <- NULL

    lambdas
} # END LAMBDA.GRID.sparsebnPath

#' @export
#' @describeIn get.adjacency.matrix Retrieves all \code{edges} slots in the solution path, converts to an adjacency matrix, and returns as a list
get.adjacency.matrix.sparsebnPath <- function(cp){
    lapply(cp, get.adjacency.matrix)
} # END GET.ADJACENCY.MATRIX.sparsebnPath
