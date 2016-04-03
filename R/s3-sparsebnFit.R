#
#  s3-sparsebnFit.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# sparsebnFit S3 Class for R
#------------------------------------------------------------------------------#

#
# sparsebnFit S3 class skeleton
#
# Data
# * edgeList edges          // edge list, adjacency matrix, or graphNEL object of DAG estimate
# * numeric lambda          // regularization parameter
# * integer nedge           // number of edges
# * integer pp              // number of nodes
# * integer nn              // number of observations
# * numeric time            // time to run CCDr algorithm
#
# Methods
# * is.sparsebnFit
# * sparsebnFit.list
# * as.list.sparsebnFit
# * print.sparsebnFit
# * get.adjacency.matrix
# * num.nodes.sparsebnFit
# * num.edges.sparsebnFit
# * num.samples.sparsebnFit
# * to_B.sparsebnFit
#

#' sparsebnFit class
#'
#' Main class for representing DAG estimates: Represents a single DAG estimate in the solution path.
#' Generally speaking, these estimates should be wrapped up in a \code{\link{sparsebnPath}} object, but
#' can be handled separately if desired (be careful!).
#'
#' @section Slots:
#' \describe{
#' \item{\code{edges}}{(edgeList) Edge list of estimated DAG (see \code{\link{edgeList}}).}
#' \item{\code{lambda}}{(numeric) Value of lambda for this estimate.}
#' \item{\code{nedge}}{(integer) Number of edges in this estimate.}
#' \item{\code{pp}}{(integer) Number of nodes.}
#' \item{\code{nn}}{(integer) Number of observations this estimate was based on.}
#' \item{\code{time}}{(numeric) Time in seconds to generate this estimate.}
#' }
#'
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}}
#' \code{\link{num.nodes}}, \code{\link{num.edges}}, \code{\link{num.samples}}
#'
#' @docType class
#' @name sparsebnFit
NULL

is.sparsebnFit <- function(cf){
    inherits(cf, "sparsebnFit")
} # END IS.sparsebnFit

# sparsebnFit constructor
sparsebnFit.list <- function(li){

    #
    # Need to be careful when using this constructor directly since it allows the nedge
    #  component to be different from the actual number of edges stored in the SBM object.
    #  This is allowed for efficiency reasons while running the main algorithm.
    #
    # UPDATE: An explicit check has been added for now.
    #

    if( !is.list(li)){
        stop("Input must be a list!")
    } else if( length(li) != 6 || !setequal(names(li), c("edges", "lambda", "nedge", "pp", "nn", "time"))){
        stop("Input is not coercable to an object of type sparsebnFit, check list for the following elements: edges (edgeList), lambda (numeric), nedge (integer), pp (integer), nn (integer), time (numeric or NA)")
    } else if( !is.edgeList(li$edges)){
        stop("'edges' component must be a valid edgeList object!")
    } else if(num.edges(li$edges) != li$nedge){
        stop("Attempting to set nedge to an improper value: Must be equal to the number of nonzero values in edges.")
    }

    ### Update values to be consistent with edgeList
    if(li$pp != num.nodes(li$edges)){
        stop("Attempting to create sparsebnFit object with inconsistent number of nodes! input = ", li$pp, " != output = ", num.nodes(li$edges))
    }
    li$pp <- num.nodes(li$edges)

    if(li$nedge != num.edges(li$edges)){
        stop("Attempting to create sparsebnFit object with inconsistent number of edges! input = ", li$nedge, " != output = ", num.edges(li$edges))
    }
    li$nedge <- num.edges(li$edges)

    ### Final output
    structure(li, class = "sparsebnFit")
} # END sparsebnFit.LIST

as.list.sparsebnFit <- function(cf){
    list(edges = cf$edges, lambda = cf$lambda, nedge = cf$nedge, pp = cf$pp, nn = cf$nn, time = cf$time)
} # END AS.LIST.sparsebnFit

print.sparsebnFit <- function(cf){
    MAX_NODES <- 20

    cat("CCDr estimate\n",
        cf$nn, " observations\n",
        "lambda = ", cf$lambda, "\n",
        sep = "")

    cat("\nDAG: \n")
    print(cf$edges)
    if(cf$pp < MAX_NODES) {
        # print(get.adjacency.matrix(cf))
    }
} # END PRINT.sparsebnFit

#' @export
#' @describeIn get.adjacency.matrix Retrieves \code{edges} slot and converts to an adjacency matrix
get.adjacency.matrix.sparsebnFit <- function(cf){
    get.adjacency.matrix.edgeList(cf$edges)
} # END GET.ADJACENCY.MATRIX.sparsebnFit

#' @export
#' @describeIn num.nodes Extracts the number of nodes of \link{sparsebnFit} object.
num.nodes.sparsebnFit <- function(cf){
    cf$pp
} # END NUM.NODES.sparsebnFit

#' @export
#' @describeIn num.edges Extracts the number of edges of \link{sparsebnFit} object.
num.edges.sparsebnFit <- function(cf){
    cf$nedge
} # END NUM.EDGES.sparsebnFit

#' @export
#' @describeIn num.samples Extracts the number of samples of \link{sparsebnFit} object.
num.samples.sparsebnFit <- function(cf){
    cf$nn
} # END NUM.SAMPLES.sparsebnFit

#------------------------------------------------------------------------------#
# to_B.sparsebnFit
# Internal function to convert estimates from the (Rho, R) parametrization to
#  the standard (B, Omega) parametrization.
#
# !!! 1-29-16: This function needs to be deprecated
#
# to_B.sparsebnFit <- function(cf){
#     .Deprecated()
#     cf$sbm <- to_B(cf$sbm)
#
#     cf
# }
