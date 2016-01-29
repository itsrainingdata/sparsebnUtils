#
#  s3-ccdrFit.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#------------------------------------------------------------------------------#
# ccdrFit S3 Class for R
#------------------------------------------------------------------------------#

#
# ccdrFit S3 class skeleton
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
# * is.ccdrFit
# * ccdrFit.list
# * as.list.ccdrFit
# * print.ccdrFit
# * get.adjacency.matrix
# * num.nodes.ccdrFit
# * num.edges.ccdrFit
# * num.samples.ccdrFit
# * to_B.ccdrFit
#

#' ccdrFit class
#'
#' Main class for representing DAG estimates: Represents a single DAG estimate in the solution path.
#' Generally speaking, these estimates should be wrapped up in a \code{\link{ccdrPath-class}} object, but
#' can be handled separately if desired (be careful!).
#'
#' @section Slots:
#' \describe{
#' \item{\code{edges}}{(edgeList) Edge list of estimated DAG (see \code{\link{edgeList-class}}).}
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
#' @name ccdrFit-class
NULL

#' @export
is.ccdrFit <- function(cf){
    inherits(cf, "ccdrFit")
} # END IS.CCDRFIT

# ccdrFit constructor
ccdrFit.list <- function(li){

    #
    # Need to be careful when using this constructor directly since it allows the nedge
    #  component to be different from the actual number of edges stored in the SBM object.
    #  This is allowed for efficiency reasons while running the main algorithm.
    #
    # UPDATE: An explicit check has been added for now.
    #

    if( !is.list(li)){
        stop("Input must be a list!")
    } else if( length(li) != 6 || !setequal(names(li), c("sbm", "lambda", "nedge", "pp", "nn", "time"))){
        stop("Input is not coercable to an object of type ccdrFit, check list for the following elements: sbm (SparseBlockMatrixR), lambda (numeric), nedge (integer), pp (integer), nn (integer), time (numeric or NA)")
    } else if( !is.SparseBlockMatrixR(li$sbm)){
        stop("'sbm' component must be a valid SparseBlockMatrixR object!")
    } else if(num.edges(li$sbm) != li$nedge){
        stop("Attempting to set nedge to an improper value: Must be equal to the number of nonzero values in sbm.")
    }

    #
    # Output DAG as an edge list (i.e. an edgeList object).
    #  This is NOT the same as sbm$rows since some of these rows may correspond to edges with zero coefficients.
    #  See docs for SpareBlockMatrixR class for details.
    #
    names(li)[1] <- "edges"
    li$edges <- as.edgeList.SparseBlockMatrixR(li$edges) # Before coercion, li$edges is actually an SBM object

    ### Update values to be consistent with edgeList
    if(li$pp != num.nodes(li$edges)){
        stop("Attempting to create ccdrFit object with inconsistent number of nodes! input = ", li$pp, " != output = ", num.nodes(li$edges))
    }
    li$pp <- num.nodes(li$edges)

    if(li$nedge != num.edges(li$edges)){
        stop("Attempting to create ccdrFit object with inconsistent number of edges! input = ", li$nedge, " != output = ", num.edges(li$edges))
    }
    li$nedge <- num.edges(li$edges)

    ### Final output
    structure(li, class = "ccdrFit")
} # END CCDRFIT.LIST

#' @export
as.list.ccdrFit <- function(cf){
    list(edges = cf$edges, lambda = cf$lambda, nedge = cf$nedge, pp = cf$pp, nn = cf$nn, time = cf$time)
} # END AS.LIST.CCDRFIT

#' @export
print.ccdrFit <- function(cf){
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
} # END PRINT.CCDRFIT

#' @export
#' @describeIn get.adjacency.matrix Retrieves \code{edges} slot and converts to an adjacency matrix
get.adjacency.matrix.ccdrFit <- function(cf){
    get.adjacency.matrix.edgeList(cf$edges)
} # END GET.ADJACENCY.MATRIX.CCDRFIT

#' @export
#' @describeIn num.nodes
num.nodes.ccdrFit <- function(cf){
    cf$pp
} # END NUM.NODES.CCDRFIT

#' @export
#' @describeIn num.edges
num.edges.ccdrFit <- function(cf){
    cf$nedge
} # END NUM.EDGES.CCDRFIT

#' @export
#' @describeIn num.samples
num.samples.ccdrFit <- function(cf){
    cf$nn
} # END NUM.SAMPLES.CCDRFIT

#------------------------------------------------------------------------------#
# to_B.ccdrFit
# Internal function to convert estimates from the (Rho, R) parametrization to
#  the standard (B, Omega) parametrization.
#
to_B.ccdrFit <- function(cf){
    cf$sbm <- to_B(cf$sbm)

    cf
}
