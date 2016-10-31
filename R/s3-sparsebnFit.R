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
#' Main class for representing DAG estimates. Represents a single DAG estimate in a solution path.
#'
#' This is the main class for storing and manipulating the output of \code{\link{estimate.dag}}.
#' The main slot of interest is \code{edges}, which stores the graph as an \code{\link{edgeList}}
#' object. If desired, this slot can be changed to hold a \code{\link[graph]{graphNEL}},
#' \code{\link[igraph]{igraph}}, or \code{\link[network]{network}} object if desired (see
#' \code{\link{setGraphPackage}}). For anything beyond simply inspecting the graph, it is recommended
#' to use one of these packages.
#'
#' Since \code{edgeList}s do not contain information on the node names, the second slot
#' \code{nodes} stores this information. The indices in \code{edges} are in one-to-one
#' correspondence with the names in the \code{nodes} vector. The \code{lambda} slot stores
#'  the regularization parameter used to estinate the graph.
#'
#' Other slots include \code{nedge}, for the number of edges; \code{pp}, for p = number of nodes;
#' \code{nn}, for n = number of samples, and \code{time}, for the time in seconds needed to
#' estimate this graph. Note that these slots are mainly for internal use, and in particular
#' it is best to query the number of nodes via \code{\link{num.nodes}}, the number of edges
#' via \code{\link{num.edges}}, and the number of samples via \code{\link{num.samples}}.
#'
#' By default, only small graphs are printed, but this behaviour can be overridden via the
#' \code{maxsize} argument to \code{print}. To view a list of parents for a specific subset of
#' nodes, use \code{\link{show.parents}}.
#'
#' Generally speaking, it should not be necessary to construct a \code{sparsebnFit} object
#' manually. Furthermore, these estimates should always be wrapped up in a \code{\link{sparsebnPath}}
#' object, but can be handled separately if desired (be careful!).
#'
#' @param x An \code{R} object.
#' @param maxsize If the number of nodes in a graph is \eqn{\le} \code{maxsize}, then the entire
#' graph is printed to screen, otherwise a short summary is displayed instead.
#' @param ... (optional) additional arguments.
#'
#' @section Slots:
#' \describe{
#' \item{\code{edges}}{(\code{\link{edgeList}}) Edge list of estimated DAG (see \code{\link{edgeList}}).}
#' \item{\code{nodes}}{(\code{\link{character}}) Vector of node names.}
#' \item{\code{lambda}}{(\code{\link{numeric}}) Value of lambda for this estimate.}
#' \item{\code{nedge}}{(\code{\link{integer}}) Number of edges in this estimate.}
#' \item{\code{pp}}{(\code{\link{integer}}) Number of nodes.}
#' \item{\code{nn}}{(\code{\link{integer}}) Number of observations this estimate was based on.}
#' \item{\code{time}}{(\code{\link{numeric}}) Time in seconds to generate this estimate.}
#' }
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}},
#' \code{\link{num.nodes}},
#' \code{\link{num.edges}},
#' \code{\link{num.samples}},
#' \code{\link{show.parents}}
#'
#' @examples
#'
#' \dontrun{
#' ### Learn the cytometry network
#' data(cytometryContinuous)
#' cyto.data <- sparsebnData(cytometryContinuous[["data"]], type = "continuous")
#' cyto.learn <- estimate.dag(cyto.data)
#'
#' ### Inspect the output
#' class(cyto.learn[[1]])
#' print(cyto.learn[[2]])
#' show.parents(cyto.learn[[1]], c("raf", "mek", "plc"))
#'
#' ### Manipulate a particular graph
#' cyto.fit <- cyto.learn[[7]]
#' num.nodes(cyto.fit)
#' num.edges(cyto.fit)
#' show.parents(cyto.fit, c("raf", "mek", "plc"))
#' plot(cyto.fit)
#'
#' ### Use graph package instead of edgeLists
#' setGraphPackage("graph", coerce = TRUE) # set sparsebn to use graph package
#' cyto.edges <- cyto.fit$edges
#' degree(cyto.edges)       # only available with graph package
#' isConnected(cyto.edges)  # only available with graph package
#' }
#'
#' @docType class
#' @name sparsebnFit
NULL

#' @rdname sparsebnFit
#' @export
is.sparsebnFit <- function(x){
    inherits(x, "sparsebnFit")
} # END IS.SPARSEBNFIT

# sparsebnFit constructor
#' @method sparsebnFit list
#' @export
sparsebnFit.list <- function(x){

    #
    # Need to be careful when using this constructor directly since it allows the nedge
    #  component to be different from the actual number of edges stored in the SBM object.
    #  This is allowed for efficiency reasons while running the main algorithm.
    #
    # UPDATE: An explicit check has been added for now.
    #

    if( !is.list(x)){
        stop("Input must be a list!")
    } else if( length(x) != 7 || !setequal(names(x), c("edges", "nodes", "lambda", "nedge", "pp", "nn", "time"))){
        stop("Input is not coercable to an object of type sparsebnFit, check list for the following elements: edges (edgeList), nodes (character), lambda (numeric), nedge (integer), pp (integer), nn (integer), time (numeric or NA)")
    } else if( !is.edgeList(x$edges)){
        stop("'edges' component must be a valid edgeList object!")
    } else if(num.edges(x$edges) != x$nedge){
        stop("Attempting to set nedge to an improper value: Must be equal to the number of nonzero values in edges.")
    }

    ### Check dimensions of names
    if(!is.null(x$nodes) && length(x$nodes) != x$pp){
        stop(sprintf("Length of 'nodes' must equal 'pp'! length(nodes) = %d != %d = pp", length(x$nodes), x$pp))
    }

    ### Update values to be consistent with edgeList
    if(x$pp != num.nodes(x$edges)){
        stop("Attempting to create sparsebnFit object with inconsistent number of nodes! input = ", x$pp, " != output = ", num.nodes(x$edges))
    }
    x$pp <- num.nodes(x$edges)

    if(x$nedge != num.edges(x$edges)){
        stop("Attempting to create sparsebnFit object with inconsistent number of edges! input = ", x$nedge, " != output = ", num.edges(x$edges))
    }
    x$nedge <- num.edges(x$edges)

    ### Output with DAG as edgeList
    out <- structure(x, class = "sparsebnFit")

    ### Coerce to user's desired data structure
    pkg_graph <- getGraphPackage()
    if(!is.null(pkg_graph)){
        if(pkg_graph == "graph"){
            out <- to_graphNEL(out)
        } else if(pkg_graph == "igraph"){
            out <- to_igraph(out)
        } else if(pkg_graph == "network"){
            out <- to_network(out)
        } else{
            stop(invalid_pkg_specification())
        }
    }

    out
} # END SPARSEBNFIT.LIST

#' @method as.list sparsebnFit
#' @export
as.list.sparsebnFit <- function(x, ...){
    list(edges = x$edges, nodes = x$nodes, lambda = x$lambda, nedge = x$nedge, pp = x$pp, nn = x$nn, time = x$time)
} # END AS.LIST.SPARSEBNFIT

#' @rdname sparsebnFit
#' @method print sparsebnFit
#' @export
print.sparsebnFit <- function(x, maxsize = 20, ...){
    ### Print pre-amble
    cat("CCDr estimate\n",
        x$nn, " observations\n",
        "lambda = ", x$lambda, "\n",
        sep = "")

    cat("\nDAG: \n")

    ### Truncate node names, convert edge list to reference names instead of indices, generate output
    # node_names_trunc <- substr(x$nodes, 1, 4)
    # edgeL_names <- lapply(as.list(x$edges), function(z) node_names_trunc[z])

    if(is.edgeList(x$edges)){
        edgeL_names <- edgeList_to_node_names(x, 4)
        edgeL.out <- .str_edgeList(edgeL_names, maxsize = maxsize)

        ### Print DAG output
        cat(edgeL.out, "\n", sep = "")
    } else{
        ### Use default print method for whichever data structure user has selected
        print(x$edges)
    }

} # END PRINT.SPARSEBNFIT

#' @describeIn get.adjacency.matrix Retrieves \code{edges} slot and converts to an adjacency matrix
#' @export
get.adjacency.matrix.sparsebnFit <- function(x){
    adj <- get.adjacency.matrix(to_edgeList(x$edges))
    colnames(adj) <- rownames(adj) <- x$nodes

    adj
} # END GET.ADJACENCY.MATRIX.SPARSEBNFIT

#' @describeIn get.nodes Returns the node names from a \code{\link{sparsebnFit}} object.
#' @export
get.nodes.sparsebnFit <- function(x){
    x$nodes
} # END GET.NODES.SPARSEBNFIT

#' @describeIn num.nodes Extracts the number of nodes of \link{sparsebnFit} object.
#' @export
num.nodes.sparsebnFit <- function(x){
    x$pp
} # END NUM.NODES.SPARSEBNFIT

#' @describeIn num.edges Extracts the number of edges of \link{sparsebnFit} object.
#' @export
num.edges.sparsebnFit <- function(x){
    x$nedge
} # END NUM.EDGES.SPARSEBNFIT

#' @describeIn num.samples Extracts the number of samples of \link{sparsebnFit} object.
#' @export
num.samples.sparsebnFit <- function(x){
    x$nn
} # END NUM.SAMPLES.SPARSEBNFIT

#' @rdname plot.edgeList
#' @method plot sparsebnFit
#' @export
plot.sparsebnFit <- function(x, ...){
    plot(x$edges, ...)

    # pkg_plot <- getPlotPackage()
    #
    # if(!is.null(pkg_plot)){
    #     if(pkg_plot == "igraph"){
    #         ### Over-ride defaults for igraph.plot
    #         circle_layout <- igraph::layout.circle(to_igraph(x$edges))
    #         plot(x$edges,
    #              layout = circle_layout,
    #              vertex.label.color = gray(0),
    #              vertex.color = gray(0.9),
    #              edge.color = gray(0),
    #              ...)
    #     } else{
    #         plot(x$edges, ...)
    #     }
    # }
}

#' @describeIn to_edgeList description
#' @export
to_edgeList.sparsebnFit <- function(x){
    x$edges <- to_edgeList(x$edges)

    x
} # END TO_EDGELIST.SPARSEBNFIT

# convert an edgeList to a character list containing node names (vs indices)
edgeList_to_node_names <- function(x, trunc_level = 4){
    stopifnot(is.sparsebnFit(x))

    node_names_trunc <- substr(x$nodes, 1, trunc_level)
    out <- lapply(as.list(x$edges), function(z) node_names_trunc[z])
    names(out) <- node_names_trunc

    out
} # END EDGELIST_TO_NODE_NAMES
