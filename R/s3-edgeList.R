#
#  s3-edgeList.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# edgeList S3 Class for R
#------------------------------------------------------------------------------#

#
# edgeList S3 class skeleton
#
# Data
# * <wrapper for a list>
#
# Methods
# * is.edgeList
# * edgeList.list
# * print.edgeList
# * as.matrix.edgeList
# * as.list.edgeList
# * get.adjacency.matrix.edgeList
# * num.nodes.edgeList
# * num.edges.edgeList
# * is.zero.edgeList
#

#' edgeList class
#'
#' Convenience wrapper class for a (column-major) edge list. Each component of the list
#' corresponds to a node, and each component is an integer vector whose components are the parents
#' of this node in the graph.
#'
#' Also inherits from \code{\link{list}}.
#'
#' @param x A list containing parents for each node in a graph. The length of this list
#'          should be the same as the number of nodes in the graph.
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}},
#' \code{\link{num.nodes}}, \code{\link{num.edges}}
#'
#' @docType class
#' @name edgeList
NULL

#' @rdname edgeList
#' @export
is.edgeList <- function(x){
    inherits(x, "edgeList")
} # END IS.EDGELIST

#' @export
edgeList.list <- function(x){
    if(!is.list(x)){
        stop("Input must be a list!")
    }

    ### Don't allow length zero lists: What would that represent anyway?
    if(length(x) == 0){
        stop("Input must have at least one component!")
    }

    ### Don't allow NULL components: Empty parent sets should be integer(0)
    if(check_null(x)){
        stop("Input cannot have any NULL values! An empty parent set should be indicated by 'integer(0)', not NULL.")
    }

    ### Don't allow missing values
    if(check_na(x)){
        stop("Input cannot have missing values! An empty parent set should be indicated by 'integer(0)', not NA")
    }

    ### Must only contain numbers
    if(!check_list_numeric(x)){
        stop("Invalid input detected: List should contain only integer / numeric vectors with no missing or NULL values.")
    } else{
        x <- lapply(x, as.integer) # convert all entries to integer for consistency
    }

    ### Cannot assign a parent larger than total number of nodes, or < 1
    max.node.index <- suppressWarnings(max(unlist(x))) # Ignore warning if graph is empty
    min.node.index <- suppressWarnings(min(unlist(x))) #
    if(max.node.index > length(x) || min.node.index < 1){
        stop(sprintf("The indices of the parents must be between 1 and number of nodes (= length of list)!
                      Currently between %d and %d.", min.node.index, max.node.index))
    }

    structure(x, class = c("edgeList", "list"))
} # END EDGELIST.LIST

#' @method print edgeList
#' @export
print.edgeList <- function(x, maxsize = 20, ...){
    # if(num.edges(x) == 0){
    #     edgeL.out <- sprintf("<Empty graph on %d nodes.>", num.nodes(x))
    # } else if(num.nodes(x) <= maxsize){
    #     ### Assumes the DAG has at most 1000 nodes: Output will be cramped and illegible if the graph is larger than this
    #     ### We shouldn't be printing this when pp > 1000 anyway!
    #     edgeL.out <- mapply(function(x, y){
    #         prefix <- paste0("[", x, "]")
    #         prefix <- sprintf("%-5s", prefix)
    #         paste0(prefix, paste(sprintf("%4d", sort(y)), collapse = ""))
    #     }, 1L:length(x), x)
    #     edgeL.out <- unlist(edgeL.out)
    #     edgeL.out <- paste(edgeL.out, collapse = " \n")
    # } else{
    #     edgeL.out <- sprintf("Directed graph with %d nodes and %d edges.", num.nodes(x), num.edges(x))
    # }
    edgeL.out <- .str_edgeList(x, maxsize = maxsize)

    cat("edgeList object\n", edgeL.out, "\n", sep = "")
}

### Internal method to return (as a string) the screen output of an edgeList
### Mainly useful for allow print.sparsebnFit to print out node names instead of numbers
.str_edgeList <- function(x, maxsize, ...){
    # if(length(nodes) != length(x)){
    #     stop(sprintf("Length of nodes must equal the length of x! length(nodes) = %d != %d = length(x)", length(nodes), length(x)))
    # }

    ### Can't use num.nodes or num.edges since x may not be an edgeList
    num_nodes_x <- length(x)
    num_edges_x <- sum(sapply(x, length))

    if(num_edges_x == 0){
        edgeL.out <- empty_dag_summary(length(x))
    } else if(num_nodes_x <= maxsize){
        # ### Assumes the DAG has at most 1000 nodes: Output will be cramped and illegible if the graph is larger than this
        # ### We shouldn't be printing this when pp > 1000 anyway!
        # edgeL.out <- mapply(function(x, y){
        #     prefix <- paste0("[", x, "]")
        #     prefix <- sprintf("%-7s", prefix)
        #     paste0(prefix, paste(sprintf("%5s", sort(y)), collapse = ""))
        # }, nodes, x)
        # edgeL.out <- unlist(edgeL.out)
        # edgeL.out <- paste(edgeL.out, collapse = " \n")
        edgeL.out <- format_list(as.list(x))
    } else{
        edgeL.out <- dag_summary(num_nodes_x, num_edges_x)
    }

    edgeL.out
}

#' @export
as.matrix.edgeList <- function(x, ...){
    as.matrix(get.adjacency.matrix.edgeList(x))
}

#' @export
as.list.edgeList <- function(x, ...){
    class(x) <- "list"
    x
}

#' @describeIn get.adjacency.matrix Convert internal \code{edgeList} representation to an adjacency matrix
#' @export
get.adjacency.matrix.edgeList <- function(x){
    numnode <- length(x)
    Matrix.out <- Matrix::Matrix(0, nrow = numnode, ncol = numnode)

    ### This loop is pretty slow!
    for(j in seq_along(x)){
        for(i in x[[j]]){
            Matrix.out[i, j] <- 1
        }
    }

    Matrix.out
} # END GET.ADJACENCY.MATRIX.EDGELIST

#' @describeIn num.nodes Extracts the number of nodes of \link{edgeList} object.
#' @export
num.nodes.edgeList <- function(x){
    length(x)
} # END NUM.NODES.EDGELIST

#' @describeIn num.edges Extracts the number of edges of \link{edgeList} object.
#' @export
num.edges.edgeList <- function(x){
    sum(sapply(x, length))
} # END NUM.EDGES.EDGELIST

#' @describeIn is.zero Determines whether or not the object represents a null graph with no edges.
#' @export
is.zero.edgeList <- function(x){
    (num.edges(x) == 0)
} # END IS.ZERO.EDGELIST

#' Plot a fitted Bayesian network object
#'
#' Plots the graph object associated with the output of a learning algorithm.
#'
#' \code{plot.sparsebnFit} uses some default settings to make large graphs
#' easier to interpret, but these settings can be over-ridden.
#'
#' @param x fitted object to plot.
#' @param ... (optional) additional arguments to plotting mechanism.
#'
#' @seealso \code{\link{setPlotPackage}}, \code{\link{getPlotPackage}}
#'
#' @method plot edgeList
#' @export
plot.edgeList <- function(x, ...){
    ### Set plotting parameters (Don't use no.readonly = TRUE! See https://stat.ethz.ch/pipermail/r-help/2007-July/136770.html)
    par.default <- par()["mai"] # Only re-set what we change here
    par(mai = rep(0.1,4))       # Need to reset margins (why??? graph packages seem to handle this oddly)

    pkg_plot <- getPlotPackage()

    if(!is.null(pkg_plot)){
        if(pkg_plot == "graph"){
            graph::plot(to_graphNEL(x), ...)
        } else if(pkg_plot == "igraph"){
            plot(to_igraph(x), ...)
        } else if(pkg_plot == "network"){
            plot(to_network(x), ...)
        } else{
            stop("Incorrect package specified. Must be one of: 'graph', 'igraph', 'network'.")
        }
    } else{
        stop("No package specified for plotting! This is an internal error and should not happen -- please report this issue.")
    }

    par(par.default) # restore user's original settings
}

#' @export
to_edgeList.edgeList <- function(x){
    x
}

#
# Convert a standard two-column edge list to an edgeList compatible list
#
edgelist_mat_to_edgeList_list <- function(x, numnode){

    edgeL <- lapply(vector("list", length = numnode), as.integer)
    if(nrow(x) > 0){ # If no edges, simply return list full of integer(0)'s
        for(j in 1:nrow(x)){
            ### NOTE: Fix this to be memory-efficient for large graphs
            edgeL[[x[j, 2]]] <- c(edgeL[[x[j, 2]]], x[j, 1])
        }
    }

    edgeL
}

