#
#  s3-edgeList.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
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
#' @param ... (optional) additional arguments.
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

#' as.edgeList
#'
#' Methods for coercing other \code{R} objects to \code{\link{edgeList}} objects.
#'
#' @param x A compatible \code{R} object.
#'
#' @return
#' \code{\link{edgeList}}
#'
#' @export
as.edgeList <- function(x){
    edgeList(x) # NOTE: S3 delegation is implicitly handled by the constructor here
}

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

#' @export
edgeList.sparse <- function(x, ...){
    stopifnot(x$dim[1] == x$dim[2])

    out <- lapply(vector("list", length = x$dim[1]), as.integer) # need as.integer to convert NULLs into integer(0)
    for(j in seq_along(x$cols)){
        child <- x$cols[[j]]
        parent <- x$rows[[j]]
        out[[child]] <- c(out[[child]], parent) # !!! THIS IS SLOW
    }

    edgeList(out)
}

#' @export
edgeList.matrix <- function(x){
    matrix_to_edgeList(x)
}

#' @export
edgeList.Matrix <- function(x){
    matrix_to_edgeList(x)
}

#' @export
edgeList.graphNEL <- function(x){
    to_edgeList(x)
}

#' @export
edgeList.network <- function(x){
    to_edgeList(x)
}

#' @export
edgeList.igraph <- function(x){
    to_edgeList(x)
}

#' @export
edgeList.bn <- function(x){
    to_edgeList(x)
}

### Internal method to return (as a string) the screen output of an edgeList
### Mainly useful for allow print.sparsebnFit to print out node names instead of numbers
.str_edgeList <- function(x, maxsize, ...){
    ### Can't use num.nodes or num.edges since x may not be an edgeList
    num_nodes_x <- length(x)
    num_edges_x <- sum(sapply(x, length))

    if(num_edges_x == 0){
        edgeL.out <- empty_dag_summary(length(x))
    } else if(num_nodes_x <= maxsize){
        edgeL.out <- format_list(as.list(x))
    } else{
        edgeL.out <- dag_summary(num_nodes_x, num_edges_x)
    }

    edgeL.out
} # END .STR_EDGELIST

#' @param maxsize Maximum number of nodes to print out. If
#' \code{num.nodes(x) > maxsize}, then a simple summary will be printed
#' instead.
#'
#' @rdname edgeList
#' @method print edgeList
#' @export
print.edgeList <- function(x, maxsize = 20, ...){
    edgeL.out <- .str_edgeList(x, maxsize = maxsize)

    cat("edgeList object\n", edgeL.out, "\n", sep = "")
} # END PRINT.EDGELIST

#' @param object an object of type \code{edgeList}
#'
#' @rdname edgeList
#' @method summary edgeList
#' @export
summary.edgeList <- function(object, ...){
    cat("edgeList object\n",
        " ", num.nodes(object), " nodes\n",
        " ", num.edges(object), " directed edges\n",
        "\nNodewise summary statistics:\n",
        sep = "")
    print(degrees(object))
} # END SUMMARY.SPARSEBNFIT

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
} # END PLOT.EDGELIST

#' @export
as.matrix.edgeList <- function(x, ...){
    as.matrix(get.adjacency.matrix.edgeList(x))
} # END AS.MATRIX.EDGELIST

#' @export
as.list.edgeList <- function(x, ...){
    class(x) <- "list"
    x
} # END AS.LIST.EDGELIST

#' @describeIn get.adjacency.matrix Convert internal \code{edgeList} representation to an adjacency matrix
#' @export
get.adjacency.matrix.edgeList <- function(x){
    numnode <- length(x)
    Matrix.out <- Matrix::Matrix(0, nrow = numnode, ncol = numnode)

    ### Set column and row names for output
    node_names <- names(x)
    colnames(Matrix.out) <- rownames(Matrix.out) <- node_names

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

#' Degree distribution of a graph
#'
#' Returns a \code{data.frame} with summary statistics on the total degree,
#' in-degree, and out-degree of each node in the network.
#'
#' @param x an \code{edgeList} object
#' @export
degrees <- function(x){
    stopifnot(is.edgeList(x))

    indegrees <- sapply(x, length)
    outdegrees <- tabulate(unlist(x))
    names(indegrees) <- names(outdegrees) <- names(x)
    degrees <- indegrees + outdegrees

    out <- data.frame(node = names(x), indegree = indegrees, outdegree = outdegrees, degree = degrees)
    rownames(out) <- NULL
    if(sum(out$indegree) != sum(out$outdegree)){
        stop(sprintf("There was an issue computing the degrees,
                     total indegree = %d != total outdegree = %d.
                     Please report this to the maintainer.",
                     sum(out$indegree), sum(out$outdegree)))
    }

    out
}

#' @describeIn is.zero Determines whether or not the object represents a null graph with no edges.
#' @export
is.zero.edgeList <- function(x){
    (num.edges(x) == 0)
} # END IS.ZERO.EDGELIST

#' Permute the order of nodes in a graph
#'
#' Randomize the order of the nodes in a graph.
#'
#' Useful for obfuscating the
#' topological sort in a DAG, which is often the default output of methods
#' that generate a random DAG. Output is graph isomorphic to input.
#'
#' @param x Graph as \code{\link{edgeList}} object.
#' @param perm Permutation to use.
#' @return Permuted graph as \code{\link{edgeList}} object.
#'
#' @export
permute.nodes <- function(x, perm = NULL){
    stopifnot(is.edgeList(x))

    ### Permute the nodes
    # 1) Get a random ordering
    # 2) Re-assign all parents to their new values (node_order[x])
    # 3) Permute the order of the nodes to match the new ordering
    #     using the inverse permutation of node_order (Matrix::invPerm(node_order))
    #
    if(is.null(perm)){
        node_order <- sample(1:num.nodes(x))
    } else{
        stopifnot(length(perm) == num.nodes(x))
        stopifnot(sort(perm) == 1:num.nodes(x))
        node_order <- perm
    }
    permuted <- lapply(x, function(x) node_order[x])[Matrix::invPerm(node_order)]

    edgeList(permuted)
}

#' @export
to_edgeList.edgeList <- function(x){
    x
} # END TO_EDGELIST.EDGELIST

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
} # END EDGELIST_MAT_TO_EDGELIST_LIST

matrix_to_edgeList <- function(x){
    stopifnot(check_if_matrix(x))

    out <- edgeList(as.sparse(x))

    if(!all(colnames(x) == rownames(x))){
        warning("Row names do not match column names! Defaulting to use column names only.")
    }

    if(!is.null(colnames(x))){
        names(out) <- colnames(x)
    }

    out
} # END MATRIX_TO_EDGELIST
