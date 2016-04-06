#
#  graph-compatibility.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 7/26/15.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# Various utility functions for enforcing compatibility with the 'graph' package from BioConductor.
#

#' Conversion to graphNEL object
#'
#' \code{to_graphNEL} converts an object to a \code{\link[graph]{graphNEL}} object.
#' At a basic level, simply converts all associated \code{\link{edgeList}} objects to \code{graphNEL}.
#'
#' @param x An object of type \code{\link{edgeList}}, \code{\link{sparsebnFit}}, or \code{\link{sparsebnPath}}.
#'
#' @export
to_graphNEL <- function(x) UseMethod("to_graphNEL", x)

#' @export
to_graphNEL.edgeList <- function(el){
    ### This function require the 'graph' package to be installed
    if (!requireNamespace("graph", quietly = TRUE)) {
        stop("graph package (from BioConductor) required to coerce data to 'graphNEL' type!", call. = FALSE)
    }

    el.graphNEL <- edgeList_to_graphNEL_edgeL(el)
    names.graphNEL <- as.character(1:num.nodes(el))

    graph::graphNEL(nodes = names.graphNEL, edgeL = el.graphNEL, edgemode = 'directed')
}

#' @export
to_graphNEL.sparsebnFit <- function(sbf){
    sbf$edges <- to_graphNEL(sbf$edges)

    sbf
}

#' @export
to_graphNEL.sparsebnPath <- function(sbp){
    lapply(sbp, to_graphNEL)
}

#
# Helper function to convert an edge list to a graphNEL compatible edge list
#  The main difference is instead of listing parents for each node, graphNEL requires
#  listing children for each parent. There are also different naming and indexing conventions.
#
edgeList_to_graphNEL_edgeL <- function(el){
    #----------- EXAMPLE -----------
    # Default:
    # [[1]]
    # integer(0)
    #
    # [[2]]
    # [1] 1
    #
    # [[3]]
    # [1] 1
    #
    # graphNEL:
    # $`1`
    # $`1`$edges
    # [1] 2 3
    #
    #
    # $`2`
    # $`2`$edges
    # NULL
    #
    #
    # $`3`
    # $`3`$edges
    # NULL
    #
    #-------------------------------

    numnode <- num.nodes(el) # Number of nodes should be same as length of default edge list

    ### Invert the child-parent relationships (aka implicit transpose of adjacency matrix)
    el.graphNEL <- vector(mode = "list", length = numnode)
    for(j in 1:numnode){
        this.column <- el[[j]] # "column" = interpret as column in adj matrix
        for(i in seq_along(this.column)){
            el.graphNEL[[this.column[i]]] <- c(el.graphNEL[[this.column[i]]], j)
        }
    }

    ### Needs an extra component called "edges" (allows for possible specification of weights as well)
    el.graphNEL <- lapply(el.graphNEL, function(x){ list(edges = x)})

    ### List names MUST be character 1,...,numnode
    names(el.graphNEL) <- as.character(1:numnode)

    el.graphNEL
}
