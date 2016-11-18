#
#  bnlearn-compatibility.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 3/18/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# Various utility functions for enforcing compatibility with the 'bnlearn' package.
#

#' Conversion to bnlearn object
#'
#' \code{to_bn} converts an object to a \code{\link[bnlearn]{bn-class}} object. Works on both fitted
#' objects and graphs themselves. In the first case, every underlying 'edges' component is converted to
#' \code{\link[bnlearn]{bn-class}}. In the second, the conversion applies directly to the object.
#'
#' @param x An object of type \code{\link{sparsebnPath}}, \code{\link{sparsebnFit}}, \code{\link{edgeList}},
#' \code{\link[bnlearn]{bn-class}}, \code{\link[igraph]{igraph}}, or \code{\link[network]{network}}.
#'
#' @export
to_bn <- function(x){
    ### This function require the 'bnlearn' package to be installed
    if (!requireNamespace("bnlearn", quietly = TRUE)) {
        stop("bnlearn package required to coerce data to 'bn' type!", call. = FALSE)
    }

    UseMethod("to_bn", x)
}

#' @export
to_bn.bn <- function(x){
    x
}


#' @export
to_bn.graphNEL <- function(x){
    bnlearn::as.bn(x)
}

#' @export
to_bn.edgeList <- function(x){
    to_bn(to_graphNEL(x))
}

#' @export
to_bn.igraph <- function(x){
    to_bn(to_graphNEL(x))
}

#' @export
to_bn.network <- function(x){
    to_bn(to_graphNEL(x))
}

#' @export
to_bn.sparsebnFit <- function(x){
    x$edges <- to_bn(x$edges)

    x
}

#' @export
to_bn.sparsebnPath <- function(x){
    sparsebnPath(lapply(x, to_bn))
}

#' @export
to_edgeList.bn <- function(x){
    edgeList(bn_to_edgeList_list(x))
}

#
# Helper function to convert a graphNEL object to an edgeList compatible list
#
bn_to_edgeList_list <- function(bn){
    ### This is a slicker version of this function that we might consider switching to in the future
    # lapply(bn$nodes, function(x) as.integer(x$parents))

    bn.numnode <- bnlearn::nnodes(bn)
    bn.nedge <- bnlearn::narcs(bn)

    #
    # The edge list of a bn object is stored as bn$arcs. This list consists
    #  of character names for each node, so use as.integer to fix this. Once
    #  this is done, the resulting 2-column matrix is the same as the output
    #  of as.edgelist from the network package, so we can simply use
    #
    if(bn.nedge > 0){
        bn.edgelist <- apply(bn$arcs, 2, as.integer) # convert char to int

        ### Beware drop = FALSE (or lack thereof) bug in apply
        if(!is.matrix(bn.edgelist)) bn.edgelist <- matrix(bn.edgelist, nrow = 1)

        edgelist_mat_to_edgeList_list(bn.edgelist, bn.numnode)
    } else{
        ### Need a special case for nedge == 0 since otherwise R coerces the output of apply to an empty vector
        edgelist_mat_to_edgeList_list(bn$arcs, bn.numnode)
    }
}
