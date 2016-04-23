#
#  igraph-compatibility.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/30/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# Various utility functions for enforcing compatibility with the 'igraph' package.
#

#' Conversion to igraph object
#'
#' \code{igraph} converts an object to an \code{\link[igraph]{igraph}} object.
#' At a basic level, simply converts all associated \code{\link{edgeList}} objects to \code{igraph}.
#'
#' @param x An object of type \code{\link{edgeList}}, \code{\link{sparsebnFit}}, or \code{\link{sparsebnPath}}.
#'
#' @export
to_igraph <- function(x) UseMethod("to_igraph", x)

#' @export
to_igraph.igraph <- function(ig){
    ig
}

#' @export
to_igraph.edgeList <- function(el){
    ### This function requires the 'igraph' package to be installed
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("igraph package required to coerce data to 'igraph' type!", call. = FALSE)
    }

    if(num.edges(el) > 0){
        el.igraph <- edgeList_to_igraph_edgelist(el)
        igraph::graph_from_edgelist(el.igraph, directed = TRUE)
    } else{
        ### Special case to create empty graph
        igraph::graph.empty(n = num.nodes(el), directed = TRUE)
    }
}

#' @export
to_igraph.graphNEL <- function(gr){
    to_igraph(to_edgeList(gr))
}

#' @export
to_igraph.network <- function(net){
    to_igraph(to_edgeList(net))
}

#' @export
to_igraph.sparsebnFit <- function(sbf){
    sbf$edges <- to_igraph(sbf$edges)

    sbf
}

#' @export
to_igraph.sparsebnPath <- function(sbp){
    sparsebnPath(lapply(sbp, to_igraph))
}

#
# Helper function to convert an edge list to a igraph compatible edge list matrix
#  The output is a 2 column matrix where each row represents one edge:
#       [i j] = (i, j)
#
edgeList_to_igraph_edgelist <- function(el){
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
    # igraph:
    # 1 2
    # 1 3
    #
    #-------------------------------

    ### Slick version
    el.igraph <- matrix(NA, nrow=0, ncol=2) # base case for null graph
    if(num.edges(el) > 0){
        #
        # Bugfix: By default, SIMPLIFY is set to TRUE, which causes problem when
        #         do.call is called below ('Error in do.call("rbind", el.igraph) : second argument must be a list')
        #         Need to set SIMPLIFY = FALSE to prevent pre-mature matrixification.
        #
        el.igraph <- mapply(function(x, y){
            if(length(x) > 0) cbind(x, y)
        }, el, 1:num.nodes(el), SIMPLIFY = FALSE)

        el.igraph <- do.call("rbind", el.igraph)
    }

    ### Equivalent code: What the slick version does under the hood
#         numnode <- num.nodes(el) # number of nodes in edgeList
#         numedge <- num.edges(el) # number of edges in edgeList
#
#         el.igraph <- matrix(NA, ncol = 2, nrow = numedge)
#         start <- 1
#         for(j in 1:numnode){
#             this.size <- length(el[[j]])
#             end <- start + this.size - 1
#             if(this.size > 0){
#                 el.igraph[start:end, ] <- cbind(el[[j]], j) # el[[j]] are the parents
#                 start <- end + 1
#             }
#         }

    el.igraph
}

#' @export
to_edgeList.igraph <- function(igr){
    edgeList(igraph_to_edgeList_list(igr))
}

#
# Helper function to convert a igraph object to an edgeList compatible list
#
igraph_to_edgeList_list <- function(igr){
    igr.edgeL <- igraph::as_adj_list(igr, mode = "in")
    igr.edgeL <- lapply(igr.edgeL, as.integer)

    igr.edgeL
}
