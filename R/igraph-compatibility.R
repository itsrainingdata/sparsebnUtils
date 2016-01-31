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

to_igraph <- function(x) UseMethod("to_igraph", x)

to_igraph.edgeList <- function(el){
    ### This function requires the 'igraph' package to be installed
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("igraph package required to coerce data to 'igraph' type!", call. = FALSE)
    }

    el.igraph <- edgeList_to_igraph_edgelist(el)

    igraph::graph_from_edgelist(el.igraph, directed = TRUE)
}

to_igraph.sparsebnFit <- function(sbf){
    sbf$edges <- to_igraph(sbf$edges)

    sbf
}

to_igraph.sparsebnPath <- function(sbp){
    lapply(sbp, to_igraph)
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
        el.igraph <- mapply(function(x, y){
            if(length(x) > 0) cbind(x, y)
        }, el, 1:num.nodes(el))
        el.igraph <- do.call("rbind", el.igraph)
    }

    ### Equivalent code: What the slick version does under the hood
    #     numnode <- num.nodes(el) # number of nodes in edgeList
    #     numedge <- num.edges(el) # number of edges in edgeList
    #
    #     el.igraph <- matrix(NA, ncol = 2, nrow = numedge)
    #     start <- 1
    #     for(j in 1:numnode){
    #         this.size <- length(el[[j]])
    #         end <- start + this.size - 1
    #         if(this.size > 0){
    #             el.igraph[start:end, ] <- cbind(el[[j]], j) # el[[j]] are the parents
    #             start <- end + 1
    #         }
    #     }

    el.igraph
}
