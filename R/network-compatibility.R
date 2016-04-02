#
#  network-compatibility.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/30/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# Various utility functions for enforcing compatibility with the 'network' package (part of statnet).
#

#' Conversion to network object
#'
#' \code{igraph} converts an object to a \code{\link[network]{network}} object.
#' At a basic level, simply converts all associated \code{\link{edgeList}} objects to \code{network}.
#'
#' @param x An object of type \code{\link{edgeList}}, \code{\link{sparsebnFit}}, or \code{\link{sparsebnPath}}.
#'
to_network <- function(x) UseMethod("to_network", x)

to_network.edgeList <- function(el){
    ### This function requires the 'network' package to be installed
    if (!requireNamespace("network", quietly = TRUE)) {
        stop("network package required to coerce data to 'network' type!", call. = FALSE)
    }

    if(num.edges(el) > 0){
        el.network <- edgeList_to_network_edgelist(el)

        network::network(el.network, directed = TRUE)
    } else{
        #
        # Is there a way to intialize a null graph using the network constructor?
        #
        network::network.initialize(num.nodes(el)) # hack to work around special case of null graph
    }

}

to_network.sparsebnFit <- function(sbf){
    sbf$edges <- to_network(sbf$edges)

    sbf
}

to_network.sparsebnPath <- function(sbp){
    lapply(sbp, to_network)
}

#
# Helper function to convert an edge list to a network compatible edge list matrix
#  The output is a 2 column matrix where each row represents one edge:
#       [i j] = (i, j)
#
# NOTE: The required input is the _same_ as for igraph, so we just hijack that function.
#
edgeList_to_network_edgelist <- function(el){
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
    # network:
    # 1 2
    # 1 3
    #
    #-------------------------------

    edgeList_to_igraph_edgelist(el)
}
