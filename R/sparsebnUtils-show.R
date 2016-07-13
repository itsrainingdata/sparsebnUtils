#
#  sparsebnUtils-show.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 6/17/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Methods for showing / printing various object attributes
#
#   CONTENTS:
#       show.parents
#

#' Inspect subgraph
#'
#' Print out the edge list corresponding to a subset of nodes in a graph. Useful
#' for inspecting particular nodes of interest in a large graph. Out is indexed
#' by children, with the parents of each node listed to the right of each child.
#'
#' Uses partial matching, duplicates are OK and will be duplicated in output.
#'
#' @param x \code{\link{sparsebnFit}} object.
#' @param nodes \code{character} vector containing names of nodes to show.
#'
#' @export
show.parents <- function(x, nodes){
    stopifnot(is.sparsebnFit(x))

    ### Convert edgeList to plain list with desired data
    edges.str <- edgeList_to_node_names(x, 4)                   # convert edgeList to use node names
    match.nodes <- pmatch(nodes, x$nodes, duplicates.ok = TRUE) # find the nodes of interest

    check_match <- is.na(match.nodes)
    if(any(check_match)){
        stop(sprintf("No node(s) named %s found!", paste(nodes[check_match], collapse = ", ")))
    }

    out <- as.list(edges.str)[match.nodes]      # extract the parent lists of the nodes of interest
    names(out) <- x$nodes[match.nodes]          # add names attribute to list to print out node names as row names (see format_list)

    ### Print output
    cat(format_list(out))
}
