#
#  sparsebnUtils-bwlist.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 9/1/17.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Helper methods for constructing black/white lists
#
#   CONTENTS:
#     specify.prior
#     specify_root
#     specify_leaf
#

#' Build a black list based on prior knowledge
#'
#' Utility for specifying known root and leaf nodes in a network, to be used
#' in conjunction with the \code{blacklist} argument of network estimation
#' methods.
#'
#' Builds an (m+k)x2 matrix, where m is the number of user-specified root
#' nodes and k is the number of user-specified leaf nodes.
#'
#' \itemize{
#' \item A \emph{root} node is any node without any parents, i.e. with no
#' incoming edges.
#' \item A \emph{leaf} node is any node without any children, i.e. with no
#' outgoing edges.
#' }
#'
#' @param roots Vector of root nodes. May be character or integer.
#' @param leaves Vector of leaf nodes. May be character or integer.
#' @param nodes Full vector of node names of the entire network. Both
#' \code{roots} and \code{leaves} must be a subset of this vector.
#' @param indices Logical: Return indices or character names?
#'
#' @export
specify.prior <- function(roots, leaves, nodes, indices = FALSE){
    blacklist_root <- specify_root(roots, nodes, indices)
    blacklist_leaf <- specify_leaf(leaves, nodes, indices)

    rbind(blacklist_root, blacklist_leaf)
}

specify_root <- function(root, nodes, indices = FALSE){
    if(!all(root %in% nodes)){
        msg <- sprintf("The list of root nodes must be contained within the full list of nodes!")
        stop(msg)
    }

    lists <- lapply(root, function(r) t(sapply(nodes, function(x) c(x, r))))
    lists <- do.call("rbind", lists)

    if(!indices){
        lists
    } else{
        ### Use match to convert node names to indices
        matrix(match(lists, nodes), ncol = 2)
    }
} # END SPECIFY_ROOT

specify_leaf <- function(leaf, nodes, indices = FALSE){
    if(!all(leaf %in% nodes)){
        msg <- sprintf("The list of leaf nodes must be contained within the full list of nodes!")
        stop(msg)
    }

    lists <- lapply(leaf, function(r) t(sapply(nodes, function(x) c(r, x))))
    lists <- do.call("rbind", lists)

    if(!indices){
        lists
    } else{
        ### Use match to convert node names to indices
        matrix(match(lists, nodes), ncol = 2)
    }
} # END SPECIFY_LEAF
