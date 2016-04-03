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
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}},
#' \code{\link{num.nodes}}, \code{\link{num.edges}}
#'
#' @docType class
#' @name edgeList
NULL

#' @export
is.edgeList <- function(edgeL){
    inherits(edgeL, "edgeList")
} # END IS.EDGELIST

edgeList.list <- function(li){
    if(!is.list(li)){
        stop("Input must be a list!")
    }

    ### Don't allow length zero lists: What would that represent anyway?
    if(length(li) == 0){
        stop("Input must have at least one component!")
    }

    structure(li, class = c("edgeList", "list"))
} # END EDGELIST.LIST

print.edgeList <- function(edgeL){
    if(num.edges(edgeL) == 0){
        edgeL.out <- paste0("<Empty graph on ", num.nodes(edgeL), " nodes.>")
    } else{
        ### Assumes the DAG has at most 1000 nodes: Output will be cramped and illegible if the graph is larger than this
        ### We shouldn't be printing this when pp > 1000 anyway!
        edgeL.out <- mapply(function(x, y){
            prefix <- paste0("[", x, "]")
            prefix <- sprintf("%-5s", prefix)
            paste0(prefix, paste(sprintf("%4d", sort(y)), collapse = ""))
        }, 1L:length(edgeL), edgeL)
        edgeL.out <- unlist(edgeL.out)
        edgeL.out <- paste(edgeL.out, collapse = " \n")
    }

    cat("edgeList object\n", edgeL.out, "\n", sep = "")
}

as.matrix.edgeList <- function(edgeL){
    as.matrix(get.adjacency.matrix.edgeList(edgeL))
}

as.list.edgeList <- function(edgeL){
    class(edgeL) <- "list"
    edgeL
}

#' @describeIn get.adjacency.matrix Convert internal \code{edgeList} representation to an adjacency matrix
get.adjacency.matrix.edgeList <- function(edgeL){
    numnode <- length(edgeL)
    Matrix.out <- Matrix::Matrix(0, nrow = numnode, ncol = numnode)

    ### This loop is pretty slow!
    for(j in seq_along(edgeL)){
        for(i in edgeL[[j]]){
            Matrix.out[i, j] <- 1
        }
    }

    Matrix.out
} # END GET.ADJACENCY.MATRIX.EDGELIST

#' @describeIn num.nodes Extracts the number of nodes of \link{edgeList} object.
num.nodes.edgeList <- function(edgeL){
    length(edgeL)
} # END NUM.NODES.EDGELIST

#' @describeIn num.edges Extracts the number of edges of \link{edgeList} object.
num.edges.edgeList <- function(edgeL){
    sum(sapply(edgeL, length))
} # END NUM.EDGES.EDGELIST

#' @describeIn is.zero Determines whether or not the object represents a null graph with no edges.
is.zero.edgeList <- function(edgeL){
    (num.edges(edgeL) == 0)
} # END IS.ZERO.EDGELIST
