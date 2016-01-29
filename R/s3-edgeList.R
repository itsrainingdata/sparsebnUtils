#
#  s3-edgeList.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 7/27/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#------------------------------------------------------------------------------#
# edgeList S3 Class for ccdr package
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
#' of this node in the graph. Only intended for internal use.
#'
#' Also inherits from \code{\link{list}}.
#'
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}},
#' \code{\link{num.nodes}}, \code{\link{num.edges}}
#'
#' @docType class
#' @name edgeList-class
NULL

#' @export
is.edgeList <- function(edgeL){
    inherits(edgeL, "edgeList")
} # END IS.EDGELIST

#' @export
edgeList.list <- function(li){
    ### Minimal consistency checks for this class
    if(!is.list(li)){
        stop("Input must be a list!")
    }

    structure(li, class = c("edgeList", "list"))
} # END EDGELIST.LIST

#' @export
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

#' @export
as.matrix.edgeList <- function(edgeL){
    as.matrix(get.adjacency.matrix.edgeList(edgeL))
}

#' @export
as.list.edgeList <- function(edgeL){
    class(edgeL) <- "list"
    edgeL
}

#' @export
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

#' @export
#' @describeIn num.nodes
num.nodes.edgeList <- function(edgeL){
    length(edgeL)
} # END NUM.NODES.EDGELIST

#' @export
#' @describeIn num.edges
num.edges.edgeList <- function(edgeL){
    sum(unlist(lapply(edgeL, length)))
} # END NUM.EDGES.EDGELIST

#' @export
#' @describeIn is.zero
is.zero.edgeList <- function(edgeL){
    (num.edges(edgeL) == 0)
} # END IS.ZERO.EDGELIST
