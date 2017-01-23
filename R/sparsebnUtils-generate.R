#
#  sparsebnUtils-generate.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 4/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Generate
#
#   CONTENTS:
#       random.dag
#       random.spd
#       random_householder
#       allBlocks
#

#' Generate random DAGs
#'
#' Generate a random graph with fixed number of edges.
#'
#' @param nnode Number of nodes in the graph.
#' @param nedge Number of edges in the graph.
#' @param acyclic If \code{TRUE}, output will be an acyclic graph.
#' @param loops If \code{TRUE}, output may include self-loops.
#'
#' @return An \code{\link[sparsebnUtils]{edgeList}} object containing a list of parents for each node.
#'
#' @export
random.graph <- function(nnode, nedge, acyclic = TRUE, loops = FALSE){

    max_nnz <- nnode*(nnode-1)/2
    if(nedge > max_nnz){
        stop(sprintf("A DAG with p = %d nodes can have at most p*(p-1)/2 = %d edges! Please check your input for nedge.", nnode, max_nnz))
    }

    #
    # Sample a random edgeList
    #
    # nnode <- 10
    # nedge <- nnode*(nnode-1)/2

    ### First use natural ordering 1,...,p
    node_order <- 1:nnode

    ### Get all pairs of off-diagonal indices in a pxp matrix
    indices <- allBlocks(node_order)

    ### Eliminate self-loops
    if(!loops){
        indices <- indices[indices[,1] != indices[,2], ]
    }

    ### If acyclic, select pairs in the lower triangular portion
    if(acyclic){
        indices <- indices[indices[,1] > indices[,2], ]
    }

    ### Randomly sample nedge of these pairs
    edges <- sample(1:nrow(indices), size = nedge)
    indices <- indices[edges, ]

    ### Convert from sparse representation to child-parent edge list
    edgeL <- lapply(1:nnode, function(x) unname(indices[indices[,2] == x, 1, drop = TRUE]))

    ### Name the cols/rows according to the current top sort
    ### This is useful since it gives quick access to a
    ###  top sort for the graph even after permuting
    names(edgeL) <- paste0("V", 1:nnode)


    ### Permute the nodes
    # 1) Get a random ordering
    # 2) Re-assign all parents to their new values (node_order[x])
    # 3) Permute the order of the nodes to match the new ordering
    #     using the inverse permutation of node_order (Matrix::invPerm(node_order))
    #
    node_order <- sample(1:nnode)
    edgeL <- lapply(edgeL, function(x) node_order[x])[Matrix::invPerm(node_order)]

    edgeList(edgeL)
}

### Generate a vector of parameters compatible with generate_mvn_data
gen_params <- function(graph, FUN = NULL, ...){
    nedge <- num.edges(graph)
    nnode <- num.nodes(graph)

    if(is.null(FUN)){
        coefs <- stats::runif(nedge)
        vars <- stats::runif(nnode)
    } else{
        FUN <- match.fun(FUN)
        coefs <- replicate(nedge, FUN(n = 1))
        vars <- replicate(nnode, FUN(n = 1))
    }

    c(coefs, vars)
}

#' Generate random DAGs
#'
#' Generate a random DAG with fixed number of edges.
#'
#' FUN can be any function whose first argument is called \code{n}. This
#' allows for both random and deterministic outputs.
#'
#' @param nnode Number of nodes in the DAG
#' @param nedge Number of edges in the DAG
#' @param FUN Optional function to be used as a random number generator
#' @param ... Additional arguments to \code{FUN}.
#'
#' @return An (weighted) adjacency matrix.
#'
#' @export
random.dag <- function(nnode, nedge, FUN = NULL, ...){

    graph <- random.graph(nnode, nedge, acyclic = TRUE)
    amat <- as.matrix(graph)

    ### Randomly sample values for nonzero coefs
    if(is.null(FUN)){
        coefs <- stats::runif(nedge)
    } else{
        FUN <- match.fun(FUN)
        coefs <- replicate(nedge, FUN(n = 1))
    }

    ### given these indices, update the values in amat with random values
    amat[amat!=0] <- coefs

    ### Final output
    amat
}

#' Generate a random positive definite matrix
#'
#' @param nnode Number of nodes in the matrix
#' @param eigenvalues Vector of eigenvalues desired in output. If this
#' has fewer than nnode values, the remainder are filled in as zero.
#' @param num.ortho Number of random Householder reflections to compose
#'
#' @export
random.spd <- function(nnode,
                       eigenvalues = NULL,
                       num.ortho = 10){
    stopifnot(nnode > 1)

    if(!is.null(eigenvalues)){
        len_eigenvalue <- length(eigenvalues)
        if(len_eigenvalue > nnode){
            stop(sprintf("A %dx%d matrix cannot have more than %d eigenvalues! Please check your input.", nnode, nnode, nnode))
        } else if(len_eigenvalue < nnode){
            num_zero <- nnode - len_eigenvalue
            eigenvalues <- c(eigenvalues, rep(0, num_zero))
        }
    }

    ortho_matrices <- lapply(1:num.ortho, function(x) random_householder(nnode))
    Q <- Reduce("%*%", ortho_matrices)
    if(is.null(eigenvalues)) eigenvalues <- stats::runif(nnode)
    m <- Q %*% diag(eigenvalues) %*% t(Q)

    m
}

random_householder <- function(nnode){
    v <- stats::rnorm(nnode)
    v <- v / sqrt(sum(v^2))
    householder <- diag(rep(1, nnode)) - 2 * v %*% t(v)
    householder
}

allBlocks <- function(nodes){
    blocks <- lapply(nodes, function(x){
            row <- (nodes)[nodes != x]
            col <- rep(x, length(col))
            cbind(row, col)
        })
    blocks <- do.call("rbind", blocks)

    blocks
}
