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
#     random.dag
#     random.psd
#

#' Generate a random DAG with fixed number of edges
#'
#' @param nnode Number of nodes in the DAG
#' @param nedge Number of edges in the DAG
#' @param call Optional function to be used as a random number generator
#'
#' @export
random.dag <- function(nnode, nedge, call = NULL){

    #
    # Works by randomly sampling elements of the lower triangular
    #  portion of a square matrix, and filling in these elements
    #  with random values
    #

    ### Initialize parameters
    m <- matrix(0, nrow = nnode, ncol = nnode)
    vals <- rep(0, nnode*(nnode-1)/2)

    ### randomly sample indices for nonzero coefs
    nonzero_coefs <- sample(seq_along(vals), size = nedge)

    ### randomly sample values for nonzero coefs
    if(is.null(call)){
        coefs <- stats::runif(nedge)
    } else{
        coefs <- replicate(nedge, call())
    }

    ### given these indices, update the values in m with random values
    ### Note that we are only changing the lower triangular portion
    vals[nonzero_coefs] <- coefs
    m[lower.tri(m)] <- vals

    ### shuffle the rows and columns
    shuffle <- sample(1:nnode)
    m <- m[shuffle, shuffle]

    ### Final output
    m
}

#' Generate a random positive definite matrix
#'
#' @param nnode Number of nodes in the matrix
#' @param eigenvalues Vector of eigenvalues desired in output
#' @param num.ortho Number of random Householder reflections to compose
#'
#' @export
random.psd <- function(nnode,
                       eigenvalues = NULL,
                       num.ortho = 10){
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
