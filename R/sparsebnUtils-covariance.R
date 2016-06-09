#
#  sparsebnUtils-covariance.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 6/7/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Methods for estimating covariance / precision matrices
#
#   CONTENTS:
#       get.covariance
#       estimate.covariance
#       cov_mat
#       get.precision
#       estimate.precision
#       inv_cov_mat
#

#' Covariance estimation
#'
#' Methods for inferring (i) Covariance matrices and (ii) Precision matrices for continuous,
#' Gaussian data.
#'
#' For Gaussian data, the precision matrix corresponds to an undirected graphical model for the
#' distribution. This undirected graph can be tied to the corresponding directed graphical model;
#' see Sections 2.1 and 2.2 (equation (6)) of Aragam and Zhou (2015) for more details.
#'
#' @param data data as \code{\link{sparsebnData}} object.
#' @param x fitted \code{\link{sparsebnFit}} or \code{\link{sparsebnPath}} object.
#' @param ... (optional) additional parameters to \code{\link[sparsebn]{estimate.dag}}
#'
#' @return
#' Solution path as a plain \code{\link{list}}. Each component is a \code{\link[Matrix]{Matrix}}
#' corresponding to an estimate of the covariance or precision (inverse covariance) matrix for a
#' given value of lambda.
#'
#' @name estimate.covariance
#' @rdname estimate.covariance
NULL

### Covariance fitting --------------------------------------------------------
#' @export
estimate.covariance.sparsebnData <- function(data, ...){
    estimated.dags <- sparsebn::estimate.dag(data, ...)
    get.covariance(estimated.dags, data)
}

#' @export
get.covariance.sparsebnFit <- function(x, data, ...){
    fitted.dag <- estimate.parameters(x, data)
    cov_mat(Matrix::Matrix(fitted.dag$coefs), Matrix::Matrix(fitted.dag$vars))
}

#' @export
get.covariance.sparsebnPath <- function(x, data, ...){
    lapply(x, function(z) get.covariance(z, data))
}

cov_mat <- function(coefs, vars){
    # Checks: nrow = ncol

    pp <- nrow(coefs)
    identity_mat <- Matrix::Diagonal(pp, rep(1, pp))
    Matrix::t(Matrix::solve(identity_mat - coefs)) %*% vars %*% Matrix::solve(identity_mat - coefs)
}

### Inverse covariance fitting --------------------------------------------------------
#' @export
estimate.precision.sparsebnData <- function(data, ...){
    estimated.dags <- sparsebn::estimate.dag(data, ...)
    # fitted.dags <- estimate.parameters(estimated.dags, data)
    get.precision(estimated.dags, data)
}

#' @export
get.precision.sparsebnFit <- function(x, data, ...){
    fitted.dag <- estimate.parameters(x, data)
    inv_cov_mat(Matrix::Matrix(fitted.dag$coefs), Matrix::Matrix(fitted.dag$vars))
}

#' @export
get.precision.sparsebnPath <- function(x, data, ...){
    lapply(x, function(z) get.precision(z, data))
}

inv_cov_mat <- function(coefs, vars){
    # Checks: nrow = ncol

    pp <- nrow(coefs)
    identity_mat <- Matrix::Diagonal(pp, rep(1, pp))
    (identity_mat - coefs) %*% Matrix::solve(vars) %*% Matrix::t(identity_mat - coefs)
}
