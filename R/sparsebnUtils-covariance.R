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

### Covariance estimation --------------------------------------------------------
#' @export
get.covariance.edgeList <- function(x, data, ...){
    fitted.dag <- estimate.parameters(x, data)
    cov_mat(Matrix::Matrix(fitted.dag$coefs), Matrix::Matrix(fitted.dag$vars))
}

#' @export
get.covariance.sparsebnFit <- function(x, data, ...){
    get.covariance(x$edges, data, ...)
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

### Inverse covariance estimation --------------------------------------------------------
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
