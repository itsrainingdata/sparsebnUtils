#
#  sparsebnUtils-inference.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 3/17/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Methods for parameter inference
#
#   CONTENTS:
#       estimate.parameters
#       fit_ols_dag
#

### DAG fitting --------------------------------------------------------
#' @export
estimate.parameters.edgeList <- function(edges, data, ...){
    choose_fit_method(edges, data, ...)
}

#' @export
estimate.parameters.sparsebnFit <- function(fit, data, ...){
    estimate.parameters.edgeList(fit$edges, data)
}

#' @export
estimate.parameters.sparsebnPath <- function(path, data, ...){
    lapply(path, function(x){ estimate.parameters.sparsebnFit(x, data)})
}

### Choose which fitting method to use: Enforces use of OLS or logistic regression only
###  (See fit_dag for a more general fitting function)
choose_fit_method <- function(edges, data, ...){
    family <- pick_family(data)
    if(family == "gaussian"){
        fit_dag(edges, data$data, call = "lm.fit", ...)
    } else if(family == "binomial"){
        fit_dag(edges, data$data, call = "glm.fit", family = binomial(), ...)
    }
}

#' Inference in Bayesian networks
#'
#' Basic computing engine called by \code{\link{estimate.parameters}} for fitting parameters
#' in a Bayesian network. Should not be used directly unless by experienced users.
#'
#' Can call either \code{\link{lm.fit}} or \code{link{glm.fit}}, with any choice of family.
#'
#' @param parents \code{\link{edgeList}} object.
#' @param dat Data.
#' @param call Either \code{"lm.fit"} or \code{"glm.fit"}.
#' @param ... If \code{call = "glm.fit"}, specify \code{family} here. Also allows for other parameters to \code{lm.fit} and \code{glm.fit}.
#'
#' @export
fit_dag <- function(parents,
                    dat,
                    call = "lm.fit",
                    ...
){
    dat <- as.matrix(dat) ### Only works for fully observed, numeric data

    pp <- num.nodes(parents)
    nn <- nrow(dat)
    coefs <- Matrix::Diagonal(pp, 0)
    vars <- numeric(pp)

    # print(parents)
    for(j in 1:pp){
        select.vars <- parents[[j]]

        if(length(select.vars) > nn){
            stop(sprintf("Node %d has too many parents! <%d > %d>\n", j, length(select.vars), nn))
        }

        # lm.fit is much faster than glm.fit!
        #         lm.fit, p = 200, n = 1000
        #         elapsed
        #           2.898
        #         glm.fit, p = 200, n = 1000
        #         elapsed
        #           5.289
#         if(opt == 1) ols.fit <- lm.fit(x = dat[, select.vars, drop = FALSE], y = dat[, j])
#         if(opt == 2) ols.fit <- glm.fit(x = dat[, select.vars, drop = FALSE], y = dat[, j], family = gaussian())
        dag.fit <- do.call(what = call, args = list(x = dat[, select.vars, drop = FALSE], y = dat[, j], ...))
        # if(opt == 2) ols.fit <- do.call("glm.fit", args = list(x = dat[, select.vars, drop = FALSE], y = dat[, j], family = gaussian()))

        coefs[select.vars, j] <- dag.fit$coefficients

        vars[j] <- var(dag.fit$residuals)
    }

    list(coefs = coefs, vars = Matrix::Diagonal(pp, vars))
}

### Covariance fitting --------------------------------------------------------
#' @export
get.covariance.matrix <- function(coefs, vars, ...){
    get.covariance.Matrix(Matrix::Matrix(coefs), Matrix::Matrix(vars))
}

#' @export
get.covariance.Matrix <- function(coefs, vars, ...){
    if(missing(vars)) stop("Must specify variance matrix!")

    cov_mat(coefs, vars)
}

#' @export
get.covariance.list <- function(li, ...){
    stopifnot(check_list_names(li, c("coefs", "vars")))
    get.covariance(li$coefs, li$vars)
}

#' @export
estimate.covariance.sparsebnFit <- function(fit, data, ...){
    fitted.dag <- estimate.parameters(fit, data)
    get.covariance(fitted.dag$coefs, fitted.dag$vars)
}

#' @export
estimate.covariance.sparsebnPath <- function(path, data, ...){
    lapply(path, function(x) estimate.covariance.sparsebnFit(x, data, ...))
}

cov_mat <- function(coefs, vars){
    # Checks: nrow = ncol

    pp <- nrow(coefs)
    identity_mat <- Matrix::Diagonal(pp, rep(1, pp))
    Matrix::t(Matrix::solve(identity_mat - coefs)) %*% vars %*% Matrix::solve(identity_mat - coefs)
}

### Inverse covariance fitting --------------------------------------------------------
#' @export
get.precision.matrix <- function(coefs, vars, ...){
    get.precision.Matrix(Matrix::Matrix(coefs), Matrix::Matrix(vars))
}

#' @export
get.precision.Matrix <- function(coefs, vars, ...){
    if(missing(vars)) stop("Must specify variance matrix!")

    inv_cov_mat(coefs, vars)
}

#' @export
get.precision.list <- function(li, ...){
    stopifnot(check_list_names(li, c("coefs", "vars")))
    get.precision(li$coefs, li$vars)
}

#' @export
estimate.precision.sparsebnFit <- function(fit, data, ...){
    fitted.dag <- estimate.parameters(fit, data)
    get.precision(fitted.dag$coefs, fitted.dag$vars)
}

#' @export
estimate.precision.sparsebnPath <- function(path, data, ...){
    lapply(path, function(x) estimate.precision.sparsebnFit(x, data, ...))
}

inv_cov_mat <- function(coefs, vars){
    # Checks: nrow = ncol

    pp <- nrow(coefs)
    identity_mat <- Matrix::Diagonal(pp, rep(1, pp))
    (identity_mat - coefs) %*% Matrix::solve(vars) %*% Matrix::t(identity_mat - coefs)
}
