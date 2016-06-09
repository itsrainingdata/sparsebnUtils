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
#       choose_fit_method
#       fit_glm_dag
#

### DAG fitting --------------------------------------------------------
#' @export
estimate.parameters.edgeList <- function(fit, data, ...){
    choose_fit_method(fit, data, ...)
}

#' @export
estimate.parameters.sparsebnFit <- function(fit, data, ...){
    estimate.parameters.edgeList(fit$edges, data)
}

#' @export
estimate.parameters.sparsebnPath <- function(fit, data, ...){
    lapply(fit, function(x){ estimate.parameters.sparsebnFit(x, data)})
}

### Choose which fitting method to use: Enforces use of OLS or logistic regression only
###  (See fit_glm_dag for a more general fitting function)
choose_fit_method <- function(edges, data, ...){
    family <- pick_family(data)
    if(family == "gaussian"){
        out <- fit_glm_dag(edges, data$data, call = "lm.fit", ...)
    } else if(family == "binomial"){
        out <- fit_glm_dag(edges, data$data, call = "glm.fit", family = stats::binomial(), ...)
        out <- out$coefs
    } else if(family == "multinomial"){
        out <- fit_multinom_dag(edges, dat = data$data, n_levels = unlist(auto_count_levels(data$data)), ...)
    }

    out
}

#' Inference in Bayesian networks
#'
#' Basic computing engine called by \code{\link{estimate.parameters}} for fitting parameters
#' in a Bayesian network. Should not be used directly unless by experienced users.
#'
#' Can call either \code{\link{lm.fit}} or \code{\link{glm.fit}}, with any choice of family.
#'
#' @param parents \code{\link{edgeList}} object.
#' @param dat Data.
#' @param call Either \code{"lm.fit"} or \code{"glm.fit"}.
#' @param ... If \code{call = "glm.fit"}, specify \code{family} here. Also allows for other parameters to \code{lm.fit} and \code{glm.fit}.
#'
#' @export
fit_glm_dag <- function(parents,
                    dat,
                    call = "lm.fit",
                    ...
){
    dat <- as.matrix(dat) ### Only works for fully observed, numeric data

    pp <- num.nodes(parents)
    if(ncol(dat) != pp){
        stop(sprintf("Incompatible graph and data! Data has %d columns but graph has %d nodes.", ncol(dat), pp))
    }
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

        vars[j] <- stats::var(dag.fit$residuals)
    }

    list(coefs = coefs, vars = Matrix::Diagonal(pp, vars))
}

# a function to convert the coefficient vector into a list of coefficients
get_coef_matrix <- function(coef_vec, n_levels) {
    # a vector to index each independant varibles
    node <- 1:length(n_levels)
    # a vector to index each coefficient in coef_vec
    node_index <- rep(node, (n_levels-1))
    if (!is.matrix(coef_vec)) {coef_vec <- matrix(coef_vec, nrow=1)}
    # check if number of columns of coef_vec is compatible with n_levels
    if (ncol(coef_vec)!=sum(n_levels-1)) stop("number of columns of coef_vec is not compatible with n_levels")
    if (length(n_levels) >=2) {
        coef_matrix <- lapply(node, function(x, node_index, coef_vec){matrix(coef_vec[, which(node_index==x)], nrow=nrow(coef_vec))}, node_index, coef_vec)
    } else {
        coef_matrix <- list(coef_vec)
    }

    return(coef_matrix)
}

#' A function to do inference in Bayesian network.
#' @param parents An edgeList object.
#' @param n_levels A vector indicating number of levels for each variable
#' @param dat Data, a dataframe or matrix
#' @return
#' List, of length the number of nodes. ith entry of the list is another list that contains parents of node i and the intercept coefficient for the ith node. For each parent of node i, it is a list of index of parent and the coefficient matrix of influence of parent has on node i.
#' @export
fit_multinom_dag <- function(parents, # rename to something else
                             n_levels,
                             dat
) {
    data <- as.data.frame(dat)

    node <- ncol(data)
    # check that the number of node and the what has been input in parents are consistent
    if (length(parents) != ncol(data)) {stop(sprintf("Incompatible graph and data! Data has %d columns but graph has %d nodes.", ncol(dat), length(parents)))}

    # factorize each observation
    for (i in 1:node){
        level <- 0:(n_levels[i]-1)
        data[,i] <- factor(data[,i],levels=level)
    }

    # subtract dependent and independent variables for each regression
    coef <- lapply(seq_len(node), function(i){integer(0)})
    for (i in 1:node){
        x_ind <- parents[[i]] # if inputs are only edgeList object
        if (length(x_ind)!=0) { # do nothing if a node has no parents
            fit <- nnet::multinom(data[, c(i, x_ind)], trace = FALSE) # Why does this work / should we do this?
            coef_vec <- coef(fit)
            temp_n_levels <- n_levels[x_ind]
            intercept <- coef_vec[1]
            coef_vec <- coef_vec[-1]
            coef_seq <- get_coef_matrix(coef_vec, temp_n_levels)
            node_index <- 1:length(x_ind)
            coef[[i]] <- lapply(node_index, function(x, coef_seq, x_ind){list(parent=x_ind[x], coef=coef_seq[[x]])}, coef_seq, x_ind)
            coef[[i]][[length(x_ind)+1]] <- list(intercept=intercept)
        }
    }
    return(coef)
}
