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
#       get_coef_matrix
#       fit_multinom_dag
#       gaussian_loglikelihood
#       gaussian_profile_loglikelihood
#

### DAG fitting --------------------------------------------------------
#' @export
estimate.parameters.edgeList <- function(fit, data, ...){
    choose_fit_method(fit, data, ...)
}

#' @export
estimate.parameters.sparsebnFit <- function(fit, data, ...){
    estimate.parameters.edgeList(to_edgeList(fit$edges), data)
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
        out <- fit_multinom_dag(edges, dat = data$data, ...)
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

#' Inference in discrete Bayesian networks
#'
#' Given the structure of a Bayesian network, estimate the parameters
#' using multinomial logistic regression. For each node \eqn{i}, regress
#' \eqn{i} onto its parents set using \code{\link[nnet]{multinom}}
#' in package \code{\link{nnet}}.
#'
#' @param parents An \code{\link{edgeList}} object.
#' @param dat Data, a dataframe or matrix
#'
#' @return
#' A list with with one component for each node in the graph.
#' Each node is a coefficient matrix for the parents of that node.
#'
#' @examples
#'
#' \dontrun{
#' ### construct a random data set
#' x <- c(0,1,0,1,0)
#' y <- c(1,0,1,0,1)
#' z <- c(0,1,2,1,0)
#' a <- c(1,1,1,0,0)
#' b <- c(0,0,1,1,1)
#' dat <- data.frame(x, y, z, a, b)
#'
#' ### randomly construct an edgelist of a graph
#' nnode <- ncol(dat)
#' li <- vector("list", length = nnode)
#' li[[1]] <- c(2L,4L)
#' li[[2]] <- c(3L,4L,5L)
#' li[[3]] <- integer(0)
#' li[[4]] <- integer(0)
#' li[[5]] <- integer(0)
#' edgeL <- edgeList(li)
#'
#' ### run fit_multinom_dag
#' fit.multinom <- fit_multinom_dag(edgeL, dat)
#' }
#'
#' @export
fit_multinom_dag <- function(parents,
                             dat
) {
    data <- as.data.frame(dat)
    n_levels <- unlist(auto_count_levels(dat))

    node <- ncol(data)
    # check that the number of node and the what has been input in parents are consistent
    if (length(parents) != ncol(data)) {stop(sprintf("Incompatible graph and data! Data has %d columns but graph has %d nodes.", ncol(dat), length(parents)))}

    # factorize each observation
    for (i in 1:node){
        # level <- 0:(n_levels[i]-1)
        # data[,i] <- factor(data[,i],levels=level)
        data[,i] <- factor(data[,i])
    }

    # subtract dependent and independent variables for each regression
    coef <- lapply(seq_len(node), function(i){integer(0)})
    for (i in 1:node){
        x_ind <- parents[[i]] # if inputs are only edgeList object
        if (length(x_ind)!=0) { # do nothing if a node has no parents
            fit <- nnet::multinom(data[, c(i, x_ind)], trace = FALSE) # Why does this work / should we do this?
            coef_vec <- coef(fit)
            subname <- names(data)[x_ind]
            subn_levels <- n_levels[x_ind] - 1
            if (is.matrix(coef_vec)) {
                coef_names <- colnames(coef_vec)
            }
            else
                coef_names <- names(coef_vec)
            name_ind <- c(0, rep(x_ind, subn_levels))
            coef_names_new <- lapply(1:length(x_ind), function(x){gsub(subname[x], paste0(subname[x], "_"), coef_names[which(name_ind==x_ind[x])])})
            coef_names_new <- c(coef_names[1], unlist(coef_names_new))
            if (is.matrix(coef_vec)) {
                colnames(coef_vec) <- coef_names_new
            }
            else
                names(coef_vec) <- coef_names_new
            coef[[i]] <- coef_vec
        }
    }
    names(coef) <- colnames(dat)
    return(coef)
}

gaussian_loglikelihood <- function(dat, coefs, vars){
    # data_matrix <- as.matrix(data$data)
    dat <- as.matrix(dat)
    vars_vector <- Matrix::diag(vars)
    nn <- nrow(dat)
    pp <- ncol(dat)

    ### Compute cumulant function
    cumulant <- -0.5 * nn * sum(log(vars_vector))

    ### Compute LS
    ls <- numeric(pp)
    for(j in seq_along(ls)){
        res <- dat[, j] - dat %*% coefs[, j]
        ls[j] <- (0.5 / vars_vector[j]) * sum(res^2)
    }
    ls <- sum(ls)

    cumulant + ls
}

gaussian_profile_loglikelihood <- function(dat, coefs){
    # data_matrix <- as.matrix(data$data)
    dat <- as.matrix(dat)
    nn <- nrow(dat)
    pp <- ncol(dat)

    ### Compute log(LS)
    pll <- numeric(pp)
    for(j in seq_along(pll)){
        res <- dat[, j] - dat %*% coefs[, j]
        pll[j] <- 0.5 * nn * log(sum(res^2))
    }
    pll <- sum(pll)

    -pll ### Need to take negative output loglikelihood (vs NLL)
}
