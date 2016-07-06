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

# a function to convert the coefficient vector into a list of coefficients
get_coef_matrix <- function(coef_vec, node_name, n_levels) {
    # a vector to index each independant varibles
    node <- 1:length(n_levels)
    # a vector to index each coefficient in coef_vec
    node_index <- rep(node, (n_levels-1))
    # if (!is.matrix(coef_vec)) {coef_vec <- matrix(coef_vec, nrow=1)}
    # check if number of columns of coef_vec is compatible with n_levels
    if (ncol(coef_vec)!=sum(n_levels-1)) stop("number of columns of coef_vec is not compatible with n_levels")
    if (length(n_levels) >=2) {
        coef_matrix <- lapply(node, function(x, node_index, coef_vec, node_name){
            coef_sub <- coef_vec[, which(node_index==x), drop=FALSE]
            coef_names <- colnames(coef_sub)
            coef_names <- gsub(node_name[x], "", coef_names)
            colnames(coef_sub) <- coef_names
            coef_sub
            }, node_index, coef_vec, node_name)
    } else {
        coef_names <- colnames(coef_vec)
        coef_names <- gsub(node_name[1], "", coef_names)
        colnames(coef_vec) <- coef_names
        coef_matrix <- list(coef_vec)
    }

    return(coef_matrix)
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
#' A list with with one component for each node in the graph. The
#' \eqn{i}th entry is a list containing information on the \eqn{i}th
#' regression, last element for the \eqn{i}th regression is the
#' intercept coefficient (always a scalar). For each parent \eqn{j} of
#' node \eqn{i}, there is a list consisting of the index of parent \eqn{j}
#' and a coefficient matrix \eqn{C_{ij}} for the influence parent \eqn{j}
#' has on node \eqn{i} (the coefficient matrix is of size
#' \eqn{(r_i-1)\times (r_j-1)}, where \eqn{r_i} is the number of levels
#' of ith node). Note that the \eqn{(h,k)}th entry of the coefficient
#' matrix \eqn{C_{ij}}, is the coefficient for level \eqn{k} of parent
#' \eqn{j} has on level \eqn{h} of node \eqn{i}.
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
#'
#' ### interpret the output
#' fit.multinom # a list of length 5
#' fit.multinom[[1]] # the first variable has 2 parents,
#'  thus the first entry has 3 slots. The last slot is the intercept coefficient.
#'  And the first two slots each represent for a parent
#' fit.multinom[[1]][[1]] # the first parent for the first node is "y"
#' and the coefficient is -19.44
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
            if(!is.matrix(coef_vec)) {
                coef_name <- names(coef_vec)
                coef_vec <- matrix(coef_vec, nrow=1)
                colnames(coef_vec) <- coef_name
                }
            temp_n_levels <- n_levels[x_ind]
            intercept <- coef_vec[, 1, drop = FALSE]
            coef_vec <- coef_vec[, -1, drop = FALSE]
            node_name <- as.character(colnames(data[, x_ind]))
            coef_seq <- get_coef_matrix(coef_vec, node_name, temp_n_levels)
            node_index <- 1:length(x_ind)
            # coef[[i]] <- lapply(node_index, function(x, coef_seq, x_ind){list(parent=x_ind[x], coef=coef_seq[[x]])}, coef_seq, x_ind)
            coef[[i]] <- lapply(node_index, function(x, coef_seq, x_ind){list(parent=colnames(data)[x_ind[x]], coef=coef_seq[[x]])}, coef_seq, x_ind)
            coef[[i]][[length(x_ind)+1]] <- list(intercept=intercept)
        }
    }
    return(coef)
}

gaussian_loglikelihood <- function(data, coefs, vars){
    data_matrix <- as.matrix(data$data)
    vars_vector <- Matrix::diag(vars)
    nn <- nrow(data_matrix)
    pp <- ncol(data_matrix)

    ### Compute cumulant function
    cumulant <- -0.5 * nn * sum(log(vars_vector))

    ### Compute LS
    ls <- numeric(pp)
    for(j in seq_along(ls)){
        res <- data_matrix[, j] - data_matrix %*% coefs[, j]
        ls[j] <- (0.5 / vars_vector[j]) * sum(res^2)
    }
    ls <- sum(ls)

    cumulant + ls
}

gaussian_profile_loglikelihood <- function(data, coefs){
    data_matrix <- as.matrix(data$data)
    nn <- nrow(data_matrix)
    pp <- ncol(data_matrix)

    ### Compute log(LS)
    pll <- numeric(pp)
    for(j in seq_along(pll)){
        res <- data_matrix[, j] - data_matrix %*% coefs[, j]
        pll[j] <- 0.5 * nn * log(sum(res^2))
    }
    pll <- sum(pll)

    -pll ### Need to take negative output loglikelihood (vs NLL)
}

### Eventually move to its own file
select.parameter <- function(x,
                             data,
                             type = "profile",
                             alpha = 0.05){
    ### Check args
    stopifnot(is.sparsebnPath(x))
    stopifnot(is.sparsebnData(data))
    type <- match.arg(type, c("profile", "full"))

    ### Estimate unpenalized parameters
    params <- estimate.parameters(x, data)

    ### Compute (profile / full) log-likelihood + number of edges
    obj <- switch(type,
                  profile = unlist(lapply(params, function(x) gaussian_profile_loglikelihood(dat, x$coefs))),
                  full = unlist(lapply(params, function(x) gaussian_loglikelihood(dat, x$coefs, x$vars)))
                  )
    nedges <- num.edges(x)

    ### Compute lagged differences, difference ratios, and threshold
    dobj <- diff(obj)
    # dprloglik <- diff(prloglik)
    dnedge <- diff(nedges)
    dr <- dobj / dnedge
    dr[dnedge == 0] <- NA
    threshold <- alpha * max(dr, na.rm = TRUE)

    max(which(dr >= threshold))
}
