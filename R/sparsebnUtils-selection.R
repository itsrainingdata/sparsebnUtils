#
#  sparsebnUtils-selection.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 8/3/16.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Methods for model / parameter selection
#
#   CONTENTS:
#       select
#       select.parameter
#       get.solution
#

#' Select solutions from a solution path
#'
#' Choose solutions from a solution path based on number of edges, value of
#' regularization parameter lambda, or index.
#'
#' For \code{edges} (resp. \code{lambda}), the solution with the closest number
#' of edges (resp. regularization parameter) is returned. If there is no match
#' within a tolerance of 0.1 for \code{lambda}, nothing is returned. Fuzzy matching
#' is not used for when selecting by \code{index}.
#'
#' If there is more than one match (for example, by number of edges), then
#' the first such estimate is returned. Note that \code{select(x, index = j)}
#' is equivalent to (but slightly slower than) \code{x[[j]]}.
#'
#' @param x a \code{\link{sparsebnPath}} object.
#' @param edges number of edges to search for.
#' @param lambda value of regularization parameter to search for.
#' @param index integer index to select.
#'
#' @export
select <- function(x, edges, lambda, index){
    stopifnot(is.sparsebnPath(x))

    which.idx <- integer(0)
    if(!missing(edges)){
        if(!missing(lambda) || !missing(index)){
            stop("'edges' cannot be specified with 'lambda' or 'index'! Select only one.")
        }

        ### Note the use of partial matching here
        which.idx <- pmatch_numeric(edges, num.edges(x), tol = Inf)
        # which.idx <- which(num.edges(x) == edges)
    } else if(!missing(lambda)){
        if(!missing(edges) || !missing(index)){
            stop("'lambda' cannot be specified with 'edges' or 'index'! Select only one.")
        }

        ### Note the use of partial matching here
        which.idx <- pmatch_numeric(lambda, get.lambdas(x), tol = 0.1)
    } else if(!missing(index)){
        if(!missing(edges) || !missing(lambda)){
            stop("'index' cannot be specified with 'edges' or 'lambda'! Select only one.")
        }

        which.idx <- index
    } else{
        stop("Must specify something to select! Choose 'edges', 'lambda', or 'index'.")
    }

    if(length(which.idx) == 0){
        NULL # return NULL if nothing found (mimics default behaviour of lists in R)
    } else{
        x[[min(which.idx)]] # return minimum index by default
    }
}

#' Tuning parameter selection
#'
#' Choose the best DAG model according to the criterion described in \href{http://www.stat.ucla.edu/~zhou/publications/Fu13-JASA.pdf}{Fu and Zhou (2013)}
#' (Section 3.4).
#'
#' A \code{\link{sparsebnPath}} objects represents a \emph{solution path} which depends on the regularization parameter lambda.
#' Model selection is usually based on an estimated prediction error, and commonly used model selection
#' methods include the Bayesian information criterion (BIC) and cross-validation (CV) among others. It
#' is well-known that these criteria tend to produce overly complex models in practice, so instead we
#' employ an empirical model selection criterion that works well in practice. As lambda is decreased and
#' thus the model complexity increases, the log-likelihood of the estimated graph will increase. An
#' increase in model complexity, which is represented by an increase in the total number of predicted
#' edges, is desirable only if there is a substantial increase in the log-likelihood. In order to select
#' an optimal parameter, this method computes successive difference ratios between the increase in
#' log-likelihood and the increase in number of edges and balances these quantities appropriately. For
#' specific details, please see Section 3.4 in \href{http://www.stat.ucla.edu/~zhou/publications/Fu13-JASA.pdf}{Fu and Zhou (2013)}.
#'
#' @param x \code{\link{sparsebnPath}} object.
#' @param data \code{\link{sparsebnData}} containing the original data.
#' @param type either "\code{profile}" or "\code{full}", default is \code{profile}.
#' @param alpha tuning parameter for selection between 0.05 and 0.1, default is 0.5 (see equation (11) in \href{http://www.stat.ucla.edu/~zhou/publications/Fu13-JASA.pdf}{Fu and Zhou (2013)}).
#'
#' @export
select.parameter <- function(x,
                             data,
                             type = "profile",
                             alpha = 0.1){
    ### Check args
    stopifnot(is.sparsebnPath(x))
    stopifnot(is.sparsebnData(data))

    if (data$type == "continuous") {
        type <- match.arg(type, c("profile", "full"))

        ### Estimate unpenalized parameters
        params <- estimate.parameters(x, data)

        ### Compute (profile / full) log-likelihood + number of edges
        obj <- switch(type,
                      profile = unlist(lapply(params, function(x) gaussian_profile_loglikelihood(data$data, x$coefs))),
                      full = unlist(lapply(params, function(x) gaussian_loglikelihood(data$data, x$coefs, x$vars)))
        )
    }
    else {
        edge_path <- lapply(x, function(x){x$edges})
        obj <- sapply(edge_path, function(x) multinom_loglikelihood(x, data$data))
    }

    nedges <- num.edges(x)

    ### Compute lagged differences, difference ratios, and threshold
    dobj <- diff(obj)
    # dprloglik <- diff(prloglik)
    dnedge <- diff(nedges)
    dr <- dobj / dnedge
    dr[dnedge == 0] <- NA
    threshold <- alpha * max(dr, na.rm = TRUE)

    max(which(dr >= threshold))+1
}

### Deprecated
#' Select solutions from a solution path
#'
#' Choose solutions from a solution path based on number of edges, value of
#' regularization parameter lambda, or index.
#'
#' For \code{edges} (resp. \code{lambda}), the solution with the closest number
#' of edges (resp. regularization parameter) is returned. If there is no match
#' within a tolerance of 0.1 for \code{lambda}, nothing is returned. Fuzzy matching
#' is not used for when selecting by \code{index}.
#'
#' If there is more than one match (for example, by number of edges), then
#' the first such estimate is returned. Note that \code{select(x, index = j)}
#' is equivalent to (but slightly slower than) \code{x[[j]]}.
#'
#' @param x a \code{\link{sparsebnPath}} object.
#' @param edges number of edges to search for.
#' @param lambda value of regularization parameter to search for.
#' @param index integer index to select.
#'
#' @export
get.solution <- function(x, edges, lambda, index){
    .Deprecated(new = "select")
}
