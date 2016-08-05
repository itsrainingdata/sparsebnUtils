#
#  sparsebnUtils-selection.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 8/3/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Methods for model / parameter selection
#
#   CONTENTS:
#       select.parameter
#

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
                             alpha = 0.05){
    ### Check args
    stopifnot(is.sparsebnPath(x))
    stopifnot(is.sparsebnData(data))
    type <- match.arg(type, c("profile", "full"))

    ### Estimate unpenalized parameters
    params <- estimate.parameters(x, data)

    ### Compute (profile / full) log-likelihood + number of edges
    obj <- switch(type,
                  profile = unlist(lapply(params, function(x) gaussian_profile_loglikelihood(data$data, x$coefs))),
                  full = unlist(lapply(params, function(x) gaussian_loglikelihood(data$data, x$coefs, x$vars)))
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
