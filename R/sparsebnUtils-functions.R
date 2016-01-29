#
#  ccdr-functions.R
#  ccdr
#
#  Created by Bryon Aragam (local) on 2/4/15.
#  Copyright (c) 2014-2015 Bryon Aragam (local). All rights reserved.
#

#
# PACKAGE CCDR: Functions
#
#   CONTENTS:
#     generate.lambdas
#     gen_lambdas
#

#' generate.lambdas
#'
#' Convenience function for creating a grid of lambdas.
#'
#' See Section 5.3 of \href{http://arxiv.org/abs/1401.0852}{Aragam and Zhou (2015), JMLR}
#' for a discussion of regularization paths (also, solution paths) as used in the CCDr
#' algorithm.
#'
#' @param lambda.max Maximum value of lambda; in terms of the algorithm this is the initial value
#'                   of the regularization parameter in the solution path.
#' @param lambdas.ratio Ratio between the maximum lambda value and the minimum lambda value in the solution
#'                      path.
#' @param lambdas.length Number of values to include.
#' @param scale Which scale to use: Either \code{"linear"} or \code{"log"}.
#'
#' @export
generate.lambdas <- function(lambda.max,
                             lambdas.ratio = 1e-3,
                             lambdas.length = 50,
                             scale = "linear"
){
    lambda.min <- lambdas.ratio * lambda.max
    gen_lambdas(lambda.max = lambda.max,
                lambda.min = lambda.min,
                lambdas.length = lambdas.length,
                scale = scale)
} # END GENERATE.LAMBDAS

# gen_lambdas
#  Internal implementation of generate.lambdas
gen_lambdas <- function(lambda.max,
                        lambda.min,
                        lambdas.length = 50,
                        scale = "linear"
){
#     lambda.max <- max(sqrt(nn))
#     lambda.min <- lambdas.ratio * lambda.max

    if(scale == "linear"){
        lambdas <- seq(lambda.max, lambda.min, length.out = lambdas.length)
    } else if(scale == "log"){
        lambdas.ratio <- lambda.min / lambda.max
        lambdas <- exp(seq(log(lambda.max), log(lambda.min), log(lambdas.ratio)/(lambdas.length-1)))
    } else{
        stop("Invalid input for scale argument! Must be either 'log' or 'linear'.")
    }

    lambdas
} # END GEN_LAMBDAS
