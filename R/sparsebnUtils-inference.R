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

estimate.parameters.edgeList <- function(edges, data){
    fit_ols_dag(edges, data$data)
}

estimate.parameters.sparsebnFit <- function(fit, data){
    estimate.parameters.edgeList(fit$edges, data)
}

estimate.parameters.sparsebnPath <- function(path, data){
    lapply(path, estimate.parameters.sparsebnFit)
}

fit_ols_dag <- function(parents, dat){
    dat <- as.matrix(dat) ### Only works for fully observed, numeric data

    pp <- num.nodes(parents)
    nn <- nrow(dat)
    coefs <- Matrix::Diagonal(pp, 0)
    vars <- numeric(pp)

    print(parents)
    for(j in 1:pp){
        select.vars <- parents[[j]]

        if(length(select.vars) > nn){
            stop(sprintf("Node %d has too many parents! <%d > %d>\n", j, length(select.vars), nn))
        }

        ols.fit <- lm.fit(x = dat[, select.vars, drop = FALSE], y = dat[, j])
        coefs[select.vars, j] <- ols.fit$coefficients

        vars[j] <- var(ols.fit$residuals)
    }

    list(B = coefs, Omega = Matrix::Diagonal(pp, vars))
}
