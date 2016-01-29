context("sparsebnFit get.___")

m <- rbind(c(0,  0,  0),
           c(1,  0,  0),
           c(0, 2.1, 0))
sbm <- SparseBlockMatrixR(m)
m <- Matrix::Matrix(m)

li <- list(sbm = sbm, lambda = pi, nedge = 2, pp = 3, nn = 10, time = exp(1))
cf <- sparsebnFit(li)
cf.li <- list(cf, cf, cf)

test_that("get.___ methods", {
    expect_that(get.adjacency.matrix(cf), is_equivalent_to(m))

    ### 7-29-15: get.rhos has been deprecated
    # expect_that(get.rhos(cf), is_equivalent_to(rep(0, ncol(m))))

    ### Need to add tests for lambda.grid here
})
