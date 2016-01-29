context("sparsebnFit_constructor")

m <- rbind(c(0,  0,  0),
           c(1,  0,  0),
           c(0, 2.1, 0))
sbm <- SparseBlockMatrixR(m)

test_that("sparsebnFit correctly identifies when input number of nodes is not consistent", {
    li <- list(sbm = sbm, lambda = pi, nedge = 2, pp = 1, nn = 10, time = exp(1))
    expect_error(cf <- sparsebnFit(li))
})

test_that("sparsebnFit correctly identifies when input number of edges is not consistent", {
    li <- list(sbm = sbm, lambda = pi, nedge = 20, pp = 3, nn = 10, time = exp(1))
    expect_error(cf <- sparsebnFit(li))
})

test_that("sparsebnFit is consistent with different ways of accessing nedge", {
    li <- list(sbm = sbm, lambda = pi, nedge = 2, pp = nrow(m), nn = 10, time = exp(1))
    cf <- sparsebnFit(li) ### Should not generate an error anymore

    matrix.nedge <- Matrix::nnzero(get.adjacency.matrix(cf$edges))
    edgeL.nedge <- num.edges(sbm)

    ### Note that sbm.nedge and cf$nedge are equal by construction since nedge is set manually above
    expect_equal(edgeL.nedge, matrix.nedge, cf$nedge)
})
