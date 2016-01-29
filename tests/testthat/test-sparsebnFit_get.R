context("sparsebnFit get.___")

edges <- generate_fixed_edgeList()
m <- rbind(c(0, 0, 0, 0, 0),
           c(1, 0, 0, 0, 0),
           c(0, 1, 0, 0, 0),
           c(1, 1, 0, 0, 0),
           c(0, 1, 0, 0, 0))
m <- Matrix::Matrix(m)

li <- list(edges = edges, lambda = pi, nedge = num.edges(edges), pp = num.nodes(edges), nn = 10, time = exp(1))
sbf <- sparsebnFit(li)

test_that("get.___ methods", {
    expect_that(get.adjacency.matrix(sbf), is_equivalent_to(m))

    ### 7-29-15: get.rhos has been deprecated
    # expect_that(get.rhos(cf), is_equivalent_to(rep(0, ncol(m))))

    ### Need to add tests for lambda.grid here
})
