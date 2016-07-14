context("get.solution")

sbp <- generate_nontrivial_sparsebnPath()

test_that("Selection by number of edges works", {
    expect_equal(get.solution(sbp, edges = 0), sbp[[1]])
    expect_equal(get.solution(sbp, edges = 4), sbp[[2]])
    expect_equal(get.solution(sbp, edges = 5), sbp[[3]])
    expect_equal(get.solution(sbp, edges = 7), sbp[[4]])
})

test_that("Selection by lambda works", {
    expect_equal(get.solution(sbp, lambda = 2.1), sbp[[1]])
    expect_equal(get.solution(sbp, lambda = 1.54), sbp[[2]])
    expect_equal(get.solution(sbp, lambda = 0.97), sbp[[3]])
    expect_equal(get.solution(sbp, lambda = 0.57), sbp[[4]])
})

test_that("Selection by index works", {
    expect_equal(get.solution(sbp, index = 1), sbp[[1]])
    expect_equal(get.solution(sbp, index = 2), sbp[[2]])
    expect_equal(get.solution(sbp, index = 3), sbp[[3]])
    expect_equal(get.solution(sbp, index = 4), sbp[[4]])
})

