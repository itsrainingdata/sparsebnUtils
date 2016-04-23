context("setGraphPackage")

sbf.empty <- generate_empty_sparsebnFit()
sbp.empty <- generate_empty_sparsebnPath()
sbp <- generate_fixed_sparsebnPath()
sbf <- generate_fixed_sparsebnFit()

test_that("Switching without coercion works", {
    ### graph pkg
    expect_that(setGraphPackage("graph"), not(throws_error()))
    expect_equal(options()$sparsebn.graph, "graph")

    ### igraph pkg
    expect_that(setGraphPackage("igraph"), not(throws_error()))
    expect_equal(options()$sparsebn.graph, "igraph")

    ### network pkg
    expect_that(setGraphPackage("network"), not(throws_error()))
    expect_equal(options()$sparsebn.graph, "network")

    ### Back to NULL
    expect_that(setGraphPackage(NULL), not(throws_error()))
    expect_equal(options()$sparsebn.graph, NULL)
})

test_that("Switching with coercion works", {
    ### How to force coercion in the local environment of testthat?
})
