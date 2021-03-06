context("setGraphPackage")

#
# THESE TESTS RELY ON A HELPER FILE
#

sbf.empty <- generate_empty_sparsebnFit()
sbp.empty <- generate_empty_sparsebnPath()
sbp <- generate_fixed_sparsebnPath()
sbf <- generate_fixed_sparsebnFit()

test_that("Switching without coercion works", {
    ### graph pkg
    if(requireNamespace("graph", quietly = TRUE)){
        expect_error(setGraphPackage("graph"), NA)
        expect_equal(options()$sparsebn.graph, "graph")
    }

    ### igraph pkg
    if(requireNamespace("igraph", quietly = TRUE)){
        expect_error(setGraphPackage("igraph"), NA)
        expect_equal(options()$sparsebn.graph, "igraph")
    }
    ### network pkg
    if(requireNamespace("network", quietly = TRUE)){
        expect_error(setGraphPackage("network"), NA)
        expect_equal(options()$sparsebn.graph, "network")
    }

    ### Back to NULL
    expect_error(setGraphPackage(NULL), NA)
    expect_equal(options()$sparsebn.graph, NULL)
})

test_that("Switching with coercion works", {
    ### How to force coercion in the local environment of testthat?
})
