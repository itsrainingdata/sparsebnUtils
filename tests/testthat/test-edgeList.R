context("edgeList")

test_that("edgeList constructor fails for graphs with no nodes", {
    expect_error(edgeList())
})

test_that("edgeList constructor does NOT fail for empty graphs with >0 nodes", {
    expect_that(edgeList(list(integer(0), integer(0))), not(throws_error()))
})

test_that("edgeList constructor checks for inconsistent indices", {
    expect_error(edgeList(list(0)))                 # 0 is not a valid index
    expect_error(edgeList(list(1, 1, c(1, 5))))     # 5 > 3 = number of nodes
    expect_error(edgeList(list(1, 1, c(1, -1))))    # -1 is not a valid index
})
