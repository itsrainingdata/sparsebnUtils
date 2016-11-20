context("as.____")

gr <- generate_fixed_graphNEL()
edgeL <- generate_fixed_edgeList()
edgeL0 <- generate_empty_edgeList()
sp <- generate_fixed_sparse()
sp0 <- generate_empty_sparse()
mat <- generate_fixed_matrix()
adjmat <- generate_fixed_adjacency_matrix()

test_that("Conversion between edgeList <-> sparse works", {
    ### Check null graph
    expect_equivalent(sp0, as.sparse(as.edgeList(sp0)))
    expect_equivalent(edgeL0, as.edgeList(as.sparse(edgeL0)))

    sp$vals <- as.numeric(rep(NA, length(sp$vals))) # vals information will be lost by design so ignore this
    expect_equivalent(sp, as.sparse(as.edgeList(sp)))
    expect_equivalent(edgeL, as.edgeList(as.sparse(edgeL)))
})

test_that("Conversion between edgeList <-> matrix works", {
    expect_equivalent(adjmat, as.matrix(as.edgeList(mat))) # output should be adj matrix since weights are lost by design
    expect_equivalent(edgeL, as.edgeList(as.matrix(edgeL)))
})

test_that("Conversion between sparse <-> matrix works", {
    expect_equivalent(mat, as.matrix(as.sparse(mat))) # output not an adj matrix since sparse preserves weights
    expect_equivalent(sp, as.sparse(as.matrix(sp)))
})

test_that("Iterative coercing works", {
    ### The tests below fail: In going from edgeList -> sparse, NAs are introduced (by design)
    ###  We could, e.g. output an adjaency matrix for sparse -> matrix, but this hides the fact that
    ###  the weights are unknown. For now we leave as-is and expect this test to fail.
    expect_failure(expect_equivalent(edgeL, as.edgeList(as.matrix(as.sparse(edgeL)))))
    expect_failure(expect_equivalent(sp, as.sparse(as.edgeList(as.matrix(sp)))))
    expect_failure(expect_equivalent(mat, as.matrix(as.sparse(as.edgeList(mat)))))
    expect_failure(expect_equivalent(mat, as.matrix(as.edgeList(as.sparse(mat)))))

    ### edgeList
    expect_equivalent(edgeL, as.edgeList(as.sparse(as.matrix(edgeL))))

    ### sparse
    sp$vals <- as.numeric(rep(1, length(sp$vals))) # weights will be lost by design, and as.matrix will return adjacency matrix
    expect_equivalent(sp, as.sparse(as.matrix(as.edgeList(sp))))
})
