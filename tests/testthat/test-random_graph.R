context("random.graph")

test_that("random.graph runs as expected", {
    expect_error(random.graph(5, 5, acyclic = TRUE, loops = TRUE), NA)
    expect_error(random.graph(5, 5, acyclic = TRUE, loops = FALSE), NA)
    expect_error(random.graph(5, 5, acyclic = FALSE, loops = TRUE), NA)
    expect_error(random.graph(5, 5, acyclic = FALSE, loops = FALSE), NA)
})

test_that("random.graph does not produce loops", {
    gr <- random.graph(5, 5, loops = FALSE)
    m <- as.matrix(gr)
    expect_equivalent(diag(m), rep(0, 5))
})

test_that("random.graph works in degenerate cases", {
    # Null graphs
    expect_error(random.graph(1, 0), NA)
    expect_error(random.graph(2, 0), NA)
    expect_error(random.graph(3, 0), NA)
    expect_error(random.graph(5, 0), NA)

    # One edge
    ### nnode = 1 can't have any edges
    expect_error(random.graph(2, 1), NA)
    expect_error(random.graph(3, 1), NA)
    expect_error(random.graph(5, 1), NA)

    # Max edges
    expect_error(random.graph(1, 0), NA)
    expect_error(random.graph(2, 1), NA)
    expect_error(random.graph(3, 3), NA)
    expect_error(random.graph(5, 10), NA)
})
