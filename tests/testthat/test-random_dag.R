context("random.dag")

test_that("random.dag runs as expected", { # PASSES
    expect_error(random.dag(1,1), "A DAG with p = 1")
    expect_error(random.dag(2,2), "A DAG with p = 2")

    # Base case
    expect_error(random.dag(3,2), NA)
    expect_error(random.dag(10,10), NA)

    # Base R RNGs
    expect_error(random.dag(3,2,"rnorm"), NA)
    expect_error(random.dag(10,10,"rnorm"), NA)
    expect_error(random.dag(3,2,"rexp"), NA)
    expect_error(random.dag(10,10,"rexp"), NA)
    expect_error(random.dag(3,2,"runif"), NA)
    expect_error(random.dag(10,10,"runif"), NA)

    # Custom RNGs
    expect_error(random.dag(3,2,function(n) rnorm(n, mean = 10, sd = 0.5)), NA)
    expect_error(random.dag(10,10,function(n) rnorm(n, mean = 10, sd = 0.5)), NA)
    expect_error(random.dag(3,2,function(n) rexp(n, rate = 10)), NA)
    expect_error(random.dag(10,10,function(n) rexp(n, rate = 10)), NA)
    expect_error(random.dag(3,2,function(n) runif(n, min = -5, max = 5)), NA)
    expect_error(random.dag(10,10,function(n) runif(n, min = -5, max = 5)), NA)
    expect_error(random.dag(3,2,function(n) rgamma(n, shape = 10)), NA)
    expect_error(random.dag(10,10,function(n) rgamma(n, shape = 10)), NA)
})

test_that("random.dag always produces a DAG", { # PASSES
    ### This test requires the igraph pkg
    if(requireNamespace("igraph", quietly = TRUE)){
        m <- random.dag(5,5)
        m[m!=0] <- 1
        expect_warning(igraph::topo_sort(igraph::graph.adjacency(m)), NA)
    }
})

test_that("random.dag works in degenerate cases", { # FAILS
    # Null graphs
    expect_error(random.dag(1, 0), NA) # FAILS
    expect_error(random.dag(2, 0), NA) # PASSES
    expect_error(random.dag(3, 0), NA) # PASSES
    expect_error(random.dag(5, 0), NA) # PASSES

    # One edge
    ### nnode = 1 can't have any edges
    expect_error(random.dag(2, 1), NA) # PASSES
    expect_error(random.dag(3, 1), NA) # PASSES
    expect_error(random.dag(5, 1), NA) # PASSES

    # Max edges
    expect_error(random.dag(1, 0), NA) # FAILS
    expect_error(random.dag(2, 1), NA) # PASSES
    expect_error(random.dag(3, 3), NA) # PASSES
    expect_error(random.dag(5, 10), NA) # PASSES
})

