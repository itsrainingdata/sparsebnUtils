context("col_classes")

test_that("col_classes behaves as expected on matrices and dfs", {
    ### Integer matrix
    m <- matrix(c(1L, 2L, 3L, 5L), ncol = 2)
    m <- as.data.frame(m)
    expect_error(col_classes(m), NA)
    expect_equivalent(col_classes(m), c("integer", "integer"))

    ### Numeric data.frame
    m <- matrix(c(1, 2, 3, 5), ncol = 2)
    m <- as.data.frame(m)
    expect_error(col_classes(m), NA)
    expect_equivalent(col_classes(m), c("numeric", "numeric"))
})

test_that("col_classes throws error for improper input", {
    expect_error(col_classes(list()), "is.data.frame")

    m <- matrix(c(1, 2, 3, 5), ncol = 2)
    expect_error(col_classes(as.numeric(m)), "is.data.frame")
})
