context("sparsebnData")

dat <- generate_fixed_data_frame()

test_that("sparsebnData constructor fails if input not a list, data.frame, or matrix", {
    expect_error(sparsebnData(1L))
    expect_error(sparsebnData(pi))
    expect_error(sparsebnData(rep(1,5)))
})

test_that("sparsebnData constructor fails if type not specified", {
    expect_error(sparsebnData(data = dat), regexp = "type")
})

test_that("print.sparsebnData functions properly", {
    expect_output(print(sparsebnData(data = dat, type = "continuous")), regexp = "5 total rows")
    expect_output(print(sparsebnData(data = dat, type = "continuous")), regexp = "Observational")
})
