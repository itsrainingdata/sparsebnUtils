context("sparsebnData")

dat <- generate_fixed_data_frame()

test_that("sparsebnData constructor fails if input not a list, data.frame, or matrix", {
    expect_error(sparsebnData(1L))
    expect_error(sparsebnData(pi))
    expect_error(sparsebnData(rep(1,5)))
})

test_that("sparsebnData constructor fails if type not specified or improperly specified", {
    expect_error(sparsebnData(data = dat), regexp = "type")
    expect_error(sparsebnData(data = dat, type = "contns"), regexp = "Invalid \'type\'")
    expect_error(sparsebnData(data = dat, type = "dicsrt"), regexp = "Invalid \'type\'")

    ### Check that partial matching works OK
    expect_that(sparsebnData(data = dat, type = "c"), not(throws_error()))
    expect_that(sparsebnData(data = dat, type = "cont"), not(throws_error()))
    expect_that(sparsebnData(data = dat, type = "d"), not(throws_error()))
    expect_that(sparsebnData(data = dat, type = "disc"), not(throws_error()))
})

test_that("print.sparsebnData functions properly", {
    expect_output(print(sparsebnData(data = dat, type = "continuous")), regexp = "5 total rows")
    expect_output(print(sparsebnData(data = dat, type = "continuous")), regexp = "Observational")
})
