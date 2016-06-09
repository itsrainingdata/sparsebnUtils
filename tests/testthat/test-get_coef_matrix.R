context("get_coef_matrix")

# set up input parameters
n_node <- 6
n_levels <- c(2, 3, 4, 2, 3, 2)
coef_vec <- 1:10
coef_matrix <- matrix(1:30, nrow=3, byrow = TRUE)

# test
test_that("Inputs are compatible", {
  n_levels_wrong <- c(2, 4, 4, 2, 3, 2)
  expect_error(get_coef_matrix(coef_vec, n_levels_wrong))
  expect_error(get_coef_matrix(coef_matrix, n_levels_wrong))
})

test_that("output is as expected", {
  coef.out <- get_coef_matrix(coef_vec, n_levels)
  expect_equal(length(coef.out), n_node)
  expect_equal(dim(coef.out[[1]]), c(1, 1))
  expect_equal(dim(coef.out[[3]]), c(1, 3))
  expect_equal(coef.out[[1]], matrix(1, 1, 1))
  expect_equal(coef.out[[3]], matrix(c(4, 5, 6), nrow=1))

  coef.matrix.out <- get_coef_matrix(coef_matrix, n_levels)
  expect_equal(dim(coef.matrix.out[[2]]), c(3, 2))
  expect_equal(dim(coef.matrix.out[[3]]), c(3, 3))
  expect_equal(coef.matrix.out[[2]], matrix(c(2, 3, 12, 13, 22, 23), nrow=3, byrow = TRUE))
  expect_equal(coef.matrix.out[[3]], matrix(c(4, 5, 6, 14, 15, 16, 24, 25, 26), nrow=3, byrow = TRUE))
})
