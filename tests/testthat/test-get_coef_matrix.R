context("get_coef_matrix")

# set up input parameters
n_node <- 6
n_levels <- c(2, 3, 4, 2, 3, 2)
node_name <- c("a", "b", "c", "d", "e", "grade")
coef_name <- c("a2", "bmedian", "bhigh","c1", "c5", "c6", "dMale", "emaster", "ePh.D", "grade2")
coef_matrix <- matrix(1:30, nrow=3, byrow = TRUE)
colnames(coef_matrix) <- coef_name

# test
test_that("Inputs are compatible", {
  n_levels_wrong <- c(2, 4, 4, 2, 3, 2)
  expect_error(get_coef_matrix(coef_matrix, node_name, n_levels_wrong))
})

test_that("output is as expected", {

  coef.matrix.out <- get_coef_matrix(coef_matrix, node_name, n_levels)
  expect_equal(dim(coef.matrix.out[[2]]), c(3, 2))
  expect_equal(dim(coef.matrix.out[[3]]), c(3, 3))
  expect_equal(matrix(coef.matrix.out[[2]], nrow=3), matrix(c(2, 3, 12, 13, 22, 23), nrow=3, byrow = TRUE))
  expect_equal(matrix(coef.matrix.out[[3]], nrow=3), matrix(c(4, 5, 6, 14, 15, 16, 24, 25, 26), nrow=3, byrow = TRUE))
  # test if levels names are correct
  expect_equal(colnames(coef.matrix.out[[1]]), "2")
  expect_equal(colnames(coef.matrix.out[[2]]), c("median", "high"))
  expect_equal(colnames(coef.matrix.out[[6]]), "2")
})
