context("fit_multinom_dag")

# set up input variable
data <- matrix(c(1, 1, 0, 0, 1, 1,
                 1, 1, 0, 1, 1, 1,
                 0, 0, 1, 0, 0, 1,
                 0, 0, 1, 0, 0, 1,
                 0, 0, 0, 1, 1, 0,
                 0, 0, 0, 1, 1, 1,
                 1, 1, 1, 1, 0, 0,
                 1, 0, 1, 1, 0, 1,
                 0, 0, 0, 0, 1, 0,
                 1, 1, 1, 1, 0, 1,
                 1, 1, 0, 1, 1, 1,
                 0, 0, 1, 0, 0, 1,
                 1, 1, 0, 1, 0, 0,
                 1, 0, 1, 1, 0, 1,
                 1, 1, 1, 1, 1, 0,
                 1, 0, 1, 1, 1, 1,
                 0, 0, 1, 0, 0, 0,
                 1, 1, 0, 1, 1, 1,
                 1, 1, 1, 0, 0, 0,
                 1, 1, 1, 1, 0, 0,
                 0, 0, 0, 0, 1, 0,
                 0, 0, 1, 0, 0, 0,
                 1, 0, 0, 0, 1, 1,
                 0, 0, 1, 0, 0, 0,
                 1, 0, 1, 1, 0, 1,
                 0, 0, 0, 1, 1, 0,
                 0, 0, 0, 0, 0, 0,
                 0, 0, 1, 0, 1, 0,
                 0, 0, 1, 0, 0, 0,
                 0, 0, 1, 0, 0, 0), byrow = TRUE, ncol = 6)
dataSize <- dim(data)[1]
node <- dim(data)[2]
n_levels <- rep(2, node)
ivn <- vector("list", length = dataSize)
ivn <- lapply(ivn, function(x){return(as.integer(0))})
databn <- sparsebnUtils::sparsebnData(data, ivn = ivn, type = "discrete")

final <- cd.run(databn)
final.dag <- final[[length(final)]]
edge_list<- final.dag$edges
# adj_matrix <- sparsebnUtils::get.adjacency.matrix(final.dag)
# matrix <- matrix(adj_matrix, nrow = 6)

# test
test_that("fit_multinom_dag can take empty graphs", {
  empty_graph <- final[[1]]$edges
  expect_error(fit_multinom_dag(empty_graph, n_levels, data), NA)

  ### test if I input a graph with a single node, will the algorithm work.
  data_single <- as.data.frame(matrix(c(0, 1, 2, 0, 0, 3, 3, 2, 2, 1), nrow=10))
  ### Generate fixed objects for empty graphs
  generate_empty_edgeList <- function(){
    sparsebnUtils::edgeList(list(integer(0)))
  }
  single_node <- generate_empty_edgeList()
  expect_error(fit_multinom_dag(single_node, n_levels=4, data_single), NA)
})

test_that("fit_multinom_dag can run", {
  ### fit_multinom_dag can accept an edgeList object as an input
  expect_error(fit_multinom_dag(edge_list, n_levels, data), NA)

  ### throw an error if fit_multinom_dag has the wrong input
  expect_error(fit_multinom_dag(final.dag, n_levels, data))
})

test_that("fit_multinom_dag output the right result", {
  out <- fit_multinom_dag(edge_list, n_levels, data)

  ### length of output should be the number of variables
  expect_equal(length(out), node)
  ### legnth of each element should be the numeber of parents
  for (i in 1:node) {
    expect_equal(length(out[[i]]), length(edge_list[[i]])+(length(edge_list[[i]])!=0))
  }
  ### randomly check some entries
  expect_equal(dim(out[[2]][[1]]$coef), c(1, 1))
  expect_equal(out[[2]][[1]]$parent, 1)
  expect_equal(dim(out[[2]][[2]]$coef), c(1, 1))
  expect_equal(out[[2]][[2]]$parent, 3)
})
