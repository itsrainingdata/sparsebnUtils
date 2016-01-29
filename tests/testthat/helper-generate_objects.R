generate_empty_edgeList <- function(){
    edgeList.list(list())
}

generate_empty_SparseBlockMatrixR <- function(){
    li <- list(rows = list(), vals = list(), blocks = list(), sigmas = c(), start = 1)
    SparseBlockMatrixR.list(li)
}

generate_empty_sparsebnFit <- function(){
    li <- list(sbm = generate_empty_SparseBlockMatrixR(), lambda = 1, nedge = 0, pp = 0, nn = 10, time = 1)
    sparsebnFit.list(li)
}

generate_empty_sparsebnPath <- function(){
    cf <- generate_empty_sparsebnFit()
    sparsebnPath.list(list(cf, cf, cf, cf))
}

generate_empty_adjacency_matrix <- function(){
    m <- matrix(0, nrow = 0, ncol = 0)
    m
    # Matrix::Matrix(m)
}

### Generate fixed objects for the following toy DAG
#
# 0 . . . .
# 1 0 . . .
# 0 1 0 . .
# 5 4 0 . .
# 0 3 0 0 .
#
generate_fixed_edgeList <- function(){
    nnode <- 5
    li <- vector("list", length = nnode)
    li[[1]] <- c(2L,4L)
    li[[2]] <- c(3L,4L,5L)
    li[[3]] <- integer(0)
    li[[4]] <- integer(0)
    li[[5]] <- integer(0)
    edgeL <- edgeList.list(li)

    edgeL
}

generate_fixed_SparseBlockMatrixR <- function(){
    nnode <- 5
    li <- list(rows = vector("list", length = nnode),
               vals = vector("list", length = nnode),
               blocks = vector("list", length = nnode),
               sigmas = numeric(nnode),
               start = 1)

    ### Parents / rows
    li$rows[[1]] <- c(2L,4L)
    li$rows[[2]] <- c(3L,4L,5L)
    li$rows[[3]] <- integer(0)
    li$rows[[4]] <- integer(0)
    li$rows[[5]] <- integer(0)

    ### Values
    li$vals[[1]] <- c(1,5)
    li$vals[[2]] <- c(1,4,3)
    li$vals[[3]] <- integer(0)
    li$vals[[4]] <- integer(0)
    li$vals[[5]] <- integer(0)

    ### Blocks -- LEAVE EMPTY

    ### Sigmas
    li$sigmas <- rep(1,5)

    SparseBlockMatrixR.list(li)
}

generate_fixed_sparsebnFit <- function(){
    sbm <- generate_fixed_SparseBlockMatrixR()
    cf <- sparsebnFit.list(list(sbm = sbm, lambda = 1.54, nedge = num.edges(sbm), pp = num.nodes(sbm), nn = 10, time = 1))

    cf
}

generate_fixed_sparsebnPath <- function(){
    cf <- generate_fixed_sparsebnFit()
    cp <- sparsebnPath.list(list(cf, cf, cf, cf))

    cp
}

generate_fixed_adjacency_matrix <- function(){
    ### CCDr output is unweighted adjacency matrix by default
    m <- rbind(c(0, 0, 0, 0, 0),
               c(1, 0, 0, 0, 0),
               c(0, 1, 0, 0, 0),
               c(1, 1, 0, 0, 0),
               c(0, 1, 0, 0, 0))
    m
    # Matrix::Matrix(m)
}
