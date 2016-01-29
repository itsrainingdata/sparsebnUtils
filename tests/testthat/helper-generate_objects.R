generate_empty_edgeList <- function(){
    edgeList.list(list())
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
