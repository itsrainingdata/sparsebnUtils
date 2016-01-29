### Ensure random.dag.matrix function is available to all tests
library(pcalg)
random.dag.matrix <- function(pp, nedge){
    g <- randomDAG(n = pp, prob = 2 * nedge / (pp * (pp - 1)), lB = -5.0, uB = 5.0) # Note that the edge weights are selected at random here!
    pi <- sample(1:pp) # permutation ordering
    g <- as(g, "matrix")
    m <- g[pi, pi]
    colnames(m) <- rownames(m) <- as.character(1:pp)

    m
}

### Generate a random sparse matrix with <= s nonzero elements (returned as a matrix object)
random.sparse <- function(dim, s, diag = TRUE){
    if(length(dim) == 1){
        nrow <- ncol <- dim
    } else if(length(dim) == 2){
        nrow <- dim[1]
        ncol <- dim[2]
    } else{
        stop("dim must have either one or two components!")
    }

    m <- matrix(0, nrow = nrow, ncol = ncol)
    for(i in 1:s){
        m[sample(1:nrow, 1), sample(1:ncol, 1)] <- rnorm(1)
    }

    if(!diag) diag(m) <- rep(0, ncol)

    m
}
