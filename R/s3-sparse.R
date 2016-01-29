#
#  s3-sparse.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# sparse S3 Class for R
#------------------------------------------------------------------------------#

#
# An alternative data structure for storing sparse matrices in R using the (row, column, value)
#   format. Internally it is stored as a list with three components, each vectors, that contain
#   the rows / columns / values of the nonzero elements.
#
# Its main purpose is to serve as an intermediary between the standard R dense matrix class and the
#   internal SparseBlockMatrixR class. That is, to convert from matrix to SBM, we do
#
#       matrix -->> sparse -->> SparseBlockMatrixR
#
# In theory, this class can be used externally as a useful data structure for storing sparse matrices
#   as an alternative to the Matrix class provided by the Matrix package. Currently, however, the class
#   structure is fairly limited, so there isn't much a reason to do this.
#
#

#------------------------------------------------------------------------------#
# is.sparse
#
is.sparse <- function(sp){
    inherits(sp, "sparse")
} # END IS.SPARSE

#------------------------------------------------------------------------------#
# reIndexC.sparse
#  Re-indexing TO C for sparse objects
#
reIndexC.sparse <- function(sp){
    if(sp$start == 0){
        warning("This object already uses C-style indexing!")
        return(sp)
    }

    sp$rows <- sp$rows - 1
    sp$cols <- sp$cols - 1
    sp$start <- 0

    sp
} # END REINDEXC.SPARSE

#------------------------------------------------------------------------------#
# reIndexR.sparse
#  Re-indexing TO R for sparse objects
#
reIndexR.sparse <- function(sp){
    if(sp$start == 1){
        warning("This object already uses R-style indexing!")
        return(sp)
    }

    sp$rows <- sp$rows + 1
    sp$cols <- sp$cols + 1
    sp$start <- 1

    sp
} # END REINDEXR.SPARSE

#------------------------------------------------------------------------------#
# sparse.list
#  List constructor
#
sparse.list <- function(li){

    if( !is.list(li)){
        stop("Input must be a list!")
    }

    if( length(li) != 5 || names(li) != c("rows", "cols", "vals", "dim", "start") || is.null(names(li))){
        stop("Input is not coercable to an object of type sparse, check list for the following (named) elements: rows, cols, vals, dim, start")
    }

    if( length(unique(lapply(li[1:3], length))) > 1){
        stop("rows / cols / vals elements have different sizes; should all have the same length (pp)!!")
    }

    if(length(li$dim) != 2){
        stop("dim attribute must have length 2!")
    }

    if(li$start != 0 && li$start != 1){
        stop("start attribute must be 0 (C-style) or 1 (R-style)!")
    }

    if(!is.integer(li$rows) || !is.integer(li$cols)){
        stop("rows / cols must both be integers!")
    }

    if(!is.numeric(li$vals)){
        stop("vals must be numeric!")
    }

    structure(li, class = "sparse")
} # END SPARSE.LIST

#------------------------------------------------------------------------------#
# sparse.matrix
#
sparse.matrix <- function(m, index = "R"){
    if( nrow(m) != ncol(m)) stop("Input matrix must be square!") # 2-7-15: Why does it need to be square?

    if(index != "R" && index != "C") stop("Invalid entry for index parameter: Must be either 'R' or 'C'!")

    pp <- nrow(m)

    nnz <- which(abs(m) > .MACHINE_EPS) - 1
    vals <- double(length(nnz))
    rows <- integer(length(nnz))
    cols <- integer(length(nnz))
    for(k in seq_along(nnz)){
        col <- trunc(nnz[k] / pp)
        row <- nnz[k] - (pp * col)
        vals[k] <- as.vector(m)[nnz[k] + 1]
        rows[k] <- row
        cols[k] <- col
    }

    sp <- sparse.list(list(rows = as.integer(rows), cols = as.integer(cols), vals = as.numeric(vals), dim = c(pp, pp), start = 0))

    if(index == "R"){
        reIndexR(sp)
    } else{
        sp
    }
} # END SPARSE.MATRIX

#------------------------------------------------------------------------------#
# as.sparse.list
#  Convert FROM list TO sparse
#
as.sparse.list <- function(li){
    sparse.list(li)
} # END AS.SPARSE.LIST

#------------------------------------------------------------------------------#
# as.sparse.matrix
#  Convert FROM matrix TO sparse
#  By default, return the object using R indexing. If desired, the method can return C-style indexing by setting
#    index = "C".
as.sparse.matrix <- function(m, index = "R"){
    sparse.matrix(m, index)
} # END AS.SPARSE.MATRIX

#------------------------------------------------------------------------------#
# as.matrix.sparse
#  Convert FROM sparse TO matrix
#
as.matrix.sparse <- function(sp){

    if( !is.sparse(sp)){
        stop("Input must be a sparse object!")
    }

    if(sp$start == 0) sp <- reIndexR(sp) # if indexing starts at 0, adjust to start 1 instead

    m.dim <- sp$dim
    m <- matrix(0, nrow = m.dim[1], ncol = m.dim[2])

    for(k in seq_along(sp$vals)){
        m[sp$rows[k], sp$cols[k]] <- sp$vals[k]
    }

    attributes(m)$dim <- sp$dim
    # attributes(m)$dimnames <- list()
    rownames(m) <- as.character(1:nrow(m))
    colnames(m) <- as.character(1:ncol(m))

    m
} # END AS.MATRIX.SPARSE

#------------------------------------------------------------------------------#
# as.list.sparse
#  Convert FROM sparse TO list
#
as.list.sparse <- function(sp){

    list(rows = sp$rows, cols = sp$cols, vals = sp$cols, dim = sp$dim, start = sp$start)
} # END AS.LIST.SPARSE

#------------------------------------------------------------------------------#
# print.sparse
#  Print function for sparse objects
#  By default, format the output as a three-column matrix [cols | rows | vals] ordered by increasing columns.
#    Optionally, set pretty = FALSE to print the sparse object as a list.
print.sparse <- function(sp, pretty = TRUE){
    if(pretty){
        out <- cbind(sp$cols, sp$rows, sp$vals)
        colnames(out) <- c("cols", "rows", "vals")
        print(out)
    } else{
        print(as.list(sp))
    }

} # END PRINT.SPARSE

#------------------------------------------------------------------------------#
# is.zero.sparse
#  Check to see if a sparse object represents the zero matrix
#
is.zero.sparse <- function(x){
    check_if_zero <- (length(x$rows) == 0)

    check_if_zero
} # END IS.ZERO.SPARSE

#------------------------------------------------------------------------------#
# .num_edges.sparse
# Internal function for returning the number of edges in a sparse object
#
.num_edges.sparse <- function(sp){
    ### Testing only for now
    if(length(which(abs(sp$vals) > .MACHINE_EPS)) != length(sp$rows)){
        stop("Error in .num_edges.sparse! Please check source code.")
    }

    length(which(abs(sp$vals) > .MACHINE_EPS))
} # END .NUM_EDGES.SPARSE
