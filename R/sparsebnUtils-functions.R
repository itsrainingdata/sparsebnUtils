#
#  sparsebnUtils-functions.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Functions
#
#   CONTENTS:
#       check_if_matrix
#       check_if_data_matrix
#       check_if_complete_data
#       check_null
#       check_na
#       count_nas
#       list_classes
#       auto_generate_levels
#       auto_count_levels
#       check_list_class
#       check_list_numeric
#       check_list_names
#       col_classes
#       cor_vector
#       capitalize
#       recode_levels
#       convert_factor_to_discrete
#       format_list
#       pmatch_numeric
#

#' @name sparsebn-functions
#' @rdname sparsebn-functions
#'
#' @param x a compatible object.
#' @param m a \code{matrix}.
#' @param df a \code{data.frame}.
#' @param li a \code{list}.
#' @param check.class \code{character} class name to compare against.
#' @param check.names \code{character} names to compare against.
#' @param X a matrix.
#' @param data a \code{data.frame}.
#' @param ivn list of interventions (see \code{\link{sparsebnData}}).
#' @param string a \code{character} string.
#' @param numnode \code{integer} number of nodes.
#' @param table table of values to compare against.
#' @param tol maximum tolerance used for matching.
#'
#' @title Utility functions
#'
#' @description Various utility functions for packages in the \code{sparsebn} family
#'
NULL

# Check if an object is EITHER matrix or Matrix object
#' @rdname sparsebn-functions
#' @export
check_if_matrix <- function(m){
    is.matrix(m) || inherits(m, "Matrix")
} # END .CHECK_IF_MATRIX

# Check if an object is a valid dataset
#' @rdname sparsebn-functions
#' @export
check_if_data_matrix <- function(df){
    is.data.frame(df) || check_if_matrix(df)
} # END .CHECK_IF_DATA_MATRIX

# Check if a dataset contains missing data
#' @rdname sparsebn-functions
#' @export
check_if_complete_data <- function(df){
    (count_nas(df) == 0)
} # END .CHECK_IF_COMPLETE_DATA

# Check if a dataset contains only numeric (including integer) values
#' @rdname sparsebn-functions
#' @export
check_if_numeric_data <- function(df){
    all(col_classes(df) %in% c("numeric", "integer"))
} # END .CHECK_IF_NUMERIC_DATA

# Check if an object contains any null values
#' @rdname sparsebn-functions
#' @export
check_null <- function(x){
    any(unlist(lapply(x, is.null)))
} # END .CHECK_NULL

# Check if an object contains any missing values
#' @rdname sparsebn-functions
#' @export
check_na <- function(x){
    suppressWarnings(any(unlist(lapply(x, is.na)))) # suppress warning about checking NULLs
} # END .CHECK_NA

# Count missing values in a matrix or data.frame
#' @rdname sparsebn-functions
#' @export
count_nas <- function(df){
    if( !check_if_data_matrix(df)){
        stop("Input must be a data.frame or a matrix!")
    }

    sum(is.na(df))
} # END .COUNT_NAS

# Return the types for each element in a list
#' @rdname sparsebn-functions
#' @export
list_classes <- function(li){
    unlist(lapply(li, class))
} # END .LIST_CLASSES

# Return the different levels for each column in a data.frame, compatible
# with the 'levels' component of sparsebnData
#' @rdname sparsebn-functions
#' @export
auto_generate_levels <- function(df){
    if( !check_if_data_matrix(df)){
        stop("Input must be a data.frame or a matrix!")
    }

    if(!is.data.frame(df)) df <- data.frame(df)
    out <- lapply(df, function(x) sort(unique(x)))

    out
} # END AUTO_GENERATE_LEVELS

# Return the number of levels for each column in a data.frame
#' @rdname sparsebn-functions
#' @export
auto_count_levels <- function(df){
    lapply(auto_generate_levels(df), length)
} # END .COUNT_NAS

# Return TRUE if every element of a list inherits check.class, FALSE otherwise
#' @rdname sparsebn-functions
#' @export
check_list_class <- function(li, check.class){
    if(length(li) == 0){
        warning("List contains no elements!")

        TRUE # default to true if empty
    }

    all(unlist(lapply(li, function(x) inherits(x, check.class))))
} # END .CHECK_LIST_CLASS

# Return TRUE if every element of a list inherits numeric, FALSE otherwise
#  In particular, if every component is integer, will still return TRUE due to quirks of R
#  This is because is.numeric(1L) returns TRUE
#' @rdname sparsebn-functions
#' @export
check_list_numeric <- function(li){
    if(length(li) == 0){
        warning("List contains no elements!")

        TRUE # default to true if empty
    }

    all(unlist(lapply(li, is.numeric)))
} # END .CHECK_LIST_NUMERIC

# Return TRUE if names(list) matches check.names, FALSE otherwise
#' @rdname sparsebn-functions
#' @export
check_list_names <- function(li, check.names){
    if(length(li) == 0){
        warning("List contains no elements!")

        TRUE # default to true if empty
    }

    (length(li) == length(check.names)) && (names(li) == check.names)
} # END .CHECK_LIST_NAMES

# Output the class of each column in X, return as a character vector
#' @rdname sparsebn-functions
#' @export
col_classes <- function(X){
    # if( !check_if_data_matrix(X)){
    #     stop("Input must be a data.frame or a matrix!")
    # }
    stopifnot(is.data.frame(X))

    sapply(X, class)
} # END .COL_CLASSES

# Utility to capitalize the first letter in a string
#  Borrowed verbatim from the 'Hmisc' package
#' @rdname sparsebn-functions
#' @export
capitalize <- function(string) {
    capped <- grep("^[^A-Z]*$", string, perl = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped],
        1, 1))
    return(string)
} # END CAPITALIZE

# Recode a factor to start at 0
#' @rdname sparsebn-functions
#' @export
recode_levels <- function(x){
    stopifnot(is.factor(x))
    levels(x) <- seq(0, length(levels(x)) - 1, 1) # convert levels to 0...k-1

    x
}

# Convert a factor to integer with levels 0...n-1
#' @rdname sparsebn-functions
#' @export
convert_factor_to_discrete <- function(x){
    f <- recode_levels(x)
    as.numeric(levels(f))[f] # convert factor to numeric (see ?factor)
}

# Format a list for pretty printing
format_list <- function(x){
    stopifnot(is.list(x))

    if(is.null(names(x))){
        row_names <- 1L:length(x)
    } else{
        row_names <- names(x)
    }

    ### Adjust width of output based on longest string
    row_width <- max(c(0, nchar(row_names))) + 4        # Row headers
    row_width_str <- paste0("%-", row_width, "s")       #

    cell_width <- max(c(0, nchar(unlist(x)))) + 2       # Cell contents
    cell_width_str <- paste0("%-", cell_width, "s")     #

    ### Assemble string output
    list.out <- mapply(function(x, y){
        prefix <- paste0("[", x, "]")
        # prefix <- sprintf("%-5s", prefix)
        prefix <- sprintf(row_width_str, prefix)
        if(is.numeric(y)) y <- round(y, 2)
        # paste0(prefix, paste(sprintf("%-5s", sort(y)), collapse = ""))
        paste0(prefix, paste(sprintf(cell_width_str, sort(y)), collapse = ""))
    }, row_names, x)
    list.out <- unlist(list.out)
    list.out <- paste(list.out, collapse = " \n")
    list.out <- paste0(list.out, "\n") # add trailing newline

    list.out
}

# Compute the correlation matrix of a dataset, and return the unduplicated elements (i.e. upper-triangular portions) as a vector
#  Used as the primary "carrier of information" in ccdr since the algorithms only depends on pairwise correlations
#
# NOTE: Should be deprecated at this point, but needs further testing.
#' @rdname sparsebn-functions
#' @export
cor_vector <- function(data){
    # .Deprecated()

    check.numeric <- (col_classes(data) != "numeric")
    if( any(check.numeric)){
        not.numeric <- which(check.numeric)
        stop(paste0("Input columns must be numeric! Columns ", paste(not.numeric, collapse = ", "), " are non-numeric."))
    }

    if( any(dim(data) < 2)){
        stop("Input must have at least 2 rows and columns!") # 2-8-15: Why do we check this here?
    }

    cors <- stats::cor(data)
    cors <- cors[upper.tri(cors, diag = TRUE)]

    cors
} # END COR_VECTOR

# Migrated from ccdrAlgorithm package
#' @rdname sparsebn-functions
#' @export
cor_vector_ivn <- function(data, ivn){
    check.numeric <- check_if_numeric_data(data)
    if(!check.numeric){
        check.numeric <- (col_classes(data) %in% c("numeric", "integer"))
        not.numeric <- which(!check.numeric)
        stop(data_not_numeric(not.numeric))
    }

    if( any(dim(data) < 2)){
        stop("Input must have at least 2 rows and columns!") # 2-8-15: Why do we check this here?
    }

    pp <- ncol(data)
    ## unique(unlist(list(NULL, NULL, ...))) returns NULL without error
    ## so checking list(NULL, NULL, ...) is equivalent to checking if ivnlabels is NULL
    ivnlabels <- unique(unlist(ivn))
    if(is.null(ivnlabels)) {
        ## i.e. purely observational
        cors <- stats::cor(data)
        cors <- cors[upper.tri(cors, diag = TRUE)]
        return(list(cors = cors, indexj = rep(0L, pp + 1)))
    } else {
        ## so there are at least some interventions
        ivnj <- as.integer(c(ivnlabels, pp + 1))
        # get all the j's that has interventions
        # including pp+1 for observational rows (compatible with purely observational data)
        # is this necessary if most are balanced designs where all nodes get intervention?
        # any possible optimization?
        len <- length(ivnj)
        cors <- vector("list", len)
        indexj <- as.integer(rep(len - 1, pp + 1)) # zero-based index for C compatibility
        for(j in 1:(len - 1)) {
            jj <- ivnj[j]
            indexj[jj] <- j - 1
            ## extract rows where node jj has no intervetion
            corsjj <- stats::cor(data[!sapply(lapply(ivn, is.element, jj), any), , drop = FALSE])
            cors[[j]] <- corsjj[upper.tri(corsjj, diag = TRUE)]
        }
        corsjj <- stats::cor(data[!sapply(lapply(ivn, is.element, pp + 1), any), , drop = FALSE])
        ## do not change above line to cor(data[sapply(ivn, is.null), ])
        ## why?
        cors[[len]] <- corsjj[upper.tri(corsjj, diag = TRUE)]
        cors <- unlist(cors)
        return(list(cors = cors, indexj = indexj))
    }
} # END COR_VECTOR_IVN

# Partial matching for numeric values
#  Returns the value in table closest to x, unless the minimum absolute distance
#  exceeds tol (in which case NA is returned).
#
#' @rdname sparsebn-functions
#' @export
pmatch_numeric <- function(x, table, tol = 0.1){
    absdiff <- abs(table - x)
    idx <- which.min(absdiff)

    if(absdiff[idx] < tol){
        idx
    } else{
        NA
    }
} # END PMATCH_NUMERIC

