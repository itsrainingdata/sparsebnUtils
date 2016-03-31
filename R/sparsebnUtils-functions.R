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
#     check_if_matrix
#     check_if_data_matrix
#     check_if_complete_data
#     count_nas
#     list_classes
#     check_list_class
#     col_classes
#     cor_vector
#

# Check if an object is EITHER matrix or Matrix object
check_if_matrix <- function(m){
    is.matrix(m) || inherits(m, "Matrix")
} # END .CHECK_IF_MATRIX

# Check if an object is a valid dataset
check_if_data_matrix <- function(df){
    is.data.frame(df) || check_if_matrix(df)
} # END .CHECK_IF_DATA_MATRIX

# Check if a dataset contains missing data
check_if_complete_data <- function(df){
    (count_nas(df) == 0)
} # END .CHECK_IF_COMPLETE_DATA

# Count missing values in a matrix or data.frame
count_nas <- function(df){
    if( !check_if_data_matrix(df)){
        stop("Input must be a data.frame or a matrix!")
    }

    sum(is.na(df))
} # END .COUNT_NAS

# Return the types for each element in a list
list_classes <- function(li){
    unlist(lapply(li, class))
} # END .LIST_CLASSES

# Return TRUE if every element of a list inherits check.class, FALSE otherwise
check_list_class <- function(li, check.class){
    if(length(li) == 0){
        warning("List contains no elements!")

        TRUE # default to true if empty
    }

    all(unlist(lapply(li, function(x) inherits(x, check.class))))
} # END .CHECK_LIST_CLASS

# Output the class of each column in X, return as a character vector
col_classes <- function(X){
    if( !check_if_data_matrix(X)){
        stop("Input must be a data.frame or a matrix!")
    }

    apply(X, 2, class)
} # END .COL_CLASSES

# Compute the correlation matrix of a dataset, and return the unduplicated elements (i.e. upper-triangular portions) as a vector
#  Used as the primary "carrier of information" in ccdr since the algorithms only depends on pairwise correlations
cor_vector <- function(X){
    check.numeric <- (col_classes(X) != "numeric")
    if( any(check.numeric)){
        not.numeric <- which(check.numeric)
        stop(paste0("Input columns must be numeric! Columns ", paste(not.numeric, collapse = ", "), " are non-numeric."))
    }

    if( any(dim(X) < 2)){
        stop("Input must have at least 2 rows and columns!") # 2-8-15: Why do we check this here?
    }

    cors <- cor(X)
    cors <- cors[upper.tri(cors, diag = TRUE)]

    cors
} # END .COR_VECTOR

# Utility to capitalize the first letter in a string
#  Borrowed verbatim from the 'Hmisc' package
capitalize <- function(string) {
    capped <- grep("^[^A-Z]*$", string, perl = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped],
        1, 1))
    return(string)
} # END CAPITALIZE
