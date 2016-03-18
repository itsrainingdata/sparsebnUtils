#
#  s3-sparsebnData.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/28/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# sparsebnData S3 Class for R
#------------------------------------------------------------------------------#

#
# sparsebnData S3 class skeleton
#
# Data
# * data.frame data     // data
# * list ivn            // list of nodes under intervention for each row (observation)
#

#
# A convenience class for keeping track of which nodes are under intervention in a dataset. Each
#  component of the list 'ivn' is an integer vector indicating which nodes (columns) were under
#  intervention for the corresponding row. Note that 'ivn' must be a list in order to accommodate
#  the possibility that multiple nodes are under intervention, and different nodes may have
#  different treatments (i.e. number and/or identity of manipulated nodes).
#
# Examples:
#  1) sbd$ivn[1] = NULL: No nodes were under intervention for the first observation
#  2) sbd$ivn[10] = c(1): The first node was under intervention for the tenth observation
#  3) sbd$ivn[120] = c(1,5,500): The 1st, 5th, and 500th nodes were under intervention for the 120th observation
#

#' sparsebnData class
#'
#' Convenience wrapper class for observational and experimental data.
#'
#' Also inherits from \code{\link{list}}.
#'
#'
#' @section Methods:
#' \code{\link{first.method.name}}
#' \code{\link{second.method.name}}
#'
#' @docType class
#' @name sparsebnData-class
NULL

#' @export
is.sparsebnData <- function(sbd){
    inherits(sbd, "sparsebnData")
} # END IS.SPARSEBNDATA

# sparsebnData constructor
sparsebnData.list <- function(li){

    if( !is.list(li)){
        stop("Input must be a list!")
    } else if( length(li) != 2 || !setequal(names(li), c("data", "ivn"))){
        stop("Input is not coercable to an object of type sparsebnFit, check list for the following elements: data (data.frame), ivn (list)")
    } else if( !sparsebnUtils::check_if_data_matrix(li$data)){
        stop(sprintf("Component 'data' must be a valid data.frame or numeric object! <Current type: %s>", class(li$data)))
    } else if(nrow(li$data) != length(li$ivn)){
        stop("The length of the ivn list must equal the number of rows in the data!")
    }

    ### Final output
    structure(li, class = "sparsebnData")
} # END SPARSEBNDATA.LIST

# sparsebnData constructor
#  Default constructor for data.frame input
sparsebnData.data.frame <- function(data){

    #
    # If the input is a pure data.frame, ASSUME all rows are observational. If the data
    #  is experimental, the user needs to specify this by passing in 'ivn' (see sparsebnData.list).
    #

    ivn <- vector("list", length = nrow(data))

    ### Final output
    sparsebnData.list(list(data = data, ivn = ivn))
}

# sparsebnData constructor
#  Default constructor for matrix input
sparsebnData.matrix <- function(data){
    sparsebnData.data.frame(as.data.frame(data))
}

num.samples.sparsebnData <- function(sbd){
    nrow(sbd$data)
}

# Default print method
print.sparsebnData <- function(sbd){
    print(sbd$data)
}
