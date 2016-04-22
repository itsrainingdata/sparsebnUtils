#
#  sparsebnUtils-messages.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 3/29/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Messages
#
#   CONTENTS:
#       input_not_sparsebnData
#       alg_input_data_frame
#
#

#' @export
input_not_sparsebnData <- function(data){
    sprintf("Input data must be a valid sparsebnData object! <Current type: %s>", class(data))
}

#' @export
alg_input_data_frame <- function(){
    sprintf("Data input as a data.frame: In order to coerce your data to a valid sparsebnData object, we will assume the data is purely observational. In the future, it's best to do this yourself to prevent loss of information.")
}

#' @export
has_missing_values <- function(count){
    sprintf("Data contains %d missing values. Presently, all of the methods in this package require complete data. Please impute these missing values before running any of the learning algorithms.", count)
}

#' @export
invalid_pkg_specification <- function(){
    sprintf("Incorrect package specified. Must be one of: 'graph', 'igraph', 'network'.")
}

#' @export
pkg_not_installed <- function(pkg){
    sprintf("The %s package is required in order to use this method. Please install it first.", pkg)
}
