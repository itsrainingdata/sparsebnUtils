#
#  sparsebnUtils-defaults.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 6/2/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Default values for sparsebn package
#
#   CONTENTS:
#       default_max_iters
#       default_alpha
#

#' @export
#' @rdname sparsebn-functions
default_max_iters <- function(numnode){
    2 * max(10, sqrt(numnode))
}

#' @export
#' @rdname sparsebn-functions
default_alpha <- function(){
    10
}
