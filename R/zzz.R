#
#  zzz.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2016 Bryon Aragam. All rights reserved.
#

# Export everything in this package so the other packages can use all of the methods
#' @exportPattern ^[^\\.]

.onAttach <- function(libname, pkgname){
    packageStartupMessage("NOTE: This package is currently in a development state and may be unstable.\n Please report any bugs at https://github.com/itsrainingdata/sparsebnUtils/issues.")
}
