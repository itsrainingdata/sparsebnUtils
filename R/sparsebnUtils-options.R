#
#  sparsebnUtils-options.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 4/24/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Methods for getting and setting package options
#
#   CONTENTS:
#       resetGraphPackage
#       setGraphPackage
#       getGraphPackage
#       setPlotPackage
#       getPlotPackage
#       zero_threshold
#       get_option
#       set_option
#

#' @describeIn setGraphPackage Reset all data to default \code{\link{edgeList}} format and set graph package back to default \code{"sparsebn"}.
#' @export
resetGraphPackage <- function(coerce = TRUE){
    setGraphPackage(NULL, coerce = coerce)
}


#' Change data structure for representing graphs internally
#'
#' Changes the output of the main algorithms to be compatible with other packages in the R ecosystem.
#'
#' \code{sparsebn} is compatible with four different data structures for representing graphs:
#' \link{edgeList} (default), \link[graph]{graphNEL-class} (from the \code{graph} package),
#' \link[igraph]{igraph} (from the \link[igraph]{igraph} package), and \link[network]{network} (from
#' \link[network]{network-package}). \link{edgeList} is provided by default in \code{sparsebn}, however,
#' the other three options require that extra packages are installed.
#'
#' @param pkg The desired package; default value is \code{NULL} corresponding to \link{edgeList}. Possible values are \code{"sparsebn"}, \code{"igraph"}, \code{"graph"}, \code{"bnlearn"}, and \code{"network"}.
#' @param matchPlot Force the underlying plotting mechanism to match the selected package (see \link{setPlotPackage}).
#' @param coerce If \code{TRUE}, then all \code{\link{sparsebnFit}} and \code{\link{sparsebnPath}} objects in the global environment will be coerced to be compatible with the selected package. This will overwrite your existing data.
#'
#' @seealso \code{\link{setPlotPackage}}, \code{\link{getPlotPackage}}
#'
#' @export
setGraphPackage <- function(pkg,
                            matchPlot = TRUE,
                            coerce = FALSE){
    if(!is.null(pkg)){
        ### Check that input is currently supported
        pkg <- match.arg(pkg, c("sparsebn", "igraph", "graph", "network"))

        ### Check that required package is installed
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(pkg_not_installed(pkg = pkg), call. = FALSE)
        }

        ### Set plot package to match graph package by default
        if(matchPlot){
            if(pkg == "sparsebn"){
                stop("Cannot set matchPlot = TRUE with pkg = 'sparsebn'! For plotting, you must use one of 'igraph', 'graph', or 'network'.")
            }
            setPlotPackage(pkg = pkg)
        }
    }

    ### Must change option BEFORE attempting coercion (see pkg_change_global_coerce)
    set_option("sparsebn.graph", pkg)

    if(coerce){
        warning(global_coerce_warning(pkg))
        tryCatch({
            pkg_change_global_coerce()
        }, error = function(c){ stop(c)})
    }

}

#' @describeIn setGraphPackage Returns the current choice of graph package ( \code{NULL} corresponds to no selection)
#' @export
getGraphPackage <- function(){
    get_option("sparsebn.graph")
}

#' Change default plotting mechanism
#'
#' Changes the default plotting mechanism used by \code{sparsebn} to plot output and fitted objects.
#'
#' For plotting, \code{sparsebn} can use one of three packages: \code{graph} (see also \code{Rgraphviz}),
#' \link[igraph]{igraph} (see \link[igraph]{plot.igraph}), and \link[network]{network-package} (see \link[network]{plot.network}).
#' Note that plotting requires that (at least one of) these extra packages are installed.
#'
#' @param pkg The desired package; default value is \code{igraph}.
#'
#' @seealso \code{\link{setGraphPackage}}, \code{\link{getGraphPackage}}
#'
#' @export
setPlotPackage <- function(pkg){
    ### Check that input is currently supported
    pkg <- match.arg(pkg, c("igraph", "graph", "network"))

    set_option("sparsebn.plotting", pkg)
}

#' @describeIn setPlotPackage Returns the current choice of plotting mechanism
#' @export
getPlotPackage <- function(){
    get_option("sparsebn.plotting")
}

#' @rdname sparsebn-functions
#' @export
zero_threshold <- function(){
    get_option("sparsebn.zerothreshold")
}

set_option <- function(opt, val){
    opt_to_set <- list()
    opt_to_set[opt] <- list(val)
    options(opt_to_set)
    invisible()
}

get_option <- function(opt){
    options()[[opt]]
}
