#' @export
setGraphPackage <- function(pkg,
                            matchPlot = TRUE,
                            coerce = FALSE){
    if(!is.null(pkg)){
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(pkg_not_installed(pkg = pkg), call. = FALSE)
        }

        ### Set plot package to match graph package by default
        if(matchPlot) setPlotPackage(pkg = pkg)
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

#' @export
getGraphPackage <- function(){
    get_option("sparsebn.graph")
}

#' @export
setPlotPackage <- function(pkg){
    ### Need to add checks for packages
    set_option("sparsebn.plotting", pkg)
}

#' @export
getPlotPackage <- function(){
    get_option("sparsebn.plotting")
}

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
