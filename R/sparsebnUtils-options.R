#' @export
setGraphPackage <- function(pkg){
    ### Need to add checks for packages
    set_option("sparsebn.graph", pkg)
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

set_option <- function(opt, val){
    opt_to_set <- list()
    opt_to_set[opt] <- val
    options(opt_to_set)
    invisible()
}

get_option <- function(opt){
    options()[[opt]]
}
