find_objects_by_type <- function(types, name = ".GlobalEnv", ...){
    ### One-liner: ls()[unlist(lapply(lapply(ls(), function(x) class(get(x))), function(x) {"sparsebnPath" %in% x}))]
    ### Better one-liner
    Filter(f = function(x) any(types %in% class(get(x))),
           x = ls(name, ...)
           )

#    ### Get all objects from environment (global only by default)
#     all_objects <- ls(name, ...)
#
#     ### Get classes of all objects
#     all_types <- lapply(all_objects, function(x) class(get(x)))
#
#     ### Match classes against user-input 'types'
#     matched_types <- lapply(all_types, function(x) {any(types %in% x)}) # any will return TRUE as long as at least ONE of the classes owned by an object is in types
#
#     ### Return only those objects that match user input
#     out <- all_objects[unlist(matched_types)]
#     return(out)
}

pkg_change_global_coerce <- function(from_type = c("sparsebnFit", "sparsebnPath"),
                                     envir = .GlobalEnv){
    pkg_graph <- getGraphPackage()
    if(!is.null(pkg_graph)){
        if(pkg_graph == "graph"){
            coerce_global(to_func = "to_graphNEL", from_type, envir)
        } else if(pkg_graph == "igraph"){
            coerce_global(to_func = "to_igraph", from_type, envir)
        } else if(pkg_graph == "network"){
            coerce_global(to_func = "to_network", from_type, envir)
        } else{
            stop(invalid_pkg_specification())
        }
    }
}

coerce_global <- function(to_func,
                          from_type,
                          envir = .GlobalEnv){
    obj_to_convert <- find_objects_by_type(from_type)
    converted <- lapply(obj_to_convert,
                        function(x){
                            do.call(to_func, list(get(x)))
                        })
    mapply(function(from, to) assign(from, to, envir = envir), obj_to_convert, converted)

    invisible()
}
