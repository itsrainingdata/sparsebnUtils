#
#  sparsebnUtils-cytoscape.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 9/1/17.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Compatibility with Cytoscape app
#
#   CONTENTS:
#       openCytoscape
#       showCytoscape
#

#' Display graphs in Cytoscape
#'
#' NOTE: This method is currently experimental and under development!
#'
#' Displays the selected graph in the Cytoscape application. Note that this
#' requires that Cytoscape is installed on the user's system, and that the
#' RCy3 package is installed and properly configured. Cytoscape can be
#' downloaded at \href{http://www.cytoscape.org/}{http://www.cytoscape.org/}.
#'
#' @param x A \code{\link{sparsebnFit}} object or other graph object.
#' @param title A character string, this is the name you will see on the Cytoscape
#'              network window. Multiple windows with the same name are not
#'              permitted. See \code{\link[RCy3]{createNetworkFromGraph}} for more
#'              details.
#' @param ... Other arguments to \code{\link[RCy3]{createNetworkFromGraph}}.
#'
#' @export
openCytoscape  <- function(x, title, ...){
    ### This function requires the 'igraph' package to be installed
    if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("This method requires the igraph package; please install it first via install.packages('igraph').", call. = FALSE)
    }

    UseMethod("openCytoscape", x)
} # END OPENCYTOSCAPE

#' @export
openCytoscape.sparsebnPath <- function(x, title, ...){
    stop("Currently, this method only works on individual networks, and is not yet implemented for solution paths. Please select one network from the path instead (see ?select, ?select.parameter). Stay tuned, there will be an update for solution paths soon!")
} # END OPENCYTOSCAPE.SPARSEBNPATH

#' @export
openCytoscape.sparsebnFit <- function(x, title, ...){
    ### Convert to graphNEL object, which is needed for RCy3
    # graph <- to_igraph(x)
    # graph <- igraph::as_graphnel(graph$edges)
    graph <- to_graphNEL(x$edges)

    ### NOTE: Need to remove metadata based on current implementation of to_igraph
    ### TODO: Update this to handle the metadata more gracefully
    graph@edgeData@defaults <- list()
    graph@edgeData@data <- list()

    showCytoscape(graph, title, ...)
} # END OPENCYTOSCAPE.SPARSEBNFIT

#' @export
openCytoscape.default <- function(x, title, ...){
    ### Convert to graphNEL object, which is needed for RCy3
    # graph <- to_igraph(x)
    # graph <- igraph::as_graphnel(graph)
    graph <- to_graphNEL(x)

    ### NOTE: Need to remove metadata based on current implementation of to_igraph
    ### TODO: Update this to handle the metadata more gracefully
    graph@edgeData@defaults <- list()
    graph@edgeData@data <- list()

    showCytoscape(graph, title, ...)
} # END OPENCYTOSCAPE.DEFAULT

### Utility function to handle all RCy3-related functions including
### opening cytoscape and displaying the graph
###  Expects graphNEL input; openCytoscape handles any and all conversions
showCytoscape <- function(graph, title, ...){
    ### This function requires the 'graph' package to be installed
    if (!requireNamespace("graph", quietly = TRUE)) {
        stop("This method requires the graph package; please install it first via BioConductor.", call. = FALSE)
    }

    ### This function requires the 'graph' package to be installed
    if (!requireNamespace("RCy3", quietly = TRUE)) {
        stop("This method requires the RCy3 package to connect to Cytoscape; please install it first via BioConductor.", call. = FALSE)
    }

    stopifnot(inherits(graph, "graphNEL"))

    ### Open cytoscape window and show graph
    # cytowin <- RCy3::CytoscapeWindow(title = title,
    #                                  graph = graph,
    #                                  ...)
    cytowin <- RCy3::createNetworkFromGraph(title = title,
                                            graph = graph,
                                            ...)

    # RCy3::displayGraph(cytowin)
    RCy3::layoutNetwork(network = cytowin, layout.name = "hierarchical")
} # END SHOWCYTOSCAPE
