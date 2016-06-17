#
#  sparsebnUtils-show.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 6/17/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Methods for showing / printing various object attributes
#
#   CONTENTS:
#       show.parents
#

#' @export
show.parents <- function(x, nodes){
    stopifnot(is.sparsebnFit(x))

    ### Convert edgeList to plain list with desired data
    edges.str <- edgeList_to_node_names(x, 4)   # convert edgeList to use node names
    match.nodes <- match(nodes, x$nodes)        # find the nodes of interest
    out <- as.list(edges.str)[match.nodes]      # extract the parent lists of the nodes of interest
    names(out) <- x$nodes[match.nodes]          # add names attribute to list to print out node names as row names (see format_list)

    ### Print output
    cat(format_list(out))
}
