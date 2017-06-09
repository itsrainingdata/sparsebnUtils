#
#  sparsebnUtils-compatibility.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 6/9/17.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
#

#
# PACKAGE SPARSEBNUTILS: Compatibility
#
#   Currently this is just a placeholder for documentation of various
#   compatibility methods.
#

#' @name sparsebn-compat
#' @rdname sparsebn-compat
#'
#' @title Conversion between graph types
#'
#' @description
#' These methods convert graph objects (e.g. \code{\link{edgeList}})
#' and objects containing graph data (e.g. \code{\link{sparsebnFit}},
#' \code{\link{sparsebnPath}}) to other formats including \code{\link[igraph]{igraph}},
#' \code{\link[graph]{graphNEL}}, \code{\link[network]{network}}, and
#' \code{\link[bnlearn]{bn-class}}.
#'
#' Only graph objects are modified with these methods. For example, if the input
#' is either \code{\link{sparsebnFit}} or \code{\link{sparsebnPath}},
#' the output will still be a \code{\link{sparsebnFit}} or \code{\link{sparsebnPath}}
#' object. Only the \code{edges} slots will be converted to a different graph type.
#' This will be the case for the default output from \code{\link[sparsebn]{estimate.dag}},
#' so that metadata from the learning phase is not lost during conversion.
#' If, on the other hand, the input is already an \code{\link{edgeList}}, then the
#' output will directly be a graph object.
#'
#' @param x An object of type \code{\link{sparsebnPath}}, \code{\link{sparsebnFit}},
#' \code{\link{edgeList}}, \code{\link[igraph]{igraph}}, \code{\link[graph]{graphNEL}},
#' \code{\link[network]{network}}, or \code{\link[bnlearn]{bn-class}}.
#'
#' @details
#' \code{to_igraph} converts sparsebn objects to \code{\link[igraph]{igraph}}-compatible
#' objects.
#'
#' \code{to_graph} converts sparsebn objects to \code{\link[graph]{graphNEL}}-compatible
#' objects.
#'
#' \code{to_network} converts sparsebn objects to \code{\link[network]{network}}-compatible
#' objects.
#'
#' \code{to_bn} converts sparsebn objects to \code{\link[bnlearn]{bn-class}}-compatible
#' objects.
#'
#' @examples
#' ### Learn the cytometry network
#' cyto.data <- sparsebnData(cytometryContinuous[["data"]],
#'                           type = "continuous",
#'                           ivn = cytometryContinuous[["ivn"]])
#' cyto.learn <- estimate.dag(data = cyto.data)
#'
#' ### The output is a sparsebnPath object, which is a list of sparsebnFit objects
#' class(cyto.learn)
#' class(cyto.learn[[1]])
#'
#' ### Convert to igraph
#' cyto.igraph <- to_igraph(cyto.learn)
#' class(cyto.igraph)       # not an igraph object!
#' class(cyto.igraph[[1]]$edges) # the graph data in the 'edges' slot is converted to igraph
#' gr <- cyto.igraph[[1]]$edges
#'
#' ### Different behaviour when input is already an edgeList
#' edgeL <- cyto.learn[[1]]$edges
#' gr <- to_igraph(edgeL) # input is edgeList, not sparsebnFit or sparsebnPath
#' class(gr)              # igraph object
#'
NULL
