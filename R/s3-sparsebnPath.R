#
#  s3-sparsebnPath.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/22/16.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# sparsebnPath S3 Class for R
#------------------------------------------------------------------------------#

#
# sparsebnPath S3 class skeleton
#
# Data
# * <wrapper for a list>
#
# Methods
# * is.sparsebnPath
# * sparsebnPath.list
# * print.sparsebnPath
# * as.list.sparsebnPath
# * num.nodes.sparsebnPath
# * num.edges.sparsebnPath
# * num.samples.sparsebnPath
# * get.lambdas.sparsebnPath
# * get.adjacency.matrix.sparsebnPath
#

#' sparsebnPath class
#'
#' Convenience wrapper class for solution paths of DAG learning algorithms: This class represents an entire
#' solution path of an algorithm. Its components are of type \code{\link{sparsebnFit}}. Also inherits
#' from \code{\link{list}}.
#'
#' Each value of lambda in the (discrete) solution path corresponds to a single DAG estimate (see \href{http://jmlr.org/papers/v16/aragam15a.html}{Aragam and Zhou (2015)} for details).
#' Internally, this estimate is represented by a \code{\link{sparsebnFit}} object. The full solution
#' path is then represented as a \code{\link{list}} of \code{\link{sparsebnFit}} objects: This class is essentially a wrapper for this list.
#'
#' Most methods for \code{\link{sparsebnPath}} objects simply apply \code{\link{lapply}} to the
#' object in question. The exceptions to this rule apply when the output will always be the same
#' for every component; e.g. \code{\link{num.nodes}} and \code{\link{num.samples}}.
#'
#' @param x A \code{list} or an object of type \code{sparsebnPath}. Should only be used internally.
#' @param ... (optional) additional arguments.
#'
#' @section Methods:
#' \code{\link{get.adjacency.matrix}}, \code{\link{get.lambdas}},
#' \code{\link{num.nodes}}, \code{\link{num.edges}}, \code{\link{num.samples}}
#'
#' @examples
#'
#' \dontrun{
#' ### Learn the cytometry network
#' library(sparsebn)
#' data(cytometryContinuous) # from the sparsebn package
#' cyto.data <- sparsebnData(cytometryContinuous[["data"]], type = "continuous")
#' cyto.learn <- estimate.dag(cyto.data)
#'
#' ### Inspect the output
#' class(cyto.learn)
#' print(cyto.learn)
#' plot(cyto.learn)
#' }
#'
#' @docType class
#' @name sparsebnPath
NULL

#' @rdname sparsebnPath
#' @export
is.sparsebnPath <- function(x){
    inherits(x, "sparsebnPath")
} # END IS.sparsebnPath

# sparsebnPath constructor
#' @export
sparsebnPath.list <- function(x){
    if(!check_list_class(x, "sparsebnFit")){
        stop("Some component is not of type sparsebnPath -- sparsebnPath objects must consist of sparsebnFit components only.")
    }

    ### Note that we still allow these objects to inherit from the base list class
    structure(x, class = c("sparsebnPath", "list"))
} # END sparsebnPath.LIST

.str_sparsebnPath <- function(x){
    sbp.out <- ""
    sbp.out <- paste0(sbp.out,
                      "sparsebn Solution Path\n",
                      " ", num.nodes(x), " nodes\n",
                      " ", num.samples(x), " observations\n",
                      " ", length(x), " estimates for lambda in [", round(min(get.lambdas(x)), 4), ", ", round(max(get.lambdas(x)), 4), "]\n",
                      " ", "Number of edges per solution: ", paste(num.edges(x), collapse = "-"), "\n"
                )

    sbp.out
} # END .STR_SPARSEBNPATH

#' @param verbose If \code{TRUE}, then each estimate in the solution path is printed separately. Do not use for
#'        large graphs or large solution paths. (default = \code{FALSE})
#'
#' @rdname sparsebnPath
#' @method print sparsebnPath
#' @export
print.sparsebnPath <- function(x, verbose = FALSE, ...){
    if(verbose){
        print.default(x) # default generic reverts to list => separate calls to print.sparsebnFit for each component
    } else{
        cat(.str_sparsebnPath(x))
    }
} # END PRINT.SPARSEBNPATH

#' @param object an object of type \code{sparsebnPath}
#'
#' @rdname sparsebnPath
#' @method summary sparsebnPath
#' @export
summary.sparsebnPath <- function(object, ...){
    ### Print usual sparsebnPath output
    cat(.str_sparsebnPath(object))
    cat("\n")

    ### Add summary for each lambda
    lambdas <- get.lambdas(object)
    nedges <- num.edges(object)
    print(data.frame(lambda = lambdas, nedge = nedges))

    # sbp.out <- sprintf("%10.4f %5d", round(lambdas, 4), nedges)
    # cat(sbp.out, sep = "\n")
} # END SUMMARY.SPARSEBNPATH

#' @param labels \code{TRUE} or \code{FALSE}. Whether or not to print out
#' labels with summary information for each plot in the solution path.
#'
#' @rdname sparsebnPath
#' @method plot sparsebnPath
#' @export
plot.sparsebnPath <- function(x, labels = FALSE, ...){
    ### UPDATE 7/28/17: What are the issues? Seems fine.
    ### Issues when plotting null DAG, so remove it
    # x <- x[-1] # Do this BEFORE setting the grid layout below!

    ### Set plotting parameters (Don't use no.readonly = TRUE! See https://stat.ethz.ch/pipermail/r-help/2007-July/136770.html)
    par.default <- par()[c("mfrow", "mai")] # Only re-set what we change here
    par(mfrow = n2mfrow(length(x)),         # Automatically choose a sensible grid to use
        mai = c(0, 0, 0.1, 0)                      # Need to reset margins (why??? graph packages seem to handle this oddly)
        )

    tryCatch({
        # lapply(x, plot)
        for(fit in x){
            plot(fit, ...)
            if(labels){
                # title(sprintf("lambda = %4.2f, # of edges = %d", fit$lambda, fit$nedge), cex.main = 0.85)
                title(bquote(lambda*" = "*.(round(fit$lambda, 2))*" / "*.(fit$nedge)~"edges"), cex.main = 0.85)
            }
        }
    }, error = function(c){
        dev.off()
        stop(c)
    })

    par(par.default) # restore user's original settings
}

#' @export
as.list.sparsebnPath <- function(x, ...){
    class(x) <- "list"
    x
} # END AS.LIST.SPARSEBNPATH

#' @describeIn num.nodes Extracts the number of nodes of \link{sparsebnPath} object.
#' @export
num.nodes.sparsebnPath <- function(x){
    unique(unlist(lapply(x, function(z) z$pp)))
} # END NUM.NODES.SPARSEBNPATH

#' @describeIn num.edges Extracts the number of edges of \link{sparsebnPath} object.
#' @export
num.edges.sparsebnPath <- function(x){
    ### unique(.) not needed since different estimates should have different # of edges
    unlist(lapply(x, function(z) z$nedge))
} # END NUM.EDGES.SPARSEBNPATH

#' @describeIn num.samples Extracts the number of samples of \link{sparsebnPath} object.
#' @export
num.samples.sparsebnPath <- function(x){
    unique(unlist(lapply(x, function(z) z$nn)))
} # END NUM.SAMPLES.SPARSEBNPATH

#' Extract regularization path from solution path
#'
#' @describeIn get.lambdas Returns a vector of lambda values defining the solution path of a \code{\link{sparsebnPath}} object.
#' @export
get.lambdas.sparsebnPath <- function(x){
    lambdas <- unlist(lapply(x, function(z){ z$lambda}))
    names(lambdas) <- NULL

    lambdas
} # END GET.LAMBDAS.SPARSEBNPATH

#' Extract node names from solution path
#'
#' @describeIn get.nodes Returns the node names from a \code{\link{sparsebnPath}} object.
#' @export
get.nodes.sparsebnPath <- function(x){
    x[[1]]$nodes
} # END GET.NODES.SPARSEBNPATH

#' @describeIn get.adjacency.matrix Retrieves all \code{edges} slots in the solution path, converts to an adjacency matrix, and returns as a list
#' @export
get.adjacency.matrix.sparsebnPath <- function(x){
    lapply(x, get.adjacency.matrix)
} # END GET.ADJACENCY.MATRIX.sparsebnPath

#' @export
"[.sparsebnPath" <- function(x, i){
    ### Needed to ensure that naive subsetting of path objects as a list
    ###  still returns a path object
    sparsebnPath(as.list(x)[i])
}


#' @export
to_edgeList.sparsebnPath <- function(x){
    sparsebnPath(lapply(x, to_edgeList))
}

