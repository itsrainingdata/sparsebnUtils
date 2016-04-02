#
#  s3-sparsebnData.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/28/16.
#  Copyright (c) 2014-2016 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# sparsebnData S3 Class for R
#------------------------------------------------------------------------------#

#
# sparsebnData S3 class skeleton
#
# Data
# * data.frame data     // data
# * list ivn            // list of nodes under intervention for each row (observation)
# * character type      // either "continuous", "discrete", or "mixed"
#

#
# A convenience class for keeping track of which nodes are under intervention in a dataset. Each
#  component of the list 'ivn' is an integer vector indicating which nodes (columns) were under
#  intervention for the corresponding row. Note that 'ivn' must be a list in order to accommodate
#  the possibility that multiple nodes are under intervention, and different nodes may have
#  different treatments (i.e. number and/or identity of manipulated nodes).
#
# Examples:
#  1) sbd$ivn[1] = NULL: No nodes were under intervention for the first observation
#  2) sbd$ivn[10] = c(1): The first node was under intervention for the tenth observation
#  3) sbd$ivn[120] = c(1,5,500): The 1st, 5th, and 500th nodes were under intervention for the 120th observation
#

#' sparsebnData class
#'
#' This class stores data that may contain interventions on some or all of the observations. It also
#' allows for the degenerate case with no interventions, i.e. purely observational data.
#'
#' The structure of a \code{sparsebnData} object is very simple: It contains a \code{data.frame} object and a
#' list of interventions. The list should be the same size as the number of rows in the dataset, and each
#' component indicates which column(s) in the dataset is (are) under intervention. If an observation has no
#' interventions, then the corresponding component is \code{NULL}. If this list only contains \code{NULL} values,
#' then the data is purely observational.
#'
#' Also inherits from \code{\link{list}}.
#'
#' @section Slots:
#' \describe{
#' \item{\code{data}}{(data.frame) Dataset.}
#' \item{\code{ivn}}{(list) List of columns under intervention for each row in \code{data}.}
#' }
#'
#' @section Methods:
#' \code{\link{num.samples}}
#' \code{\link{print}}
#'
#' @docType class
#' @name sparsebnData
NULL

#' @export
is.sparsebnData <- function(sbd){
    inherits(sbd, "sparsebnData")
} # END IS.SPARSEBNDATA

# sparsebnData constructor
sparsebnData.list <- function(li){

    if( !is.list(li)){
        stop("Input must be a list!")
    } else if( length(li) != 3 || !setequal(names(li), c("data", "ivn", "type"))){
        stop("Input is not coercable to an object of type sparsebnFit, check list for the following elements: data (data.frame), ivn (list)")
    } else if( !sparsebnUtils::check_if_data_matrix(li$data)){
        stop(sprintf("Component 'data' must be a valid data.frame or numeric object! <Current type: %s>", class(li$data)))
    } else if(nrow(li$data) != length(li$ivn)){
        stop("The length of the ivn list must equal the number of rows in the data!")
    } else if(!(li$type %in% c("continuous", "discrete", "mixed"))){
        stop(sprintf("\'type\' must be one of the following: \'continuous\', \'discrete\', \'mixed\'."))
    }

    ### Final output
    structure(li, class = "sparsebnData")
} # END SPARSEBNDATA.LIST

# sparsebnData constructor
#  Default constructor for data.frame input
#' @export
sparsebnData.data.frame <- function(data, ivn, type){

    #
    # If the user fails to specify a list of interventions, ASSUME all rows are observational. If the data
    #  is experimental, the user needs to specify this by passing in 'ivn' (see also sparsebnData.list).
    #

    if(missing(ivn)){
        warning("A list of interventions was not specified: Assuming data is purely observational.")
        ivn <- vector("list", length = nrow(data))
    }

    ### Final output
    sparsebnData.list(list(data = data, ivn = ivn, type = type))
} # END SPARSEBNDATA.DATA.FRAME

# sparsebnData constructor
#  Default constructor for matrix input
#' @export
sparsebnData.matrix <- function(data, ivn, type){
    sparsebnData.data.frame(as.data.frame(data), ivn, type)
} # END SPARSEBNDATA.MATRIX

#' @export
#' @describeIn num.samples
num.samples.sparsebnData <- function(sbd){
    nrow(sbd$data)
} # END NUM.SAMPLES.SPARSEBNDATA

# Returns TRUE if the data contains no interventions, i.e. is purely observational
#' @export
is.obs <- function(sbd){
    all(unlist(lapply(dat$ivn, is.null)))
} # END IS.OBS

# Returns the number of rows with at least one intervention
#' @export
count.interventions <- function(sbd){
    sum(unlist(lapply(dat$ivn, function(x) !is.null(x))))
} # END COUNT.INTERVENTIONS

# Default print method
print.sparsebnData <- function(sbd, n = 5L){
    # print(head(sbd$data, n = n), row.names = FALSE)
    .print_data_frame(sbd$data, topn = n)

    cat(sprintf("\n%d total rows (%d rows omitted)\n", num.samples(sbd), num.samples(sbd) - 2*n))
    if(is.obs(sbd)){
        cat(sprintf("Observational data with %s observations", sbd$type))
    } else{

        cat(sprintf("%s data w/ interventions on %d/%d rows.", capitalize(sbd$type), count.interventions(sbd), num.samples(sbd)))
    }
    ### Add a message about the interventions as well / if purely obs, etc.
} # END PRINT.SPARSEBNDATA

# Convert a sparsebnData object back to a data.frame
#' @export
as.data.frame.sparsebnData <- function(x){
    data.frame(x$data)
} # END AS.DATA.FRAME.SPARSEBNDATA

### Internal method for picking the correct family for fitting parameters
pick_family.sparsebnData <- function(sbd){
    if(sbd$type == "continuous"){
        return("gaussian")
    } else if(sbd$type == "discrete"){
        return("binomial")
    } else{
        stop("'mixed' type not supported for inference yet!")
    }
}

### Borrow the print.data.table method from the 'data.table' package without needing to import the entire package
###  This is an experimental method!
.print_data_frame <- function(x,
                              topn=5,   # (5) print the top topn and bottom topn rows with '---' inbetween
                              nrows=5, # (100) under this the whole (small) table is printed, unless topn is provided
                              row.names = TRUE, ...){
    if (!is.numeric(nrows)) nrows = 100L
    if (!is.infinite(nrows)) nrows = as.integer(nrows)
    if (nrows <= 0L) return(invisible())   # ability to turn off printing
    if (!is.numeric(topn)) topn = 5L
    topnmiss = missing(topn)
    topn = max(as.integer(topn),1L)
    if (nrow(x) == 0L) {
        if (length(x)==0L)
           cat("Null data.table (0 rows and 0 cols)\n")  # See FAQ 2.5 and NEWS item in v1.8.9
        else
           cat("Empty data.table (0 rows) of ",length(x)," col",if(length(x)>1L)"s",": ",paste(head(names(x),6),collapse=","),if(ncol(x)>6)"...","\n",sep="")
        return()
    }
    if (topn*2<nrow(x) && (nrow(x)>nrows || !topnmiss)) {
        toprint = rbind(head(x, topn), tail(x, topn))
        rn = c(seq_len(topn), seq.int(to=nrow(x), length.out=topn))
        printdots = TRUE
    } else {
        toprint = x
        rn = seq_len(nrow(x))
        printdots = FALSE
    }
    toprint=format(toprint, ...)
    # FR #5020 - add row.names = logical argument to print.data.table
    if (isTRUE(row.names)) rownames(toprint)=paste(format(rn,right=TRUE),":",sep="") else rownames(toprint)=rep.int("", nrow(x))
    if (is.null(names(x))) colnames(toprint)=rep("NA", ncol(toprint)) # fixes bug #4934
    if (printdots) {
        toprint = rbind(head(toprint,topn),"---"="",tail(toprint,topn))
        rownames(toprint) = format(rownames(toprint),justify="right")
        print(toprint,right=TRUE,quote=FALSE)
        return(invisible())
    }
    if (nrow(toprint)>20L)
        # repeat colnames at the bottom if over 20 rows so you don't have to scroll up to see them
        toprint=rbind(toprint,matrix(colnames(toprint),nrow=1)) # fixes bug #4934
    print(toprint,right=TRUE,quote=FALSE)
    invisible()
}
