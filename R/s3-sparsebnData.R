#
#  s3-sparsebnData.R
#  sparsebnUtils
#
#  Created by Bryon Aragam (local) on 1/28/16.
#  Copyright (c) 2014-2017 Bryon Aragam. All rights reserved.
#

#------------------------------------------------------------------------------#
# sparsebnData S3 Class for R
#------------------------------------------------------------------------------#

#
# sparsebnData S3 class skeleton
#
# Data
# * data.frame data     // data
# * character type      // either "continuous", "discrete", or "mixed"
# * list ivn            // list of nodes under intervention for each row (observation)
# * list levels         // names of levels for each variable
#

#
# A convenience class for storing metadata associated with discrete and continuous data. In addition
#  to the data.frame containing all of the data, this class keeps track of the data type, number
#  of levels in each variable (discrete data only), and possible interventions on the observations.
#
# ivn:
#  Each component of the list 'ivn' is an integer vector indicating which nodes (columns) were under
#  intervention for the corresponding row. Note that 'ivn' must be a list in order to accommodate
#  the possibility that multiple nodes are under intervention, and different nodes may have
#  different treatments (i.e. number and/or identity of manipulated nodes).
#
# Examples:
#  1) data$ivn[1] = NULL: No nodes were under intervention for the first observation
#  2) data$ivn[10] = c(1): The first node was under intervention for the tenth observation
#  3) data$ivn[120] = c(1,5,500): The 1st, 5th, and 500th nodes were under intervention for the 120th observation
#
# levels:
#  This is a list containing the different levels for each node / variable in the dataset.
#
# Examples:
#  1) data$levels = NULL: There are no levels - data is continuous.
#  2) data$levels[1] = c(1, 2, 3): The first node has three possible levels, 1, 2, or 3.
#  3) data$levels[50] = c("A", "B"): The 50th node has two possible levels, "A" or "B.
#

#' sparsebnData class
#'
#' This class stores data that may contain interventions on some or all of the observations. It also
#' allows for the degenerate case with no interventions, i.e. purely observational data.
#'
#' The structure of a \code{sparsebnData} object is very simple: It contains a \code{data.frame} object,
#' a type identifier (i.e. discrete or continuous), a list of factor levels, and a list of interventions.
#' \itemize{
#' \item The \code{levels} list should be the same size as the number of nodes and consist of names of the different
#' levels for each node. Each level should be coded to be from 0...\eqn{k}-1 where \eqn{k} is the number of levels for a
#' particular variable (see below for more).
#' \item The \code{ivn} list should be the same size as the number of rows in the dataset,
#' and each component indicates which column(s) in the dataset is (are) under intervention. If an
#' observation has no interventions, then the corresponding component is \code{NULL}. Thus, if the data is
#' purely observational, this list should contain only \code{NULL} values.
#' }
#'
#' Presently, only levels coded as 0,1,...,\eqn{k}-1 are supported (\eqn{k} = the number of levels for a
#' variable). Future releases are planned to support more general factor levels. The level 0 corresponds
#' to the baseline level or measurement.
#'
#' Also inherits from \code{\link{list}}.
#'
#' @param x a \code{\link{data.frame}} or \code{\link{matrix}} object.
#' @param type either '\code{discrete}' or '\code{continuous}'.
#' @param levels (optional) \code{\link{list}} of levels for each node. If omitted, levels will be automatically
#'        detected from \code{\link{unique}}.
#' @param ivn (optional) \code{\link{list}} of interventions for each observation. If omitted, data is assumed to be
#'        purely observational.
#' @param n (optional) number of rows from data matrix to print.
#' @param ... (optional) additional arguments.
#'
#' @section Slots:
#' \describe{
#' \item{\code{data}}{(\code{\link{data.frame}}) Dataset.}
#' \item{\code{type}}{(\code{\link{character}}) Type of data: Either "continuous", "discrete", or "mixed".}
#' \item{\code{levels}}{(\code{\link{list}}) List of levels for each column in \code{data}.}
#' \item{\code{ivn}}{(\code{\link{list}}) List of columns under intervention for each row in \code{data}.}
#' }
#'
#' @section Methods:
#' \code{\link{print}}
#' \code{\link{num.samples}}
#' \code{\link{is.obs}}
#' \code{\link{count.levels}}
#' \code{\link{count.interventions}}
#' \code{\link{as.data.frame}}
#'
#' @examples
#'
#' ### Generate a random continuous dataset
#' mat <- matrix(rnorm(1000), nrow = 20)
#' dat <- sparsebnData(mat, type = "continuous") # purely observational data with continuous variables
#'
#' ### Discrete data
#' mat <- cbind(c(0,1,1,0),
#'              c(2,1,0,1),
#'              c(0,0,3,0))
#' dat.levels <- list(c(0,1), c(0,1,2), c(0,1,2,3))
#' dat <- sparsebnData(mat,
#'                     type = "discrete",
#'                     levels = dat.levels) # purely observational data with discrete variables
#'
#' dat.ivn <- list(c(1), c(1), c(2,3), c(2,3)) # add some interventions
#' dat <- sparsebnData(mat,
#'                     type = "discrete",
#'                     levels = dat.levels,
#'                     ivn = dat.ivn) # specify intervention rows
#'
#' @docType class
#' @name sparsebnData
NULL

#' @rdname sparsebnData
#' @export
is.sparsebnData <- function(x){
    inherits(x, "sparsebnData")
} # END IS.SPARSEBNDATA

# sparsebnData constructor
#' @export
sparsebnData.list <- function(x, ...){

    type_list <- c("continuous", "discrete")

    if( !is.list(x)){
        stop("Input must be a list!")
    } else if( length(x) != 4 || !setequal(names(x), c("data", "type", "levels", "ivn"))){
        stop("Input is not coercable to an object of type sparsebnFit, check list for the following elements: data (data.frame), type (character), levels (list), ivn (list)")
    } else if( !check_if_data_matrix(x$data)){
        stop(sprintf("Component 'data' must be a valid data.frame or numeric object! <Current type: %s>", class(x$data)))
    } else if(!(x$type %in% type_list)){
        stop(invalid_type_input(type_list))
    } else if(!is.null(x$levels)){
        if(ncol(x$data) != length(x$levels)){
            stop("The length of the levels list must equal the number of columns in the data!")
        }
    } else if(nrow(x$data) != length(x$ivn)){
        stop("The length of the ivn list must equal the number of rows in the data!")
    }

    num_missing <- count_nas(x$data)
    if(num_missing > 0){
        warning(has_missing_values(num_missing))
    }

    ### Final output
    structure(x, class = "sparsebnData")
} # END SPARSEBNDATA.LIST

# sparsebnData constructor
#  Default constructor for data.frame input
#' @rdname sparsebnData
#' @export
sparsebnData.data.frame <- function(x, type, levels = NULL, ivn = NULL, ...){

    type_list <- c("continuous", "discrete")

    ### User must specify type
    if(missing(type)){
        stop(invalid_type_input(type_list))
        ivn <- vector("list", length = nrow(x))
    } else{
        match_string <- pmatch(type, type_list) # use partial matching to select type
        if(is.na(match_string)){ # if there was no match, error
            stop(invalid_type_input(type_list))
        } else{ # if match was found, use it
            type <- type_list[match_string]
        }
    }

    #
    # If the user fails to specify a list of interventions, ASSUME all rows are observational. If the data
    #  is experimental, the user needs to specify this by passing in 'ivn' (see also sparsebnData.list).
    #
    if(is.null(ivn)){
        message("A list of interventions was not specified: Assuming data is purely observational.")
        ivn <- vector("list", length = nrow(x))
    }

    #
    # If the user fails to specify a list of levels, attempt to infer them automatically.
    #
    if(is.null(levels)){
        # message("A list of levels was not specified: Assuming data is continuous.")
        if(type == "continuous"){
            levels <- NULL
        } else{
            levels <- auto_generate_levels(x)
        }
    }

    ### Final output
    sparsebnData.list(list(data = x, type = type, levels = levels, ivn = ivn))
} # END SPARSEBNDATA.DATA.FRAME

# sparsebnData constructor
#  Default constructor for matrix input
#' @rdname sparsebnData
#' @export
sparsebnData.matrix <- function(x, type, levels = NULL, ivn = NULL, ...){
    sparsebnData.data.frame(as.data.frame(x), type, levels, ivn)
} # END SPARSEBNDATA.MATRIX

.str_sparsebnData <- function(x, n){
    sbd.out <- ""
    sbd.out <- paste0(sbd.out, sprintf("\n%d total rows (%d rows omitted)\n", num.samples(x), max(num.samples(x) - 2*n, 0)))
    if(is.obs(x)){
        sbd.out <- paste0(sbd.out, sprintf("Observational data with %s observations", x$type))
    } else{
        sbd.out <- paste0(sbd.out, sprintf("%s data w/ interventions on %d/%d rows.", capitalize(x$type), count.interventions(x), num.samples(x)))
    }
} # END PRINT.SPARSEBNDATA

# Default print method
#' @rdname sparsebnData
#' @method print sparsebnData
#' @export
print.sparsebnData <- function(x, n = 5L, ...){
    # print(utils::head(data$data, n = n), row.names = FALSE)
    .print_data_frame(x$data, topn = n)

    # cat(sprintf("\n%d total rows (%d rows omitted)\n", num.samples(x), max(num.samples(x) - 2*n, 0)))
    # if(is.obs(x)){
    #     cat(sprintf("Observational data with %s observations", x$type))
    # } else{
    #
    #     cat(sprintf("%s data w/ interventions on %d/%d rows.", capitalize(x$type), count.interventions(x), num.samples(x)))
    # }
    cat(.str_sparsebnData(x, n))
} # END PRINT.SPARSEBNDATA

# Default summary method
#' @param object an object of type \code{sparsebnData}
#'
#' @rdname sparsebnData
#' @method summary sparsebnData
#' @export
summary.sparsebnData <- function(object, n = 5L, ...){
    print(summary(object$data))
    cat(.str_sparsebnData(object, n = n))
} # END SUMMARY.SPARSEBNDATA

#' @rdname sparsebnData
#' @method plot sparsebnData
#' @export
plot.sparsebnData <- function(x, ...){
    plot(x$data)
} # END PLOT.SPARSEBNDATA

#' @describeIn num.samples Extracts the number of samples of \link{sparsebnData} object.
#' @export
num.samples.sparsebnData <- function(x){
    nrow(x$data)
} # END NUM.SAMPLES.SPARSEBNDATA

#' Check if data is observational
#'
#' Returns TRUE if the data contains no interventions, i.e. is purely observational
#'
#' @param data a \code{\link{sparsebnData}} object.
#'
#' @export
is.obs <- function(data){
    all(unlist(lapply(data$ivn, is.null)))
} # END IS.OBS

#' Count the number of rows under intervention
#'
#' Returns the number of rows with at least one intervention
#'
#' @param data a \code{\link{sparsebnData}} object.
#'
#' @export
count.interventions <- function(data){
    sum(unlist(lapply(data$ivn, function(x) !is.null(x))))
} # END COUNT.INTERVENTIONS

#' Count the number of levels per variable
#'
#' Returns the number of levels per variable as an ordered vector.
#'
#' @param data a \code{\link{sparsebnData}} object.
#'
#' @export
count.levels <- function(data){
    unlist(lapply(data$levels, length))
} # END COUNT.LEVELS

#' Convert a sparsebnData object back to a data.frame
#'
#' @param x a \code{\link{sparsebnData}} object.
#' @param ... (optional) additional argument to \code{as.data.frame}.
#'
#' @method as.data.frame sparsebnData
#' @export
as.data.frame.sparsebnData <- function(x, ...){
    data.frame(x$data, ...)
} # END AS.DATA.FRAME.SPARSEBNDATA

### Check if discrete data corresponds to binary data or not
is_binary <- function(x){
    stopifnot(is.sparsebnData(x))
    count_levels <- unique(count.levels(x))
    length(count_levels) == 1 && count_levels == 2 # TRUE if exactly two levels per variable
}

### Internal method for picking the correct family for fitting parameters
#' @export
pick_family.sparsebnData <- function(x){
    if(x$type == "continuous"){
        return("gaussian")
    } else if(x$type == "discrete"){
        if(is_binary(x)){
            return("binomial")
            # return("multinomial")
        } else{
            return("multinomial")
            # stop("Discrete data with more than 2 levels is not yet supported! Please check for future updates.")
        }
    } else{
        stop("Incompatible data found! Note that mixed data is not supported for inference yet!")
    }
}

#' @rdname coerce_discrete
#' @export
coerce_discrete.factor <- function(x){
    convert_factor_to_discrete(x)
}

#' @rdname coerce_discrete
#' @export
coerce_discrete.numeric <- function(x){
    convert_factor_to_discrete(factor(x, ordered = FALSE))
}

#' @rdname coerce_discrete
#' @export
coerce_discrete.integer <- function(x){
    convert_factor_to_discrete(factor(x, ordered = FALSE))
}

#' @rdname coerce_discrete
#' @export
coerce_discrete.character <- function(x){
    convert_factor_to_discrete(factor(x, ordered = FALSE))
}

#' @rdname coerce_discrete
#' @export
coerce_discrete.data.frame <- function(x){
    apply(x, 2, coerce_discrete)
}

#' @rdname coerce_discrete
#' @export
coerce_discrete.sparsebnData <- function(x){
    x$data <- coerce_discrete(x$data)

    x
}

### Borrow the print.data.table method from the 'data.table' package
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
           cat("Null data.frame (0 rows and 0 cols)\n")  # See FAQ 2.5 and NEWS item in v1.8.9
        else
           cat("Empty data.frame (0 rows) of ",length(x)," col",if(length(x)>1L)"s",": ",paste(utils::head(names(x),6),collapse=","),if(ncol(x)>6)"...","\n",sep="")
        return()
    }
    if (topn*2<nrow(x) && (nrow(x)>nrows || !topnmiss)) {
        toprint = rbind(utils::head(x, topn), utils::tail(x, topn))
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
        toprint = rbind(utils::head(toprint,topn),"---"="",utils::tail(toprint,topn))
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
