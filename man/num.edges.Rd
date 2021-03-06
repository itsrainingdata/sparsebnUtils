% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3-edgeList.R, R/s3-generics.R,
%   R/s3-sparsebnFit.R, R/s3-sparsebnPath.R
\name{num.edges.edgeList}
\alias{num.edges.edgeList}
\alias{num.edges}
\alias{num.edges.sparsebnFit}
\alias{num.edges.sparsebnPath}
\title{num.edges}
\usage{
\method{num.edges}{edgeList}(x)

num.edges(x)

\method{num.edges}{sparsebnFit}(x)

\method{num.edges}{sparsebnPath}(x)
}
\arguments{
\item{x}{a \code{\link{sparsebnFit}} or \code{\link{sparsebnPath}} object.}
}
\value{
Number of edges as \code{integer}.
}
\description{
Extracts the number of edges of the associated graph object.
}
\section{Methods (by class)}{
\itemize{
\item \code{edgeList}: Extracts the number of edges of \link{edgeList} object.

\item \code{sparsebnFit}: Extracts the number of edges of \link{sparsebnFit} object.

\item \code{sparsebnPath}: Extracts the number of edges of \link{sparsebnPath} object.
}}

