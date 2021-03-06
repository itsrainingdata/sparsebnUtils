% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparsebnUtils-cytoscape.R
\name{openCytoscape}
\alias{openCytoscape}
\title{Display graphs in Cytoscape}
\usage{
openCytoscape(x, title, ...)
}
\arguments{
\item{x}{A \code{\link{sparsebnFit}} object or other graph object.}

\item{title}{A character string, this is the name you will see on the Cytoscape
network window. Multiple windows with the same name are not
permitted. See \code{\link[RCy3]{createNetworkFromGraph}} for more
details.}

\item{...}{Other arguments to \code{\link[RCy3]{createNetworkFromGraph}}.}
}
\description{
NOTE: This method is currently experimental and under development!
}
\details{
Displays the selected graph in the Cytoscape application. Note that this
requires that Cytoscape is installed on the user's system, and that the
RCy3 package is installed and properly configured. Cytoscape can be
downloaded at \href{http://www.cytoscape.org/}{http://www.cytoscape.org/}.
}
