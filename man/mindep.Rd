% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindep.R
\name{mindep}
\alias{mindep}
\title{Execute minimal depth variable importance}
\usage{
mindep(variables, trees)
}
\arguments{
\item{variables}{vector of variable names}

\item{trees}{list of trees that was generated by \link{getTreeranger} and \code{\link[=addLayer]{addLayer()}}.}
}
\value{
List with the following components:
\itemize{
\item \code{depth}: mean minimal depth for each variable
\item \code{selected}: variables has been selected (1) or not (0),
\item \code{threshold}: the threshold that is used for the selection
}
}
\description{
This function determines the minimal depth of variables from a forest that
is created by \code{\link[=getTreeranger]{getTreeranger()}} and \code{\link[=addLayer]{addLayer()}}.
}
