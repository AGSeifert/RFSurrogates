% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MutualForestImpact.R
\name{MFI}
\alias{MFI}
\title{Mutual Forest Impact shortcut function (recommended).}
\usage{
MFI(variables, candidates, num.threads = 1, ...)
}
\arguments{
\item{variables}{Vector of variable names for \strong{which related variables should be searched}.}

\item{candidates}{Vector of variable names that \strong{are candidates to be related to the variables}.}

\item{num.threads}{Number of threads to parallelize with. (Default: 1)}

\item{...}{
  Arguments passed on to \code{\link[=RandomForestSurrogates]{RandomForestSurrogates}}
  \describe{
    \item{\code{x,y}}{Predictor data and dependent variables.}
    \item{\code{s.pct,s}}{Number of surrogate splits.
This can be defined either by setting \code{s.pct} to a number between
0 and 1, or providing an exact value for \code{s}.
\itemize{
\item \code{s.pct}: Percentage of variables to use for \code{s}. (Default: 0.01)
\item \code{s}:  Number of surrogate splits. (Default: Number of variables
multiplied by \code{s.pct}, which defaults to 0.01; If \code{s.pct} is
less than or equal to zero, or greater than 1: 0.01 is used instead.)
}}
    \item{\code{mtry}}{Number of variables to possibly split at in each node.
Default is the (rounded down) number of variables to the power
of three quarters (Ishwaran, 2011).
Alternatively, a single argument function returning an integer,
given the number of independent variables.}
    \item{\code{type}}{The type of random forest to create with ranger.
One of \code{"regression"} (Default), \code{"classification"} or \code{"survival"}.}
    \item{\code{status}}{If \code{type = "regression"}: Survival forest status variable.
Use 1 for event and 0 for censoring. Length must match \code{y}.}
    \item{\code{min.node.size}}{Minimal node size to split at. (Default: 1)}
    \item{\code{permutate}}{Enable to permutate \code{x} for \code{\link[=MutualForestImpact]{MutualForestImpact()}} (Default: FALSE).}
    \item{\code{seed}}{RNG seed. It is strongly recommended that you set this value.}
    \item{\code{preschedule.threads}}{(Default: TRUE) Passed as \code{mc.preschedule} to \code{\link[parallel:mclapply]{parallel::mclapply()}} in \code{\link[=addSurrogates]{addSurrogates()}}.}
    \item{\code{num.trees}}{Number of trees.}
  }}
}
\value{
A \code{\link[=MutualForestImpact]{MutualForestImpact()}} list object.
\itemize{
\item \code{REL}: The \code{\link[=MeanAdjustedAgreement]{MeanAdjustedAgreement()}} object.
\item \code{PERM}: The permutated \code{\link[=MeanAdjustedAgreement]{MeanAdjustedAgreement()}} object.
\item \code{relations}: Matrix of determined relations (rows: investigated variables, columns: candidate variables).
}
}
\description{
This method corrects the mean adjusted agreement by a permutation approach
and generates the relation parameter mutual forest impact.
Subsequently p-values are determined and related variables are selected.
}
\examples{
\donttest{
data("SMD_example_data")
mfi <- MFI(
  x = SMD_example_data[, -1], y = SMD_example_data[, 1],
  s = 10, num.trees = 50, num.threads = 1,
  variables = c("X7", "X1"), candidates = colnames(SMD_example_data)[2:101]
)
}

}
\seealso{
\code{\link[=MutualForestImpact]{MutualForestImpact()}}
}
\keyword{shortcut}
\keyword{varrel}
