% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MinimalDepth.R
\name{MinimalDepth}
\alias{MinimalDepth}
\title{Variable selection with Minimal Depth (MD).}
\usage{
MinimalDepth(RFS)
}
\arguments{
\item{RFS}{A \code{\link[=RandomForestSurrogates]{RandomForestSurrogates()}} object with \code{s} equal to 0.}
}
\value{
A \code{MinimalDepth} S3 list object:
\itemize{
\item \code{RFS}: The original \code{\link[=RandomForestSurrogates]{RandomForestSurrogates()}} object.
\item \code{selected}: A character vector of the selected variable names.
\item \code{depth}: A numeric vector of the surrogate minimal depth for each variable.
\item \code{threshold}: The depth threshold used to select variables.
\item \code{surrogates}:
\itemize{
\item \code{average}: Total average number of surrogate variables.
\item \code{layer}: Average number of surrogate variables by layer (Named numeric of length 1000).
}
}
}
\description{
This is identical to \code{\link[=SurrogateMinimalDepth]{SurrogateMinimalDepth()}}, except it requires \code{s} to be 0.
}
\examples{
\donttest{
data("SMD_example_data")

res.md <- MinimalDepth(RandomForestSurrogates(
  x = SMD_example_data[, -1],
  y = SMD_example_data[, 1],
  num.trees = 10,
  s = 0,
  num.threads = 1
))
}

}
\references{
\itemize{
\item Ishwaran, H. et al. (2011) Random survival forests for high-dimensional data. Stat Anal Data Min, 4, 115–132. \url{https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.10103}
\item Ishwaran, H. et al. (2010) High-Dimensional Variable Selection for Survival Data. J. Am. Stat. Assoc., 105, 205–217. \url{http://www.ccs.miami.edu/~hishwaran/papers/IKGML.JASA.2010.pdf}
}
}
\keyword{varsel}
