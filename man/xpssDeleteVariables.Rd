% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/xpssDeleteVariables.R
\name{xpssDeleteVariables}
\alias{xpssDeleteVariables}
\title{Deletes variables from dataset}
\usage{
xpssDeleteVariables(x, variables = NULL)
}
\arguments{
\item{x}{input data.}

\item{variables}{atomic character or character vector with the name of the variables.}
}
\value{
Output is the narrowed dataset.
}
\description{
R implementation of the SPSS \code{DELETE VARIABLES} argument.
}
\examples{
# load data
data(fromXPSS)
xpssDeleteVariables(fromXPSS, variables = "V1")
}
\author{
Bastian Wiessner
}
\seealso{
Related Functions \code{\link{drop}} \code{\link{subset}}
}

