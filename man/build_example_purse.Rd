% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examples.R
\name{build_example_purse}
\alias{build_example_purse}
\title{Build example purse}
\usage{
build_example_purse(up_to = NULL, quietly = FALSE)
}
\arguments{
\item{up_to}{The point up to which to build the index. If \code{NULL}, builds full index. Else specify a \verb{build_*} function
(as a string) - the index will be built up to and including this function. This option is mainly for helping with
function examples. Example: \code{up_to = "build_normalise"}.}

\item{quietly}{If \code{TRUE}, suppresses all messages.}
}
\value{
purse class object
}
\description{
Shortcut function to build an example purse. This is currently an "artificial" example, in that it takes the ASEM data set
used in \code{\link[=build_example_coin]{build_example_coin()}} and replicates it for five years, adding artificial noise to simulate year-on-year variation.
This was done simply to demonstrate the functionality of purses, and will at some point be replaced with a real example.
See also \code{vignette("coins")}.
}
\examples{
# build example purse up to unit screening step
purse <- build_example_purse(up_to = "Screen")
purse

}
