% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qfuncs.R
\name{qNormalise.data.frame}
\alias{qNormalise.data.frame}
\title{Quick normalisation of a data frame}
\usage{
\method{qNormalise}{data.frame}(x, f_n = "n_minmax", f_n_para = NULL, directions = NULL, ...)
}
\arguments{
\item{x}{A numeric data frame}

\item{f_n}{Name of a normalisation function (as a string) to apply to each column of \code{x}. Default \code{"n_minmax"}.}

\item{f_n_para}{Any further arguments to pass to \code{f_n}, as a named list. If \code{f_n = "n_minmax"}, this defaults
to \code{list(l_u = c(0,100))} (scale between 0 and 100).}

\item{directions}{An optional data frame containing the following columns:
\itemize{
\item \code{iCode} The indicator code, corresponding to the column names of the data frame
\item \code{Direction} numeric vector with entries either \code{-1} or \code{1}
If \code{directions} is not specified, the directions will all be assigned as \code{1}. Non-numeric columns do not need
to have directions assigned.
}}

\item{...}{arguments passed to or from other methods.}
}
\value{
A normalised data frame
}
\description{
This is a wrapper function for \code{\link[=Normalise]{Normalise()}}, which offers a simpler syntax but less flexibility. It
normalises a data frame using a specified function \code{f_n} which is used to normalise each column, with
additional function arguments passed by \code{f_n_para}. By default, \code{f_n = "n_minmax"} and \code{f_n_para} is
set so that the columns of \code{x} are normalised using the min-max method, between 0 and 100.
}
\details{
Essentially, this function is similar to \code{\link[=Normalise]{Normalise()}} but brings parameters into the function arguments
rather than being wrapped in a list. It also does not allow individual normalisation.

See \code{\link[=Normalise]{Normalise()}} documentation for more details, and \code{vignette("normalise")}.
}
\examples{
# some made up data
X <- data.frame(uCode = letters[1:10],
                a = runif(10),
                b = runif(10)*100)
# normalise (defaults to min-max)
qNormalise(X)

}
