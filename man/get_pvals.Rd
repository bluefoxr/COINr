% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlations.R
\name{get_pvals}
\alias{get_pvals}
\title{P-values for correlations in a data frame or matrix}
\usage{
get_pvals(X, ...)
}
\arguments{
\item{X}{A numeric matrix or data frame}

\item{\dots}{Additional arguments passed to function \code{\link[=cor.test]{cor.test()}}, e.g. \code{conf.level = 0.95}.}
}
\value{
Matrix of p-values
}
\description{
This is a stripped down version of the "cor.mtest()" function from the "corrplot" package. It uses
the \code{\link[stats:cor.test]{stats::cor.test()}} function to calculate pairwise p-values. Unlike the corrplot version, this
only calculates p-values, and not confidence intervals. Credit to corrplot for this code, I only
replicate it here to avoid depending on their package for a single function.
}
\examples{
# a matrix of random numbers, 3 cols
x <- matrix(runif(30), 10, 3)

# get correlations between cols
cor(x)

# get p values of correlations between cols
get_pvals(x)

}
