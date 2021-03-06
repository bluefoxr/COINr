% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_sensitivity.R
\name{SA_sample}
\alias{SA_sample}
\title{Generate sample for sensitivity analysis}
\usage{
SA_sample(N, d)
}
\arguments{
\item{N}{The number of sample points per dimension.}

\item{d}{The dimensionality of the sample}
}
\description{
Generates an input sample for a Monte Carlo estimation of global sensitivity indices. Used in
the sensitivity() function. The total sample size will be N(d+2).
}
