% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalise.R
\name{n_scaled}
\alias{n_scaled}
\title{Scale a vector}
\usage{
n_scaled(x, npara = c(0, 100), scale_factor = 100)
}
\arguments{
\item{x}{A numeric vector}

\item{npara}{Parameters as a vector \code{c(l, u)}. See description.}

\item{scale_factor}{Optional scaling factor to apply to the result. Default 100.}
}
\value{
Scaled vector
}
\description{
Scales a vector for normalisation using the method applied in the GII2020 for some indicators. This
does \code{x_scaled <- (x-l)/(u-l) * scale_factor}. Note this is \emph{not} the minmax transformation (see \code{\link[=n_minmax]{n_minmax()}}).
This is a linear transformation with shift \code{u} and scaling factor \code{u-l}.
}
\details{
This function also supports parameter specification in \code{iMeta} for the \code{\link[=Normalise.coin]{Normalise.coin()}} method.
To do this, add columns \code{scaled_lower}, \code{scaled_upper} and \code{scale_factor} to the \code{iMeta} table, which specify the
first and second elements of \code{npara}, respectively. Then set \code{f_n_para = "use_iMeta"} within the
\code{global_specs} list. See also examples in the \href{https://bluefoxr.github.io/COINr/articles/normalise.html}{normalisation vignette}.
}
\examples{
x <- runif(20)
n_scaled(x, npara = c(1,10))

}
