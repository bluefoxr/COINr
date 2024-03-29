% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{compare_coins}
\alias{compare_coins}
\title{Compare two coins}
\usage{
compare_coins(
  coin1,
  coin2,
  dset,
  iCode,
  also_get = NULL,
  compare_by = "ranks",
  sort_by = NULL,
  decreasing = FALSE
)
}
\arguments{
\item{coin1}{A coin class object}

\item{coin2}{A coin class object}

\item{dset}{A data set that is found in \code{.$Data}.}

\item{iCode}{The name of a column that is found in \code{.$Data[[dset]]}.}

\item{also_get}{Optional metadata columns to attach to the table: see \code{\link[=get_data]{get_data()}}.}

\item{compare_by}{Either \code{"ranks"} which produces a comparison using ranks, or else \code{"scores"}, which instead
uses scores. Note that scores may be very different if the methodology is different from one coin to another,
e.g. for different normalisation methods.}

\item{sort_by}{Optionally, a column name of the output data frame to sort rows by. Can be either
\code{"coin.1"}, \code{"coin.2"}, \code{"Diff"}, \code{"Abs.diff"} or possibly a column name imported using \code{also_get}.}

\item{decreasing}{Argument to pass to \code{\link[=order]{order()}}: how to sort.}
}
\value{
A data frame of comparison information.
}
\description{
Compares two coin class objects using a specified \code{iCode} (column of data) from specified data sets.
}
\details{
This function replaces the now-defunct \code{compTable()} from COINr < v1.0.
}
\examples{
# build full example coin
coin <- build_example_coin(quietly = TRUE)

# copy coin
coin2 <- coin

# change to prank function (percentile ranks)
# we don't need to specify any additional parameters (f_n_para) here
coin2$Log$Normalise$global_specs <- list(f_n = "n_prank")

# regenerate
coin2 <- Regen(coin2)

# compare index, sort by absolute rank difference
compare_coins(coin, coin2, dset = "Aggregated", iCode = "Index",
              sort_by = "Abs.diff", decreasing = TRUE)

}
