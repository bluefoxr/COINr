% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_resultsdash.R
\name{iplotRadar}
\alias{iplotRadar}
\title{Radar chart}
\usage{
iplotRadar(
  COIN,
  dset = "Raw",
  usel = NULL,
  aglev = NULL,
  isel = NULL,
  addstat = "none",
  statgroup = NULL,
  statgroup_name = NULL
)
}
\arguments{
\item{COIN}{The COIN object, or a data frame of indicator data.}

\item{dset}{The data set to use in the table}

\item{usel}{Character vector of unit code(s) to plot data from}

\item{aglev}{The selected aggregation level to take indicator data from,
where 1 is the base indicator level, and 2, 3 etc are higher aggregation levels}

\item{isel}{The indicator or aggregation code(s) to plot}

\item{addstat}{Adds the statistic of the scores in each dimension as a separate trace. If "mean" adds the overall
mean for each dimension/indicator. If "median" adds the overall median. If "groupmean" or "groupmedian", adds the
group mean or median respectively of the first unit specified in usel, using the group specified by statgroup. Default "none", i.e. no extra trace.
Using a group mean or median won't make sense unless all of selected units are from the same group.}

\item{statgroup}{A grouping variable (must be present in dset) if addstat = "groupmean" or "groupmedian"}

\item{statgroup_name}{An optional name to display for statgroup. In the legend this will appear as e.g. "statgroup_name group mean".
Defaults to statgroup.}
}
\value{
Interactive table
}
\description{
Generates an interactive radar chart for a specified unit.
}
\examples{
\dontrun{
iplotRadar(ASEM, dset = "Aggregated", usel = c("AUT", "CHN"), isel = "Physical", aglev = 1)
}

}
