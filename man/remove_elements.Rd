% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audit_tools.R
\name{remove_elements}
\alias{remove_elements}
\title{Check the effect of removing indicators or aggregates}
\usage{
remove_elements(coin, Level, dset, iCode, quietly = FALSE)
}
\arguments{
\item{coin}{A coin class object, which must be constructed up to and including the aggregation step, i.e. using \code{\link[=Aggregate]{Aggregate()}}.}

\item{Level}{The level at which to remove elements. For example, \code{Level = 1} would check the effect of removing each indicator, one at
a time. \code{Level = 2} would check the effect of removing each of the aggregation groups above the indicator level, one at a time.}

\item{dset}{The name of the data set to take \code{iCode} from. Most likely this should be name of the aggregated data set, typically \code{"Aggregated"}.}

\item{iCode}{A character string indicating the indicator or aggregate code to extract from each iteration. I.e. normally this would be set to
the index code to compare the ranks of the index upon removing each indicator or aggregate. But it can be any code that is present in
\code{.$Data[[dset]]}.}

\item{quietly}{Logical: if \code{FALSE} (default) will output to the console an indication of progress. Might be useful when iterating over many
indicators. Otherwise set to \code{TRUE} to shut this up.}
}
\value{
A list with elements as follows:
\itemize{
\item \code{.$Scores}: a data frame where each column is the scores for each unit, with indicator/aggregate corresponding to the column name removed.
E.g. \code{.$Scores$Ind1} gives the scores resulting from removing "Ind1".
\item \code{.$Ranks}: as above but ranks
\item \code{.$RankDiffs}: as above but difference between nominal rank and rank on removing each indicator/aggregate
\item \code{.$RankAbsDiffs}: as above but absolute rank differences
\item \code{.$MeanAbsDiffs}: as above, but the mean of each column. So it is the mean (over units) absolute rank change resulting from removing each
indicator or aggregate.
}
}
\description{
This is an analysis function for seeing what happens when elements of the composite indicator are removed. This can help with "what if"
experiments and acts as different measure of the influence of each indicator or aggregate.
}
\details{
One way of looking at indicator "importance" in a composite indicator is via correlations. A different way is to see what happens if we
remove the indicator completely from the framework. If removing an indicator or a whole aggregation of indicators results in very little
rank change, it is one indication that perhaps it is not necessary to include it. Emphasis on \emph{one}: there may be many other things to take
into account.

This function works by successively setting the weight of each indicator or aggregate to zero. If the analysis is performed at the indicator
level, it creates a copy of the coin, sets the weight of the first indicator to zero, regenerates the results, and compares to the nominal
results (results when no weights are set to zero). It repeats this for each indicator in turn, such that each time one indicator is set to
zero weights, and the others retain their original weights. The output is a series of tables comparing scores and ranks (see Value).

Note that "removing the indicator" here means more precisely "setting its weight to zero". In most cases the first implies the second,
but check that the aggregation method that you are using satisfies this relationship. For example, if the aggregation method does not
use any weights, then setting the weight to zero will have no effect.

This function replaces the now-defunct \code{removeElements()} from COINr < v1.0.
}
\examples{
# build example coin
coin <- build_example_coin(quietly = TRUE)

# run function removing elements in level 2
l_res <- remove_elements(coin, Level = 3, dset = "Aggregated", iCode = "Index")

# get summary of rank changes
l_res$MeanAbsDiff

}
