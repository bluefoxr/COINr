# EXTENDED AUDITING TOOLS

#' Check the effect of removing indicators or aggregates
#'
#' This is an analysis function for seeing what happens when elements of the composite indicator are removed. This can help with "what if"
#' experiments and acts as different measure of the influence of each indicator or aggregate.
#'
#' One way of looking at indicator "importance" in a composite indicator is via correlations. A different way is to see what happens if we
#' remove the indicator completely from the framework. If removing an indicator or a whole aggregation of indicators results in very little
#' rank change, it is one indication that perhaps it is not necessary to include it. Emphasis on *one*: there may be many other things to take
#' into account.
#'
#' This function works by successively setting the weight of each indicator or aggregate to zero. If the analysis is performed at the indicator
#' level, it creates a copy of the COIN, sets the weight of the first indicator to zero, regenerates the results, and compares to the nominal
#' results (results when no weights are set to zero). It repeats this for each indicator in turn, such that each time one indicator is set to
#' zero weights, and the others retain their original weights. The output is a series of tables comparing scores and ranks (see Value).
#'
#' Note that "removing the indicator" here means more precisely "setting its weight to zero". In most cases the first implies the second,
#' but check that the aggregation method that you are using satisfies this relationship. For example, if the aggregation method does not
#' use any weights, then setting the weight to zero will have no effect.
#'
#' @param COIN The COIN, which must be constructed up to and including the aggregation step.
#' @param aglev The level at which to remove elements. For example, `aglev = 1` would check the effect of removing each indicator, one at
#' a time. `aglev = 2` would check the effect of removing each of the aggregation groups above the indicator level, one at a time.
#' @param isel A character string indicating the indicator or aggregate code to extract from each iteration. I.e. normally this would be set to
#' the index code to compare the ranks of the index upon removing each indicator or aggregate. But it can be any code that is present in
#' `.$Data$Aggregated`.
#' @param quietly Logical: if FALSE (default) will output to the console an indication of progress. Might be useful when iterating over many
#' indicators. Otherwise set to TRUE to shut this up.
#'
#' @examples
#' # check the effect of removing ASEM sub-pillars, one at a time
#' # First build ASEM index
#' ASEM <- build_ASEM()
#' # now run check at sub-index level (level 3), on index scores/ranks
#' CheckPillars <- removeElements(ASEM, 3, "Index")
#' # summary by pillar
#' CheckPillars$MeanAbsDiff
#' # have a look at the rest of the output list to see more details.
#'
#' @return A list with elements as follows:
#' * `.$Scores`: a data frame where each column is the scores for each unit, with indicator/aggregate corresponding to the column name removed.
#' E.g. `.$Scores$Ind1` gives the scores resulting from removing "Ind1".
#' * `.$Ranks`: as above but ranks
#' * `.$RankDiffs`: as above but difference between nominal rank and rank on removing each indicator/aggregate
#' * `.$RankAbsDiffs`: as above but absolute rank differences
#' * `.$MeanAbsDiffs`: as above, but the mean of each column. So it is the mean (over units) absolute rank change resulting from removing each
#' indicator or aggregate.
#'
#' @seealso
#' * [compTable()] Comparison table between two COINs
#' * [compTableMulti()] Comparison table between multiple COINs
#'
#' @export

removeElements <- function(COIN, aglev, isel, quietly = FALSE){

  ##----- Checks and Preps ----

  # check input first
  stopifnot(!is.null(COIN$Data$Aggregated),
            is.coin(COIN),
            aglev > 0)

  nlev <- COIN$Parameters$Nlevels

  if(aglev > nlev -1){
    stop("aglev must be between 1 (indicator level) and the number of levels minus one.")
  }

  # get scores of nominal and create table
  if(is.null(COIN$Data$Aggregated[[isel]])){
    stop("isel is not a column in .$Data$Aggregated - please check.")
  }
  Scores <- COIN$Data$Aggregated[c("UnitCode", isel)]
  colnames(Scores)[2] <- "Nominal"

  ##----- Get weights ----

  # this function uses setting weights to zero to remove things. In order to do this, we need the weights that were used to aggregate
  # Find which weights were used
  w_used <- COIN$Method$aggregate$agweights

  # now get the weights, and run some checks
  if(is.null(w_used)){
    # if this is NULL, then it means that the original weights were used
    if(is.null(COIN$Parameters$Weights$Original)){
      stop("No 'Original' weights found in .$Parameters$Weights. This is required to use this function.")
    } else {
      # get original weights
      wts <- COIN$Parameters$Weights$Original
    }
  } else if (is.character(w_used)) {
    # agweights specified by a character string
    wts <- COIN$Parameters$Weights[[w_used]]

    # check this now exists
    if(is.null(wts)){
      stop("Cannot find specified weight set in .$Parameters$Weights...")
    }
    # check is df
    if(!(is.data.frame(wts))){
      stop("Specified set of weights is not a data frame. Please check.")
    }

  } else if (is.data.frame(w_used)){
    wts <- w_used
  } else {
    stop("Argument 'agweights' is not in the correct format. Should be NULL, a character string or data frame.")
  }

  ##----- Loop over inds or aggs ----

  # Now we start with the removing process. First get codes of the elements to remove
  # we get them from the weights df
  icodes <- wts$Code[wts$AgLevel == aglev]
  # make sure tibble is not messing things up
  icodes <- unlist(icodes)

  # now we have to loop through these and set the weight of each to 0
  # Yes i am using a for loop because it is easier than forcing a map or apply. Deal with it! :)
  for (ii in 1:length(icodes)){

    if(!quietly){
      message(paste0("Iteration ", ii, " of ", length(icodes)))
    }

    # copy the COIN and weights
    COIN2 <- COIN
    wtsii <- wts

    # modify the weights. Set element to zero
    wtsii$Weight[wtsii$Code == icodes[ii]] <- 0

    # add a new set of weights - copy of the existing weights used
    COIN2$Parameters$Weights$Removed1 <- wtsii

    # point method
    COIN2$Method$aggregate$agweights <- "Removed1"

    # regenerate
    COIN2 <- regen(COIN2, quietly = TRUE)

    # Extract the output of interest and add to Scores df
    Scores <- cbind(Scores, COIN2$Data$Aggregated[[isel]])
    colnames(Scores)[ncol(Scores)] <- icodes[ii]

  }

  # now we generate tables as the output
  # ranks
  ranktab <- rankDF(Scores)
  # rank changes
  rankchg <- ranktab
  rankchg[-1] <- apply(rankchg[-1], 2, function(x) rankchg[2] - x)
  colnames(rankchg) <- colnames(ranktab)
  # absolute rank changes
  rankchgabs <- rankchg
  rankchgabs[-1] <- apply(rankchgabs[-1], 2, abs)
  # mean absolute rank changes
  MeanAbsDiff <- colMeans(rankchgabs[-1])

  # Output
  list(Scores = Scores,
       Ranks = ranktab,
       RankDiffs = rankchg,
       RankAbsDiffs = rankchgabs,
       MeanAbsDiff = MeanAbsDiff)

}
