#' Results summary tables
#'
#' Generates fast results tables, either attached to the coin or as a data frame.
#'
#' Although results are available in a coin in `.$Data`, the format makes it difficult to quickly present results. This function
#' generates results tables that are suitable for immediate presentation, i.e. sorted by index or other indicators, and only including
#' relevant columns. Scores are also rounded by default, and there is the option to present scores or ranks.
#'
#' @param coin The coin object, or a data frame of indicator data
#' @param tab_type The type of table to generate. Either `"Summ"` (a single indicator plus rank), `"Aggs"` (all aggregated
#' scores/ranks above indicator level), or `"Full"` (all scores/ranks plus all group, denominator columns).
#' @param use Either `"scores"` (default), `"ranks"`, or `"groupranks"`. For the latter, `use_group` must be specified.
#' @param order_by A code of the indicator or aggregate to sort the table by. If not specified, defaults to the highest
#' aggregate level, i.e. the index in most cases. If `use_group` is specified, rows will also be sorted by the specified group.
#' @param nround The number of decimal places to round numerical values to. Defaults to 2.
#' @param use_group An optional grouping variable. If specified, the results table includes this group column,
#' and if `use = "groupranks"`, ranks will be returned with respect to the groups in this column.
#' @param out2 If `"df"`, outputs a data frame (tibble). Else if `"coin"` attaches to `.$Results` in an updated coin.
#'
#' @examples
#' #
#'
#' @return If `out2 = "df"`, the results table is returned as a data frame. If `out2 = "coin"`, this function returns an updated
#' coin with the results table attached to `.$Results`.
#'
#' @export
get_results <- function(coin, dset, tab_type = "Summ", also_get = NULL, use = "scores", order_by = NULL,
                       nround = 2, use_group = NULL, out2 = "df"){

  # CHECKS ------------------------------------------------------------------

  stopifnot(tab_type %in% c("Summ", "Aggs", "Full"),
            use %in% c("scores", "ranks", "groupranks"),
            is.numeric(nround),
            out2 %in% c("df", "coin"))

  # GET DATA ----------------------------------------------------------------

  # merge also_get with use_group
  also_get <- union(use_group, also_get)

  # data
  iData <- get_data(coin, dset = dset, also_get = also_get, use_group = use_group)

  # get meta col names
  mcols <- extract_iData(coin, iData, GET = "mCodes")

  # get iMeta
  iMeta <- coin$Meta$Ind
  # iMeta with only indicators and agg rows
  iMeta_ia <- iMeta[iMeta$Type %in% c("Indicator", "Aggregate"), ]
  # order it from top level down
  iMeta_ia <- iMeta_ia[order(-iMeta_ia$Level, iMeta_ia$Parent), ]

  # check if this is an aggregated data set
  if(any(iMeta_ia$iCode %nin% names(iData))){
    stop("The data set extracted by 'dset' does not seem to be an aggregated data set (indicator or aggregate codes are missing).")
  }

  # ORDERING ------------------------------------------------------------

  # results table (sorted by rows and cols)
  iData <- iData[c(mcols ,iMeta_ia$iCode)]

  # get the column name to use for sorting the df
  if(is.null(order_by)){
    sortcode <- iMeta_ia$iCode[iMeta_ia$Level == coin$Meta$maxlev]
  } else {
    if(order_by %nin% names(results)){
      stop("'order_by' is not found in the selected data set.")
    }
    sortcode <- order_by
  }

  iData$Rank <- rank(-1*iData[[sortcode]], na.last = "keep", ties.method = "min")

  # BUILD TABLE -----------------------------------------------------------------

  if(tab_type %in% c("Summ", "Summary")){

    # Just the indicator/index plus ranks
    tabout <- iData[c(mcols, sortcode, "Rank")]

  } else if (tab_type %in% c("Aggs", "Aggregates")){

    # All the aggregate scores
    tabout <- iData[c(mcols, "Rank", iMeta_ia$iCode[iMeta_ia$Type == "Aggregate"])]

  } else if (tab_type %in% c("Full", "FullWithDenoms")){

    # Get sorted indicator codes, not aggregates
    othercodes <- coin$Meta$Lineage[[1]]
    stopifnot(any(othercodes %in% names(iData)))

    # All the aggregate scores
    tabout <- iData[c(mcols, "Rank", iMeta_ia$iCode[iMeta_ia$Type == "Aggregate"], othercodes)]

  }

  # Sorting
  tabout <- tabout[order(-tabout[[sortcode]]),]

  # Rounding
  tabout <- roundDF(tabout, nround)

  # Ranks
  if(use == "ranks"){
    tabout <- tabout[colnames(tabout) != "Rank"]
    tabout <- rankDF(tabout)
  } else if (use =="groupranks"){
    if(is.null(use_group)){
      stop("If groupranks is specified, you need to also specify use_group.")
    }
    tabout <- tabout[colnames(tabout) != "Rank"]
    tabout <- rankDF(tabout, use_group = use_group)
    # sort by group
    tabout <- tabout[order(tabout[[use_group]]),]
  }

  # FINISH AND OUTPUT -------------------------------------------------

  if(out2 == "df"){

    return(tabout)

  } else if (out2 == "coin"){

    if(use == "scores"){
      coin$Results[[paste0(tab_type,"Score")]] <- tabout
    } else if (use == "ranks"){
      coin$Results[[paste0(tab_type,"Rank")]] <- tabout
    } else if (use == "groupranks"){
      coin$Results[[paste0(tab_type,"GrpRnk", use_group)]] <- tabout
    }
    return(coin)

  } else {
    stop("out2 not recognised!")
  }

}


#' Generate unit summary table
#'
#' Generates a summary table for a single unit. This is mostly useful in unit reports.
#'
#' This returns the scores and ranks for each indicator/aggregate as specified in `aglevs`. It orders the table so that
#' the highest aggregation levels are first. This means that if the index level is included, it will be first.
#'
#' @param coin A coin
#' @param usel A selected unit code
#' @param Levels The aggregation levels to display results from.
#' @param dset The data set within the coin to extract scores and ranks from
#' @param nround Number of decimals to round scores to, default 2.
#'
#' @examples
#' #
#'
#' @return A summary table as a data frame, containing scores and ranks for specified indicators/aggregates.
#'
#' @export
get_unit_summary <- function(coin, usel, Levels, dset = "Aggregated", nround = 2){

  # get rank and score tables
  scrs <- get_data(coin, dset = dset)
  rnks <- rankDF(scrs)

  if(usel %nin% scrs$uCode){
    stop("usel not found in selected data set!")
  }

  # get ind/agg codes etc and order
  iMeta_ <- coin$Meta$Ind[coin$Meta$Ind$Type %in% c("Indicator", "Aggregate"), ]
  iMeta_ <- iMeta_[order(-iMeta_$Level), ]

  if(any(Levels %nin% 1:max(iMeta_$Level))){
    stop("Levels must be integers between 1 and the maximum level.")
  }

  # select codes of levels
  agcodes <- iMeta_$iCode[iMeta_$Level %in% Levels]
  agnames <- iMeta_$iName[iMeta_$Level %in% Levels]

  if(any(agcodes %nin% names(scrs))){
    stop("One or more indicator or aggregate codes not found in the selected data set. You may need to point to
         an aggregated data set.")
  }

  # select cols corresponding to inds/aggs
  scrs1 <- scrs[agcodes]
  rnks1 <- rnks[agcodes]

  # make output table, inc. unit selection
  tabout <- data.frame(
    Code = agcodes,
    Name = agnames,
    Score = as.numeric(scrs1[scrs$uCode == usel, ]),
    Rank = as.numeric(rnks1[rnks$uCode == usel, ])
  )

  # round
  roundDF(tabout, nround)

}
