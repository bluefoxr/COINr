#' Results summary tables
#'
#' Generates fast results tables, either attached to the coin or as a data frame.
#'
#' Although results are available in a coin in `.$Data`, the format makes it difficult to quickly present results. This function
#' generates results tables that are suitable for immediate presentation, i.e. sorted by index or other indicators, and only including
#' relevant columns. Scores are also rounded by default, and there is the option to present scores or ranks.
#'
#' See also `vignette("results")` for more info.
#'
#' This function replaces the now-defunct `getResults()` from COINr < v1.0.
#'
#' @param coin The coin object, or a data frame of indicator data
#' @param dset Name of data set in `.$Data`
#' @param also_get Names of further columns to attach to table.
#' @param tab_type The type of table to generate. Either `"Summ"` (a single indicator plus rank), `"Aggs"` (all aggregated
#' scores/ranks above indicator level), or `"Full"` (all scores/ranks plus all group, denominator columns).
#' @param use Either `"scores"` (default), `"ranks"`, or `"groupranks"`. For the latter, `use_group` must be specified.
#' @param order_by A code of the indicator or aggregate to sort the table by. If not specified, defaults to the highest
#' aggregate level, i.e. the index in most cases. If `use_group` is specified, rows will also be sorted by the specified group.
#' @param nround The number of decimal places to round numerical values to. Defaults to 2.
#' @param use_group An optional grouping variable. If specified, the results table includes this group column,
#' and if `use = "groupranks"`, ranks will be returned with respect to the groups in this column.
#' @param out2 If `"df"`, outputs a data frame (tibble). Else if `"coin"` attaches to `.$Results` in an updated coin.
#' @param dset_indicators Optional data set from which to take only indicator (level 1) data from. This can be set to `"Raw"`
#' for example, so that all aggregates come from the aggregated data set, and the indicators come from the raw data set. This
#' can make more sense in presenting results in many cases, so that the "real" indicator data is visible.
#'
#' @examples
#' # build full example coin
#' coin <- build_example_coin(quietly = TRUE)
#'
#' # get results table
#' df_results <- get_results(coin, dset = "Aggregated", tab_type = "Aggs")
#'
#' head(df_results)
#'
#' @return If `out2 = "df"`, the results table is returned as a data frame. If `out2 = "coin"`, this function returns an updated
#' coin with the results table attached to `.$Results`.
#'
#' @export
get_results <- function(coin, dset, tab_type = "Summ", also_get = NULL, use = "scores", order_by = NULL,
                       nround = 2, use_group = NULL, dset_indicators = NULL, out2 = "df"){

  # CHECKS ------------------------------------------------------------------

  stopifnot(tab_type %in% c("Summ", "Aggs", "Full"),
            use %in% c("scores", "ranks", "groupranks"),
            is.numeric(nround),
            out2 %in% c("df", "coin"))

  check_coin_input(coin)

  # GET DATA ----------------------------------------------------------------

  # merge also_get with use_group
  also_get <- union(use_group, also_get)

  # data
  iData <- get_data(coin, dset = dset, also_get = also_get, use_group = use_group)

  # optionally indicator data from another data set (probably raw)
  if(!is.null(dset_indicators)){
    iDatai <- get_dset(coin, dset = dset_indicators)
    # order rows by iData (also filter to only units in iData)
    iDatai <- iDatai[match(iData$uCode, iDatai$uCode), ]
    # get all iData cols which are indicators
    ind_cols <- names(iData)[names(iData) %in% coin$Meta$Ind$iCode[which(coin$Meta$Ind$Type == "Indicator")]]
    # hot swap
    stopifnot(all(ind_cols %in% names(iDatai)))
    iData[ind_cols] <- iDatai[ind_cols]
  }

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
    if(order_by %nin% names(iData)){
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
  tabout <- round_df(tabout, nround)

  # Ranks
  if(use == "ranks"){
    tabout <- tabout[colnames(tabout) != "Rank"]
    tabout <- rank_df(tabout)
  } else if (use =="groupranks"){
    if(is.null(use_group)){
      stop("If groupranks is specified, you need to also specify use_group.")
    }
    tabout <- tabout[colnames(tabout) != "Rank"]
    tabout <- rank_df(tabout, use_group = use_group)
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
#' This function replaces the now-defunct `getUnitSummary()` from COINr < v1.0.
#'
#' @param coin A coin
#' @param usel A selected unit code
#' @param Levels The aggregation levels to display results from.
#' @param dset The data set within the coin to extract scores and ranks from
#' @param nround Number of decimals to round scores to, default 2. Set to `NULL` to disable rounding.
#'
#' @examples
#' # build full example coin
#' coin <- build_example_coin(quietly = TRUE)
#'
#' # summary of scores for IND at levels 4, 3 and 2
#' get_unit_summary(coin, usel = "IND", Levels = c(4,3,2), dset = "Aggregated")
#'
#' @return A summary table as a data frame, containing scores and ranks for specified indicators/aggregates.
#'
#' @export
get_unit_summary <- function(coin, usel, Levels, dset = "Aggregated", nround = 2){

  # get rank and score tables
  scrs <- get_data(coin, dset = dset)
  rnks <- rank_df(scrs)

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
  if(!is.null(nround)){
    df_out <- round_df(tabout, nround)
  } else {
    df_out <- tabout
  }

  df_out

}


#' Generate strengths and weaknesses for a specified unit
#'
#' Generates a table of strengths and weaknesses for a selected unit, based on ranks, or ranks within
#' a specified grouping variable.
#'
#' This currently only works at the indicator level. Indicators with `NA` values for the selected unit are ignored.
#' Strengths and weaknesses mean the `topN`-ranked indicators for the selected unit. Effectively, this takes the rank that the
#' selected unit has in each indicator, sorts the ranks, and takes the top N highest and lowest.
#'
#' This function must be used with a little care: indicators should be adjusted for their directions before use,
#' otherwise a weakness might be counted as a strength, and vice versa. Use the `adjust_direction` parameter
#' to help here.
#'
#' A further useful parameter is `unq_discard`, which also filters out any indicators with a low number of
#' unique values, based on a specified threshold. Also `min_discard` which filters out any indicators which
#' have the minimum rank.
#'
#' The best way to use this function is to play around with the settings a little bit. The reason being that
#' in practice, indicators have very different distributions and these can sometimes lead to unexpected
#' outcomes. An example is if you have an indicator with 50% zero values, and the rest non-zero (but unique).
#' Using the sport ranking system, all units with zero values will receive a rank which is equal to the number
#' of units divided by two. This then might be counted as a "strength" for some units with overall low scores.
#' But a zero value can hardly be called a strength. This is where the `min_discard` function can help out.
#'
#' Problems such as these mainly arise when e.g. generating a large number of country profiles.
#'
#' This function replaces the now-defunct `getStrengthNWeak()` from COINr < v1.0.
#'
#' @param coin A coin
#' @param dset The data set to extract indicator data from, to use as strengths and weaknesses.
#' @param usel A selected unit code
#' @param topN The top N indicators to report
#' @param bottomN The bottom N indicators to report
#' @param withcodes If `TRUE` (default), also includes a column of indicator codes. Setting to `FALSE` may be more useful
#' in generating reports, where codes are not helpful.
#' @param use_group An optional grouping variable to use for reporting
#' in-group ranks. Specifying this will report the ranks of the selected unit within the group of `use_group`
#' to which it belongs.
#' @param unq_discard Optional parameter for handling discrete indicators. Some indicators may be binary
#' variables of the type "yes = 1", "no = 0". These may be picked up as strengths or weaknesses, when they
#' may not be wanted to be highlighted, since e.g. maybe half of units will have a zero or a one. This argument
#' takes a number between 0 and 1 specifying a unique value threshold for ignoring indicators as strengths. E.g.
#' setting `prc_unq_discard = 0.2` will ensure that only indicators with at least 20% unique values will be
#' highlighted as strengths or weaknesses. Set to `NULL` to disable (default).
#' @param min_discard If `TRUE` (default), discards any strengths which correspond to the minimum rank for the given
#' indicator. See details.
#' @param report_level Aggregation level to report parent codes from. For example, setting
#' `report_level = 2` (default) will add a column to the strengths and weaknesses tables which reports the aggregation
#' group from level 2, to which each reported indicator belongs.
#' @param with_units If `TRUE` (default), includes indicator units in output tables.
#' @param adjust_direction If `TRUE`, will adjust directions of indicators according to the "Direction" column
#' of `IndMeta`. By default, this is `TRUE` *if* `dset = "Raw"`, and `FALSE` otherwise.
#' @param sig_figs Number of significant figures to round values to. If `NULL` returns values as they are.
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get strengths and weaknesses for ESP
#' get_str_weak(coin, dset = "Raw", usel = "ESP")
#'
#' @return A list containing a data frame `.$Strengths`, and a data frame `.$Weaknesses`.
#' Each data frame has columns with indicator code, name, rank and value (for the selected unit).
#'
#' @export

get_str_weak <- function(coin, dset, usel = NULL, topN = 5, bottomN = 5, withcodes = TRUE,
                             use_group = NULL, unq_discard = NULL, min_discard = TRUE, report_level = NULL,
                             with_units = TRUE, adjust_direction = NULL, sig_figs = 3){

  # PREP --------------------------------------------------------------------

  # get iMeta
  iMeta_ <- coin$Meta$Ind[coin$Meta$Ind$Type == "Indicator", ]

  # indicator codes
  iCodes <- iMeta_$iCode

  stopifnot(length(usel) == 1,
            is.character(usel),
            topN %in% 1:length(iCodes),
            bottomN %in% 1:length(iCodes),
            is.logical(withcodes),
            unq_discard >= 0,
            unq_discard <= 1,
            is.logical(min_discard),
            report_level %in% 2:max(iMeta_$Level),
            is.logical(with_units))

  # scores
  if(is.null(dset)) dset <- "Raw"
  data_scrs <- get_dset(coin, dset = dset, also_get = use_group)

  if(usel %nin% data_scrs$uCode){
    stop("usel not found in selected data set!")
  }

  # first, we have to adjust for direction
  if(is.null(adjust_direction)){
    if(dset == "Raw"){
      adjust_direction <- TRUE
    } else {
      adjust_direction <- FALSE
    }
  }
  stopifnot(is.logical(adjust_direction))

  # GET S&W -----------------------------------------------------------------

  # make a copy to adjust by direction
  data_scrs1 <- data_scrs

  if(adjust_direction){
    # note: directions are in the same order as iCodes
    directions <- iMeta_$Direction
    data_scrs1[iCodes] <- as.data.frame(mapply(`*`, data_scrs1[iCodes], directions))
  }

  data_rnks <- rank_df(data_scrs1, use_group = use_group)

  # unique value filtering
  if(!is.null(unq_discard)){
    # find fraction of unique vals for each indicator
    frc_unique <- apply(data_scrs[iCodes], MARGIN = 2,
                        function(x){
                          length(unique(x))/length(x)
                        })
    # filter indicator codes to only the ones with frac unique above thresh
    iCodes <- iCodes[frc_unique > unq_discard]
  }

  # isolate the row and indicator cols
  rnks_usel <- data_rnks[data_rnks$uCode == usel, iCodes]

  # remove NAs
  rnks_usel <- rnks_usel[,!is.na(as.numeric(rnks_usel))]

  # Also need to (optionally) remove minimum rank entries
  # (by min I mean MAX, i.e. min SCORE)
  if(min_discard){
    rnks_min <-  as.data.frame(lapply(data_rnks[colnames(rnks_usel)], max, na.rm = T))
    rnks_usel <- rnks_usel[,!(rnks_usel == rnks_min)]
  }

  # sort by row values
  rnks_usel <- rnks_usel[ ,order(as.numeric(rnks_usel[1,]))]

  # get strengths and weaknesses
  Scodes <- colnames(rnks_usel)[1:topN]
  Wcodes <- colnames(rnks_usel)[ (ncol(rnks_usel) - bottomN + 1) : ncol(rnks_usel) ]

  # find agg level column of interest
  if(is.null(report_level)){
    report_level <- 2
  }
  lin <- coin$Meta$Lineage
  agcolname <- names(lin)[report_level]

  # get values and round if asked
  sValues <- as.numeric(data_scrs[data_scrs$uCode == usel ,Scodes])
  wValues <- as.numeric(data_scrs[data_scrs$uCode == usel ,Wcodes])

  if(!is.null(sig_figs)){
    stopifnot(sig_figs %in% 0:100)
    sValues <- signif(sValues, sig_figs)
    wValues <- signif(wValues, sig_figs)
  }


  # MAKE TABLES -------------------------------------------------------------

  strengths <- data.frame(
    Code = Scodes,
    Name = iMeta_$iName[match(Scodes, iMeta_$iCode)],
    Dimension = lin[[agcolname]][match(Scodes, lin[[1]])],
    Rank = as.numeric(rnks_usel[Scodes]),
    Value = sValues
  )
  names(strengths)[3] <- agcolname

  weaks <- data.frame(
    Code = Wcodes,
    Name = iMeta_$iName[match(Wcodes, iMeta_$iCode)],
    Dimension = lin[[agcolname]][match(Wcodes, lin[[1]])],
    Rank = as.numeric(rnks_usel[Wcodes]),
    Value = wValues
  )
  names(weaks)[3] <- agcolname

  # units
  # if units col exists and requested
  if(with_units & !is.null(iMeta_$Unit)){
    strengths$Unit <- iMeta_$Unit[match(Scodes, iMeta_$iCode)]
    weaks$Unit <- iMeta_$Unit[match(Wcodes, iMeta_$iCode)]
  }

  # remove indicator codes if needed
  if(!withcodes){
    strengths <- strengths[-1]
    weaks <- weaks[-1]
  }


  # OUTPUT ------------------------------------------------------------------

  list(
    Strengths = strengths,
    Weaknesses = weaks
  )
}
