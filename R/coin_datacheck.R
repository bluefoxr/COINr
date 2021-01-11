#' Country screener by data availability
#'
#' Screens countries based on data availability, according to rules used by the GII.
#' Rule 1: BOTH Input and Output indicators must have data availability greater than a threshold, set as
#' 66% by default, but can be changed using the "ind_thresh" parameter. Rule 1 is applied by default.
#' Rule 2: All pillars must have at least 66% of their sub-pillars that have 66% indicator data availability.
#' The 66% threshold is also alternatively specified by "ind_thresh". Rule 2 is NOT applied by default.
#'
#' @param COINobj The GII object
#' @param ind_thresh A data availability threshold, which controls both Rule 1 and 2. Default 0.66. Specify as a fraction.
#' @param unit_screen Logical: if TRUE, screens any units with indicator data availability < ind_thresh.
#' This is output in a new data set .$Data$Screened and added as a column in the output table.
#' @param Force A data frame with any additional countries to force inclusion or exclusion. First column is ISO code. Second column either "Include" or "Exclude" for each country to force.
#'
#' @examples \dontrun{coin_datacheck(COINobj)}
#'
#' @return An updated COINobj object with tables showing missing data, and a filtered list of countries to include in subsequent calculations.
#' @export

coin_datacheck <- function(COINobj, ind_thresh=2/3, unit_screen = FALSE, Force = NULL){

  # TO DO: CMD check tweaks, hook up with coin_aux

  # Write function arguments to object, FTR
  COINobj$Method$Screening$IndThresh <- ind_thresh
  COINobj$Method$Screening$unit_screen <- unit_screen
  COINobj$Method$Screening$Force <- Force

  # Isolate indicator data
  ind_data_only <- COINobj$Data$Raw %>% select(all_of(COINobj$Parameters$IndCodes))

  #--- RULE 1: 66% data for all countries or OUT

  nabyrow <- rowSums(is.na(ind_data_only)) # number of missing data by row
  Prc_avail = 1 - nabyrow/ncol(ind_data_only) # the percentage of data available

  data_avail <- data.frame(UnitCode = COINobj$Parameters$UnitCodes,
                           N_missing = nabyrow,
                           PrcDataAll = Prc_avail*100,
                           LowDataAll = Prc_avail<(ind_thresh))

  #--- RULE 2: 66% data availability at the sub-pillar level or ADIOS

  # the easiest way to do this is to loop over sub-pillars. Get first the index structure
  # (selects indicator codes plus all aggregation level columns/codes)
  agg_levels <- select(COINobj$Input$IndMeta, "IndCode" | starts_with("Agg"))

  data_avail_bygroup <- data.frame("UnitCode" = COINobj$Parameters$UnitCodes)

  for (ilev in 1:(ncol(agg_levels)-1)){ # loop over aggregation levels, except the last one

    agg1 <- pull(agg_levels,1) # names of indicators
    agg2 <- pull(agg_levels,ilev+1) # names of aggregation groups in the level above
    agg2_names <- unique(agg2) # only the names of agg level above (no repetitions)

    # pre-allocate a data frame for prc data availability
    d_avail_lev <- as.data.frame(matrix(NA, nrow = nrow(ind_data_only), ncol = length(agg2_names)))

    for (igroup in 1:length(agg2_names)){ # now looping over groups inside this level

      gname <- agg2_names[igroup] # select group name

      # get indicator codes belonging to group
      gcodes <- agg1[agg2 == gname]
      # get corresponding indicator columns
      ginds <- ind_data_only %>% select(all_of(gcodes))
      # now count prc data available and add to data frame
      d_avail_lev[,igroup] <- 100*rowSums(!is.na(ginds))/ncol(ginds)

    }

    # add column names (aggregation group names) to data availability table
    colnames(d_avail_lev) <- agg2_names
    # add to big table
    data_avail_bygroup <- cbind(data_avail_bygroup, d_avail_lev)

  }

  #----- Flag data availability at the sub-index level

  # # get subindex codes, remove NAs
  # SI_codes <- unique(agg_levels$Agg3)
  # SI_codes <- SI_codes[!is.na(SI_codes)]
  #
  # SI_avail <- select(data_avail_bygroup, all_of(SI_codes))
  #
  # data_avail <- data_avail %>% add_column(
  #   PrcDataInput = pull(SI_avail,1),
  #   PrcDataOuput = pull(SI_avail,2),
  #   LowDataSI = rowSums(SI_avail < ind_thresh*100) > 0
  # )

  #----- Now we will apply the rule of 66 at the pillar level. Slightly more complicated.

  # # get sub-pillar codes
  # SP_codes <- COINobj$Input$AggMeta$Agg1Code
  # # get data availability, by country, for each sub-pillar
  # SP_avail <- data_avail_bygroup %>% select(all_of(SP_codes))
  # # assign NAs to anything less than threshold
  # SP_avail[SP_avail < ind_thresh*100] <- NA
  #
  # P_codes <- unique(COINobj$Input$AggMeta$Agg2Code)
  # P_codes <- P_codes[!is.na(P_codes)]
  #
  # # pre-allocate a data frame for prc data availability
  # P_avail <- as.data.frame(matrix(NA, nrow = nrow(ind_data_only), ncol = length(P_codes)))
  #
  # for (igroup in 1:length(P_codes)){ # now looping over groups inside this level
  #
  #   gname <- P_codes[igroup] # select group name
  #
  #   # get indicator codes belonging to group
  #   gcodes <- agg_levels$Agg1[agg_levels$Agg2 == gname] %>% unique()
  #   # get corresponding indicator columns
  #   gSPs <- SP_avail %>% select(all_of(gcodes))
  #   # now count prc data available and add to data frame
  #   P_avail[,igroup] <- 100*rowSums(!is.na(gSPs))/ncol(gSPs)
  #
  # }
  #
  # # add pillar names
  # colnames(P_avail) <- P_codes
  # # add code column
  # P_avail <- P_avail %>% add_column(
  #   UnitCode = COINobj$Parameters$UnitCodes,
  #   .before = 1)
  #
  # # get threshold for Rule 2. If Rule2Thresh is specified, use that, otherwise just use the global threshold
  # if(is.null(Rule2Thresh)){ind_threshP=ind_thresh}else{ind_threshP=Rule2Thresh}
  #
  # # add low data flag to output table
  # data_avail <- data_avail %>% add_column(
  #   LowDataPillar = rowSums(P_avail < ind_threshP*100) > 0
  # )

  # Now add final column which says if country is included or not, if asked for
  if (unit_screen == TRUE){
    data_avail <- data_avail %>%
      mutate(Included = LowDataAll == FALSE)
  }

  if (!is.null(Force)){ # if some countries to force include/exclude
    # convert to logical
    Force[2] <- Force[2]=="Included"
    # substitute in output table
    data_avail$Included[ data_avail$UnitCode %in% Force$UnitCode[Force$Status == TRUE] ] <- TRUE
    data_avail$Included[ data_avail$UnitCode %in% Force$UnitCode[Force$Status == FALSE] ] <- FALSE
  }


  # add summary tables to object
  COINobj$Analysis$DataAvail$Summary <- data_avail
  COINobj$Analysis$DataAvail$ByGroup <- data_avail_bygroup

  # create new data set which filters out the countries that didn't make the cut
  if (unit_screen == TRUE){
    COINobj$Data$Screened <- filter(COINobj$Data$Raw, COINobj$Analysis$DataAvail$Summary$Included)
  }
  return(COINobj)
}
