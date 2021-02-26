#' Detailed unit data check and screener by data availability
#'
#' Screens countries based on data availability, according to rules used by the GII.
#' Rule 1: BOTH Input and Output indicators must have data availability greater than a threshold, set as
#' 66% by default, but can be changed using the "ind_thresh" parameter. Rule 1 is applied by default.
#' Rule 2: All pillars must have at least 66% of their sub-pillars that have 66% indicator data availability.
#' The 66% threshold is also alternatively specified by "ind_thresh". Rule 2 is NOT applied by default.
#'
#' @param COIN The GII object
#' @param dset The data set to be checked/screened
#' @param ind_thresh A data availability threshold, which controls both Rule 1 and 2. Default 0.66. Specify as a fraction.
#' @param unit_screen Logical: if TRUE, screens any units with indicator data availability < ind_thresh.
#' This is output in a new data set .$Data$Screened and added as a column in the output table.
#' @param Force A data frame with any additional countries to force inclusion or exclusion. First column is ISO code. Second column either "Include" or "Exclude" for each country to force.
#'
#' @importFrom dplyr select starts_with pull mutate filter
#'
#' @examples \dontrun{checkData(COIN)}
#'
#' @return An updated COIN object with tables showing missing data, and a filtered list of countries to include in subsequent calculations.
#' @export

checkData <- function(COIN, dset = "Raw", ind_thresh=2/3, unit_screen = FALSE,
                           Force = NULL, out2 = "COIN"){

  # Write function arguments to object, FTR
  COIN$Method$Screening$IndThresh <- ind_thresh
  COIN$Method$Screening$unit_screen <- unit_screen
  COIN$Method$Screening$Force <- Force

  # Isolate indicator data
  out1 <- getIn(COIN, dset = dset)
  ind_data_only <- out1$ind_data_only

  #--- Check overall data availability

  nabyrow <- rowSums(is.na(ind_data_only)) # number of missing data by row
  Prc_avail = 1 - nabyrow/ncol(ind_data_only) # the percentage of data available

  data_avail <- data.frame(UnitCode = COIN$Parameters$UnitCodes,
                           N_missing = nabyrow,
                           PrcDataAll = Prc_avail*100,
                           LowDataAll = Prc_avail<(ind_thresh))

  #--- Check data availability by group

  # the easiest way to do this is to loop over groups. Get first the index structure
  # (selects indicator codes plus all aggregation level columns/codes)
  agg_levels <- dplyr::select(COIN$Input$IndMeta, "IndCode" | dplyr::starts_with("Agg"))

  data_avail_bygroup <- data.frame("UnitCode" = out1$UnitCodes)

  for (ilev in 1:(ncol(agg_levels)-1)){ # loop over aggregation levels, except the last one

    agg1 <- dplyr::pull(agg_levels,1) # names of indicators
    agg2 <- dplyr::pull(agg_levels,ilev+1) # names of aggregation groups in the level above
    agg2_names <- unique(agg2) # only the names of agg level above (no repetitions)

    # pre-allocate a data frame for prc data availability
    d_avail_lev <- as.data.frame(matrix(NA, nrow = nrow(ind_data_only), ncol = length(agg2_names)))

    for (igroup in 1:length(agg2_names)){ # now looping over groups inside this level

      gname <- agg2_names[igroup] # select group name

      # get indicator codes belonging to group
      gcodes <- agg1[agg2 == gname]
      # get corresponding indicator columns
      ginds <- ind_data_only[gcodes]
      # now count prc data available and add to data frame
      d_avail_lev[,igroup] <- 100*rowSums(!is.na(ginds))/ncol(ginds)

    }

    # add column names (aggregation group names) to data availability table
    colnames(d_avail_lev) <- agg2_names
    # add to big table
    data_avail_bygroup <- cbind(data_avail_bygroup, d_avail_lev)

  }

  # Now add final column which says if country is included or not, if asked for
  if (unit_screen == TRUE){
    data_avail <- cbind(data_avail, Included = data_avail$LowDataAll == FALSE)
  }

  if (!is.null(Force)){ # if some countries to force include/exclude
    # convert to logical
    Force[2] <- Force[2]=="Include"
    # substitute in output table

    data_avail$LowDataAll[ data_avail$UnitCode %in% Force$UnitCode[Force$Status == TRUE] ] <- FALSE
    data_avail$LowDataAll[ data_avail$UnitCode %in% Force$UnitCode[Force$Status == FALSE] ] <- TRUE
  }

  if (unit_screen == TRUE){
    # create new data set which filters out the countries that didn't make the cut
    ScreenedData <- dplyr::filter(out1$ind_data, !data_avail$LowDataAll)
    # units that are removed
    ScreenedUnits <- data_avail$UnitCode[data_avail$LowDataAll]
  }

  if (out2 == "list"){

    # write to a list
    return(list(
      MissDatSummary = data_avail,
      MissDatByGroup = data_avail_bygroup,
      ScreenedData = ScreenedData,
      RemovedUnits = ScreenedUnits
    ))

  } else if (out2 == "COIN") {

    # add summary tables to COIN
    eval(parse(text=paste0("COIN$Analysis$",dset,"$MissDatSummary<- data_avail")))
    eval(parse(text=paste0("COIN$Analysis$",dset,"$MissDatByGroup<- data_avail_bygroup")))

    if (unit_screen == TRUE){
      COIN$Data$Screened <- ScreenedData
      eval(parse(text=paste0("COIN$Analysis$",dset,"$RemovedUnits<- ScreenedUnits")))
    }
    return(COIN)

  } else {
    stop("out2 not recognised, should be either COIN or list")
  }
}
