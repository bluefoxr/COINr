#' Detailed unit data check and screener by data availability
#'
#' Gives detailed tables of data availability, and optionally screens units based on a data
#' availability threshold and presence of zeros. Units can be optionally "forced" to be included or excluded, making
#' exceptions for the data availability threshold.
#'
#' The two main criteria of interest are NA values, and zeros. The summary table gives percentages of
#' NA values for each unit, across indicators, and percentage zero values (*as a percentage of non-NA values*).
#' Each unit is flagged as having low data or too many zeros based on thresholds.
#'
#' This function currently only supports COINs as inputs, not data frames.
#'
#' @param COIN The COIN object
#' @param dset The data set to be checked/screened
#' @param ind_thresh A data availability threshold used for flagging low data and screening units if unit_screen != "none". Default 0.66. Specify as a fraction.
#' @param zero_thresh As ind_thresh but for non-zero values. Defaults to 0.05, i.e. it will flag any units with less than 5% non-zero values (equivalently more than 95% zero values).
#' @param unit_screen Specifies whether and how to screen units based on data availability or zero values.
#' * If set to "none" (default), does not screen any units.
#' * If set to "byNA", screens units with data availability below ind_thresh
#' * If set to "byzeros", screens units with non-zero values below zero_thresh
#' * If set to "byNAandzeros", screens units based on either of the previous two criteria being true.
#' * If you simply want to force a unit or units to be excluded (without any other screening), use the Force argument and set unit_screen = TRUE.
#' unit_screen != "none" outputs a new data set .$Data$Screened.
#' @param Force A data frame with any additional countries to force inclusion or exclusion. First column is "UnitCode". Second column "Status" either "Include" or "Exclude" for each country to force.
#' @param out2 Where to output the results. If "COIN" (default for COIN input), appends to updated COIN,
#' otherwise if "df" outputs to data frame.
#'
#' @importFrom dplyr select starts_with pull mutate filter
#'
#' @examples \dontrun{checkData(COIN)}
#'
#' @return An updated COIN object with tables showing missing data, and a filtered list of countries to include in subsequent calculations.
#' @export

checkData <- function(COIN, dset = NULL, ind_thresh = NULL, zero_thresh = NULL,
                      unit_screen = "none", Force = NULL, out2 = "COIN"){

  # Check input type. If not COIN, exit.
  if (!("COIN object" %in% class(COIN))){ # COIN obj
    stop("This function currently only supports COINs as inputs.")
  }
  # Check for dset. If not specified, exit.
  if (is.null(dset)){
    stop("dset is NULL. Please specify which data set to operate on.")
  }

  ##----- SET DEFAULTS -------##
  # Done here because otherwise if we use regen, this input could be input as NULL
  if(is.null(ind_thresh)){
    ind_thresh <- 2/3
  }
  if(is.null(zero_thresh)){
    zero_thresh <- 0.05
  }
  if(is.null(unit_screen)){
    unit_screen <- "none"
  }

  # Write function arguments to object, FTR
  COIN$Method$checkData$dset <- dset
  COIN$Method$checkData$ind_thresh <- ind_thresh
  COIN$Method$checkData$zero_thresh <- zero_thresh
  COIN$Method$checkData$unit_screen <- unit_screen
  COIN$Method$checkData$Force <- Force

  # Isolate indicator data
  out1 <- getIn(COIN, dset = dset)
  ind_data_only <- out1$ind_data_only

  #--- Check overall data availability

  nabyrow <- rowSums(is.na(ind_data_only)) # number of missing data by row
  zerobyrow <- rowSums(ind_data_only == 0, na.rm = TRUE) # number of zeros for each row
  nazerobyrow <- nabyrow + zerobyrow # number of zeros or NAs for each row
  Prc_avail = 1 - nabyrow/ncol(ind_data_only) # the percentage of data available
  Prc_nonzero = 1 - zerobyrow/(ncol(ind_data_only) - nabyrow) # the percentage of non zeros
  #Prc_avail_and_nonzero = 1 - nazerobyrow/ncol(ind_data_only) # the percentage of non zeros AND not NA

  data_avail <- data.frame(UnitCode = out1$UnitCodes,
                           N_missing = nabyrow,
                           N_zero = zerobyrow,
                           N_miss_or_zero = nazerobyrow,
                           PrcDataAll = Prc_avail*100,
                           PrcNonZero = Prc_nonzero*100,
                           #PrcDataAndNonZero = Prc_avail_and_nonzero*100,
                           LowDataAll = Prc_avail < ind_thresh,
                           ZeroFlag = Prc_nonzero < zero_thresh,
                           LowDatOrZeroFlag = (Prc_avail < ind_thresh) | (Prc_nonzero < zero_thresh))

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
  if (unit_screen == "byNA"){
    data_avail <- cbind(data_avail, Included = data_avail$LowDataAll == FALSE)
  } else if (unit_screen == "byzeros"){
    data_avail <- cbind(data_avail, Included = data_avail$ZeroFlag == FALSE)
  } else if (unit_screen == "byNAandzeros"){
    data_avail <- cbind(data_avail, Included = data_avail$LowDatOrZeroFlag == FALSE)
  } else {
    data_avail <- cbind(data_avail, Included = TRUE)
  }

  if (!is.null(Force)){ # if some countries to force include/exclude
    # convert to logical
    Force[2] <- Force[2]=="Include"
    # substitute in output table

    data_avail$Included[ data_avail$UnitCode %in% Force$UnitCode[Force$Status == TRUE] ] <- TRUE
    data_avail$Included[ data_avail$UnitCode %in% Force$UnitCode[Force$Status == FALSE] ] <- FALSE
  }

  if (unit_screen != "none"){
    # create new data set which filters out the countries that didn't make the cut
    ScreenedData <- dplyr::filter(out1$ind_data, data_avail$Included)
    # units that are removed
    ScreenedUnits <- data_avail$UnitCode[!data_avail$Included]
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

    if (unit_screen != "none"){
      COIN$Data$Screened <- ScreenedData
      eval(parse(text=paste0("COIN$Analysis$",dset,"$RemovedUnits<- ScreenedUnits")))
    }
    return(COIN)

  } else {
    stop("out2 not recognised, should be either COIN or list")
  }
}
