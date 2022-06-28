#' Screen units based on data availability
#'
#' This is a generic function for screening units/rows based on data availability. See method documentation
#' for more details:
#'
#' * [Screen.data.frame()]
#' * [Screen.coin()]
#' * [Screen.purse()]
#'
#' @param x Object to be screened
#' @param ... arguments passed to or from other methods.
#'
#' @return An object of the same class as `x`
#'
#' @export
Screen <- function (x, ...){
  UseMethod("Screen")
}


#' Screen units based on data availability
#'
#' Screens units (rows) based on a data availability threshold and presence of zeros. Units can be optionally
#' "forced" to be included or excluded, making exceptions for the data availability threshold.
#'
#' The two main criteria of interest are `NA` values, and zeros. The summary table gives percentages of
#' `NA` values for each unit, across indicators, and percentage zero values (*as a percentage of non-`NA` values*).
#' Each unit is flagged as having low data or too many zeros based on thresholds.
#'
#' See also `vignette("screening")`.
#'
#' @param x A data frame
#' @param id_col Name of column of the data frame to be used as the identifier, e.g. normally this would be `uCode`
#' for indicator data sets used in coins. This must be specified if `Force` is specified.
#' @param unit_screen Specifies whether and how to screen units based on data availability or zero values.
#' * If set to `"byNA"`, screens units with data availability below `dat_thresh`
#' * If set to `"byzeros"`, screens units with non-zero values below `nonzero_thresh`
#' * If set to `"byNAandzeros"`, screens units based on either of the previous two criteria being true.
#' @param dat_thresh A data availability threshold (`>= 1` and `<= 0`) used for flagging low data and screening units if `unit_screen != "none"`. Default 0.66.
#' @param nonzero_thresh As `dat_thresh` but for non-zero values. Defaults to 0.05, i.e. it will flag any units with less than 5% non-zero values (equivalently more than 95% zero values).
#' @param Force A data frame with any additional units to force inclusion or exclusion. Required columns `uCode`
#' (unit code(s)) and `Include` (logical: `TRUE` to include and `FALSE` to exclude). Specifications here override
#' exclusion/inclusion based on data rules.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # example data
#' iData <- ASEM_iData[40:51, c("uCode", "Research", "Pat", "CultServ", "CultGood")]
#'
#' # screen to 75% data availability (by row)
#' l_scr <- Screen(iData, unit_screen = "byNA", dat_thresh = 0.75)
#'
#' # summary of screening
#' head(l_scr$DataSummary)
#'
#' @return Missing data stats and screened data as a list.
#'
#' @export

Screen.data.frame <- function(x, id_col = NULL, unit_screen, dat_thresh = NULL, nonzero_thresh = NULL,
                              Force = NULL, ...){


  # CHECKS ------------------------------------------------------------------

  stopifnot(is.data.frame(x))

  ##----- SET DEFAULTS -------##
  if(is.null(dat_thresh)){
    dat_thresh <- 2/3
  }
  if(is.null(nonzero_thresh)){
    nonzero_thresh <- 0.05
  }
  stopifnot(dat_thresh >= 0,
            dat_thresh <= 1,
            nonzero_thresh >= 0,
            nonzero_thresh <= 1)

  # GET DATA AVAIL ----------------------------------------------------------

  l <- get_data_avail(x)

  # FLAGS FOR EXCLUSION -----------------------------------------------------

  l <- cbind(l,
             LowData = l$Dat_Avail < dat_thresh,
             LowNonZero = l$Non_Zero < nonzero_thresh,
             LowDatOrZeroFlag = (l$Dat_Avail < dat_thresh) | (l$Non_Zero < nonzero_thresh))


  # Now add final column which says if unit is included or not, if asked for
  if (unit_screen == "byNA"){
    l$Included <- !l$LowData
  } else if (unit_screen == "byzeros"){
    l$Included <- !l$LowNonZero
  } else if (unit_screen == "byNAandzeros"){
    l$Included <- !l$LowDatOrZeroFlag
  } else {
    stop("unit_screen argument value not recognised...")
  }

  # FORCE INCLUSION/EXC -----------------------------------------------------
  # (this is optional)

  if (!is.null(Force)){ # if some countries to force include/exclude

    if(is.null(id_col)){
      stop("id_col must be specified if Force is specified")
    }

    # checks
    stopifnot(!is.null(Force$uCode),
              !is.null(Force$Include),
              is.character(Force$uCode),
              is.logical(Force$Include),
              is.character(id_col),
              id_col %in% names(x))
    if(any(Force$uCode %nin% x[[id_col]])){
      stop("One or more entries in Force$uCode not found in data frame.")
    }

    l$Included[l[[id_col]] %in% Force$uCode[Force$Include == TRUE]] <- TRUE
    l$Included[l[[id_col]] %in% Force$uCode[Force$Include == FALSE]] <- FALSE
  }


  # NEW DSET AND OUTPUT -----------------------------------------------------

  # create new data set which filters out the countries that didn't make the cut
  ScreenedData <- x[l$Included, ]
  # units that are removed
  if(!is.null(id_col)){
    RemovedUnits <- l[[id_col]][!(l$Included)]
  } else if (!is.null(l$uCode)) {
    RemovedUnits <- l$uCode[!(l$Included)]
  } else {
    RemovedUnits <- rownames(l)[!(l$Included)]
  }


  # output list
  list(ScreenedData = ScreenedData,
       DataSummary = l,
       RemovedUnits = RemovedUnits)
}



#' Screen units based on data availability
#'
#' Screens units based on a data availability threshold and presence of zeros. Units can be optionally
#' "forced" to be included or excluded, making exceptions for the data availability threshold.
#'
#' The two main criteria of interest are `NA` values, and zeros. The summary table gives percentages of
#' `NA` values for each unit, across indicators, and percentage zero values (*as a percentage of non-`NA` values*).
#' Each unit is flagged as having low data or too many zeros based on thresholds.
#'
#' See also `vignette("screening")`.
#'
#' @param x A coin
#' @param dset The data set to be checked/screened
#' @param unit_screen Specifies whether and how to screen units based on data availability or zero values.
#' * If set to `"byNA"`, screens units with data availability below `dat_thresh`
#' * If set to `"byzeros"`, screens units with non-zero values below `nonzero_thresh`
#' * If set to `"byNAandzeros"`, screens units based on either of the previous two criteria being true.
#' @param dat_thresh A data availability threshold (`>= 1` and `<= 0`) used for flagging low data and screening units if `unit_screen != "none"`. Default 0.66.
#' @param nonzero_thresh As `dat_thresh` but for non-zero values. Defaults to 0.05, i.e. it will flag any units with less than 5% non-zero values (equivalently more than 95% zero values).
#' @param Force A data frame with any additional countries to force inclusion or exclusion. Required columns `uCode`
#' (unit code(s)) and `Include` (logical: `TRUE` to include and `FALSE` to exclude). Specifications here override
#' exclusion/inclusion based on data rules.
#' @param out2 Where to output the results. If `"COIN"` (default for COIN input), appends to updated COIN,
#' otherwise if `"list"` outputs to data frame.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Screened"`.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # screen units from raw dset
#' coin <- Screen(coin, dset = "Raw", unit_screen = "byNA",
#'                dat_thresh = 0.85, write_to = "Filtered_85pc")
#'
#' # some details about the coin by calling its print method
#' coin
#'
#' @return An updated coin with data frames showing missing data in `.$Analysis`, and a new data set `.$Data$Screened`.
#' If `out2 = "list"` wraps missing data stats and screened data set into a list.
#'
#' @export

Screen.coin <- function(x, dset, unit_screen, dat_thresh = NULL, nonzero_thresh = NULL,
                              Force = NULL, out2 = "coin", write_to = NULL, ...){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")

  # GET DSET, CHECKS --------------------------------------------------------

  iData <- get_dset(coin, dset)

  # SCREEN DF ---------------------------------------------------------------

  l_out <- Screen.data.frame(iData, id_col = "uCode", unit_screen = unit_screen,
                                   dat_thresh = dat_thresh, nonzero_thresh = nonzero_thresh,
                                   Force = Force)

  # output list
  if(out2 == "list"){
    l_out
  } else {
    if(is.null(write_to)){
      write_to <- "Screened"
    }
    coin <- write_dset(coin, l_out$ScreenedData, dset = write_to)
    write2coin(coin, l_out[names(l_out) != "ScreenedData"], out2, "Analysis", write_to)
  }
}

#' Screen units based on data availability
#'
#' Screens units based on a data availability threshold and presence of zeros. Units can be optionally
#' "forced" to be included or excluded, making exceptions for the data availability threshold.
#'
#' The two main criteria of interest are `NA` values, and zeros. The summary table gives percentages of
#' `NA` values for each unit, across indicators, and percentage zero values (*as a percentage of non-`NA` values*).
#' Each unit is flagged as having low data or too many zeros based on thresholds.
#'
#' See also `vignette("screening")`.
#'
#' @param x A purse object
#' @param dset The data set to be checked/screened
#' @param unit_screen Specifies whether and how to screen units based on data availability or zero values.
#' * If set to `"byNA"`, screens units with data availability below `dat_thresh`
#' * If set to `"byzeros"`, screens units with non-zero values below `nonzero_thresh`
#' * If set to `"byNAandzeros"`, screens units based on either of the previous two criteria being true.
#' @param dat_thresh A data availability threshold (`>= 1` and `<= 0`) used for flagging low data and screening units if `unit_screen != "none"`. Default 0.66.
#' @param nonzero_thresh As `dat_thresh` but for non-zero values. Defaults to 0.05, i.e. it will flag any units with less than 5% non-zero values (equivalently more than 95% zero values).
#' @param Force A data frame with any additional countries to force inclusion or exclusion. Required columns `uCode`
#' (unit code(s)) and `Include` (logical: `TRUE` to include and `FALSE` to exclude). Specifications here override
#' exclusion/inclusion based on data rules.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Screened"`.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # see vignette("screening") for an example.
#'
#' @return An updated purse with coins screened and updated.
#'
#' @export
Screen.purse <- function(x, dset, unit_screen, dat_thresh = NULL, nonzero_thresh = NULL,
                         Force = NULL, write_to = NULL, ...){

  # input check
  check_purse(x)

  # apply unit screening to each coin
  x$coin <- lapply(x$coin, function(coin){
    Screen.coin(coin, dset = dset, unit_screen = unit_screen,
                dat_thresh = dat_thresh, nonzero_thresh = nonzero_thresh,
                Force = Force, out2 = "coin", write_to = write_to)
  })
  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}


#' Get data availability of units
#'
#' Generic function for getting the data availability of each unit (row).
#'
#' See method documentation:
#'
#' * [get_data_avail.data.frame()]
#' * [get_data_avail.coin()]
#'
#' See also vignettes: `vignette("analysis")` and `vignette("imputation")`.
#'
#' @param x Either a coin or a data frame
#' @param ... Arguments passed to other methods
#'
#' @export
get_data_avail <- function(x, ...){
  UseMethod("get_data_avail")
}


#' Get data availability of units
#'
#' Returns a list of data frames: the data availability of each unit (row) in a given data set, as well as percentage of zeros.
#' A second data frame gives data availability by aggregation (indicator) groups.
#'
#' This function ignores any non-numeric columns, and returns a data availability table of numeric columns with non-numeric columns
#' appended at the beginning.
#'
#' See also vignettes: `vignette("analysis")` and `vignette("imputation")`.
#'
#' @param x A coin
#' @param dset String indicating name of data set in `.$Data`.
#' @param out2 Either `"coin"` to output an updated coin or `"list"` to output a list.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated coin with data availability tables written in `.$Analysis[[dset]]`, or a
#' list of data availability tables.
#' @export
#'
#' @examples
#' # build example coin
#' coin <-  build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get data availability of Raw dset
#' l_dat <- get_data_avail(coin, dset = "Raw", out2 = "list")
#' head(l_dat$Summary, 5)
#'
get_data_avail.coin <- function(x, dset, out2 = "coin", ...){

  # PREP --------------------------------------------------------------------

  iData <- get_dset(x, dset)
  lin <- x$Meta$Lineage

  # DAT AVAIL AND TABLE -----------------------------------------------------

  # call df method
  dat_avail <- get_data_avail(iData)

  # generic function to check frac NAs rowwise
  frc_avail <- function(X){
    1 - rowMeans(is.na(X))
  }

  # indicator-level group data avail function
  group_avail <- function(grp, lev){
    # get cols of indicators inside group
    grp_codes <- unique(lin[[1]][lin[[lev]] == grp])

    # get data avail
    frc_avail( iData[grp_codes])
  }

  # get all data availability, for all groups in all levels
  # note, this is indicator-level availability
  for(lev in 2: ncol(lin)){
    lev_codes <- unique(lin[[lev]])
    df_lev <- sapply(lev_codes, group_avail, lev)
    if(lev == 2){
      dat_avail_group <- data.frame(uCode = iData$uCode, df_lev)
    } else {
      dat_avail_group <- cbind(dat_avail_group, df_lev)
    }
  }

  # OUTPUT ------------------------------------------------------------------

  l_out <- list(Summary = dat_avail,
       ByGroup = dat_avail_group)

  write2coin(x, l_out, out2, "Analysis", dset, "DatAvail")

}


#' Get data availability of units
#'
#' Returns a data frame of the data availability of each unit (row), as well as percentage of zeros. This
#' function ignores any non-numeric columns, and returns a data availability table with non-numeric columns
#' appended at the beginning.
#'
#' See also vignettes: `vignette("analysis")` and `vignette("imputation")`.
#'
#' @param x A data frame
#' @param ... arguments passed to or from other methods.
#'
#' @return A data frame of data availability statistics for each column of `x`.
#' @export
#'
#' @examples
#' # data availability of "airquality" data set
#' get_data_avail(airquality)
#'
get_data_avail.data.frame <- function(x, ...){

  # PREP --------------------------------------------------------------------

  xsplit <- split_by_numeric(x)
  x_ <- xsplit$numeric


  # DAT AVAIL AND TABLE -----------------------------------------------------

  nabyrow <- rowSums(is.na(x_)) # number of missing data by row
  zerobyrow <- rowSums(x_ == 0, na.rm = TRUE) # number of zeros for each row
  nazerobyrow <- nabyrow + zerobyrow # number of zeros or NAs for each row
  Prc_avail = 1 - nabyrow/ncol(x_) # the percentage of data available
  Prc_nonzero = 1 - zerobyrow/(ncol(x_) - nabyrow) # the percentage of non zeros

  data.frame(xsplit$not_numeric,
             N_missing = nabyrow,
             N_zero = zerobyrow,
             N_miss_or_zero = nazerobyrow,
             Dat_Avail = Prc_avail,
             Non_Zero = Prc_nonzero
  )

}
