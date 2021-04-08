#' Get subsets of indicator data
#'
#' This function does a number of things that are useful for many COINr functions and operations.
#' First, it checks to see what kind of input object is input. Then, it selects the indicator data
#' according to the specs supplied.
#'
#' For example, specifying dset = "Raw" and icodes = c("Ind1", "Ind5"), it will the indicator columns
#' named "Ind1" and "Ind5" (if they exist), in the format described below. icodes can be indicators
#' or aggregation groups, and can call multiple groups.
#'
#' You can also specify which aggregation level to target, using the "aglev" argument. See examples
#' below, and the COINr vignette.
#'
#' @param obj An input object. The function can handle either the COIN object, or a data frame.
#' The data frame should have each column as an indicator, and optional columns "UnitCode" and "UnitName" which
#' specify the code (or name) of each unit. Any columns except these latter two will be treated as indicators. Any other type of object will return an error.
#' @param dset If input is a COIN object, this specifies which data set in .$Data to use.
#' @param icodes An optional character vector of indicator codes to subset the indicator data. Usefully, can also refer to
#' an aggregation group name, and data will be subsetted accordingly. NOTE does not work with multiple aggregate group names.
#' @param aglev The aggregation level to take indicator data from. Integer from 1 (indicator level)
#' to N (top aggregation level, typically the index).
#' @param justnumeric Logical: if TRUE, removes any non-numeric columns from ind_data_only. Otherwise keeps all except those
#'
#' @importFrom magrittr extract
#' @importFrom dplyr select starts_with ends_with
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'
#' # Get data from indicators "Ind1" and "Ind5", from the "Raw" data set
#' out <- getIn(obj, dset = "Raw", icodes = c("Ind1", "Ind5"))
#' }
#'
#' @return A list with the following entries:
#' * .$IndCodes  The indicator codes
#' * .$IndNames  The indicator names (if a COIN object is input)
#' * .$ind_data A data frame of indicator data, according to the input specifications, including any unit codes, names and groups
#' * .$ind_data_only A data frame, as above, but without unit codes, names, groups
#' * .$UnitCodes Unit codes of selected data set
#' * .$otype Object type (a string: either "COINobj" or "df")
#'
#' @export

getIn <- function(obj, dset = "Raw", icodes = NULL, aglev = NULL, justnumeric = TRUE){

  # Check to see what kind of input we have.
  if ("COIN object" %in% class(obj)){ # COIN obj

    otype <- "COINobj"

    # The full table of indicator data
    # If looking for denominators, it is in a separate place
    if (dset=="Denominators"){
      ind_data = tryCatch({
        obj$Input$Denominators
      }, error = function(e) {
        stop("Denominators not found.")
      })
    } else {
      # all other dsets are here in the .$Data folder
      ind_data = tryCatch({
        obj$Data[[dset]]
      }, error = function(e) {
        stop("dset name not recognised...")
      })
    }
    if(is.null(ind_data)){
      stop("dset not found. Did you make a typo or forget to create it first?")
    }

    # get unit codes. Have to do this because if the units have been screened, then it may
    # not be the same set as when the data was input.
    UnitCodes <- ind_data$UnitCode

    # If no indicator names specified, return all
    if(is.null(icodes)){
      # get indicator names, i.e. columns excluding groups, denominators, names etc.
      icodes <- ind_data %>% dplyr::select(!dplyr::starts_with(
        c("UnitCode", "UnitName", "Year", "Group_", "x_")) ) %>% colnames()
    }

    if (is.null(aglev) | (dset=="Denominators")){ # take icodes as it is given
      IndCodes <- icodes
    } else {

      # get index structure from Indicator metadata
      aggcols <- dplyr::select(obj$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg")) %>% as.data.frame()
      # filter any rows containing the specified string(s), pick column corresponding to agg level
      # Might look strange, but checks whether each row has any of the specified codes in.
      # Then, filters to those rows, and selects the column corresponding to the ag level.
      IndCodes <- aggcols[rowSums(sapply(aggcols, "%in%", icodes))>0,aglev] %>% unique()

    }

    # get indicator names
    if (dset!="Denominators"){
      IndNames <- obj$Parameters$Code2Name$AggName[
        obj$Parameters$Code2Name$AggCode %in% IndCodes]
    } else {
      # if data set is denominators, then just use codes (may update this at some point)
      IndNames <- IndCodes
    }

    # select indicator data columns

    ind_data <- ind_data %>% dplyr::select(dplyr::starts_with(
      c("UnitCode", "UnitName", "Year", "Group_", "x_", IndCodes)) )

  } else if (is.data.frame(obj)){ # Data frame

    otype <- "df"

    ind_data <- obj

    if (is.null(icodes)){ # no ind codes given
      if (exists("UnitCode",ind_data)){
        # If there are unit codes, record them and assume all other cols are indicator names
        IndCodes <- obj[colnames(obj) != "UnitCode"] %>% colnames()
        UnitCodes <- obj$UnitCode
      } else {
        # All cols are indicators. No names supplied.
        IndCodes <- colnames(obj)
        UnitCodes <- NA
      }
    } else { # indicator names are supplied
      IndCodes <- icodes
      if (exists("UnitCode",ind_data)){
        UnitCodes <- obj$UnitCode
      } else {
        UnitCodes <- NA
      }
    }

    if (exists("UnitName",ind_data)){
      IndNames <- ind_data$UnitName
      IndCodes <- IndCodes[IndCodes != "UnitName"]
    } else {
      IndNames <- IndCodes # we don't know names, so use codes
    }


  } else { # Not COIN obj OR df
    stop("Input should either be COIN object or data frame.")
  }

  # Check if indicator codes actually found in data set
  # This can happen e.g. if we call the Index from the "Raw" data set (not yet aggregated).
  if (any(IndCodes %in% colnames(ind_data))){
    ind_data_only = ind_data[IndCodes]
  } else {
    ind_data_only = ind_data[NULL]
    warning("Indicator codes not found in selected level or data set.")
  }

  # finally, remove any non-numeric columns in ind_data_only
  if(justnumeric){
    numcols <- unlist(lapply(ind_data_only, is.numeric))
    ind_data_only <- ind_data_only[numcols]
    IndCodes <- IndCodes[numcols]
    if (length(numcols)>ncol(ind_data_only)){
      warning(paste0("Removed ",length(numcols)-ncol(ind_data_only), " non-numeric column(s)."))
    }
  }

  out <- list(IndCodes = IndCodes,
              IndNames = IndNames,
              ind_data = ind_data,
              ind_data_only = ind_data_only,
              UnitCodes = UnitCodes,
              otype = otype # the object type
  )

  return(out)
}


#' Round down a data frame
#'
#' Tiny function just to round down a data frame for display in a table.
#'
#'
#'
#' @param df A data frame to input
#' @param decimals The number of decimal places to round to (default 2)
#'
#' @examples
#' \dontrun{ df <- roundDF( as.data.frame(matrix(runif(20),10,2)) )}
#'
#' @return A data frame, with any numeric columns rounded to the specified amount
#'
#' @export

roundDF <- function(df, decimals = 2){
  df <- lapply(df, function(y) if(is.numeric(y)) round(y, decimals) else y) %>%
    data.frame()
  rownames(df) <- NULL
  df
}


