#' Get subsets of indicator data
#'
#' This function does a number of things that are useful for many COINr functions and operations.
#' First, it checks to see what kind of input object is input. Then, it selects the indicator data
#' according to the specs supplied.
#'
#' For example, specifying dset = "Raw" and inames = c("Ind1", "Ind5"), it will the indicator columns
#' named "Ind1" and "Ind5" (if they exist), in the format described below. inames can be indicators
#' or aggregation groups, and can call multiple groups.
#'
#' You can also specify which aggregation level to target, using the "aglev" argument. See examples
#' below, and the COINr vignette.
#'
#' @param obj An input object. The function can handle either the COIN object, or a data frame.
#' The data frame should have each column as an indicator, and an optional column "UnitCode" which
#' specifies the code (or name) of each unit. Any other type of object will return an error.
#' @param dset If input is a COIN object, this specifies which data set in .$Data to use.
#' @param inames An optional character vector of indicator codes to subset the indicator data. Usefully, can also refer to
#' an aggregation group name, and data will be subsetted accordingly. NOTE does not work with multiple aggregate group names.
#' @param aglev The aggregation level to take indicator data from. Integer from 1 (indicator level)
#' to N (top aggregation level, typically the index).
#'
#' @importFrom magrittr extract
#' @importFrom dplyr select starts_with ends_with
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'
#' # Get data from indicators "Ind1" and "Ind5", from the "Raw" data set
#' out <- getIn(COINobj, dset = "Raw", inames = c("Ind1", "Ind5"))
#'
#' # get data from "Research" and "Education" dimensions, calling agggregation level 2.
#' out <- coin_aux_objcheck(COINobj, dset = "Aggregated",
#'                          inames = c("Research", "Education"), aglev = 2)
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

getIn <- function(obj, dset = "Raw", inames = NULL, aglev = NULL){

  # Check to see what kind of input we have.
  if ("COIN object" %in% class(obj)){ # COIN obj

    otype <- "COINobj"

    # The full table of indicator data
    ind_data = tryCatch({
      obj$Data[[dset]]
    }, error = function(e) {
      stop("dset name not recognised...")
    })

    # get unit codes. Have to do this because if the units have been screened, then it may
    # not be the same set as when the data was input.
    UnitCodes <- ind_data$UnitCode

    # If no indicator names specified, return all
    if(is.null(inames)){
      # get indicator names, i.e. columns excluding groups, denominators, names etc.
      inames <- ind_data %>% dplyr::select(!dplyr::starts_with(
        c("UnitCode", "UnitName", "Year", "Group_","Den_")) ) %>% colnames()
    }

    if (is.null(aglev)){ # take inames as it is given
      IndCodes <- inames
    } else { # take inames as reference to a group or groups

      # get index structure from Indicator metadata
      aggcols <- dplyr::select(obj$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg")) %>% as.data.frame()
      # filter any rows containing the specified string(s), pick column corresponding to agg level
      # Might look strange, but checks whether each row has any of the specified codes in.
      # Then, filters to those rows, and selects the column corresponding to the ag level.
      IndCodes <- aggcols[rowSums(sapply(aggcols, "%in%", inames))>0,aglev] %>% unique()

    }

    # get indicator names
    IndNames <- obj$Parameters$Code2Name$AggName[
      obj$Parameters$Code2Name$AggCode %in% IndCodes]

    # select indicator data columns

    ind_data <- ind_data %>% dplyr::select(dplyr::starts_with(
      c("UnitCode", "UnitName", "Year", "Group_","Den_", IndCodes)) )

  } else if (is.data.frame(obj)){ # Data frame

    otype <- "df"

    ind_data <- obj

    if (is.null(inames)){ # no ind names given
      if (exists("UnitCode",ind_data)){
        # If there are unit codes, record them and assume all other cols are indicator names
        IndCodes <- obj[colnames(obj) != "UnitCode"] %>% colnames()
        UnitCodes <- obj$UnitCode
      } else {
        # All cols are indicators. No names supplied.
        IndCodes <- colnames(obj)
        IndNames <- IndCodes # we don't know names, so use codes
        UnitCodes <- NA
      }
    } else { # indicator names are supplied
      IndCodes <- inames
      IndNames <- IndCodes # we don't know names, so use codes
      if (exists("UnitCode",ind_data)){
        UnitCodes <- obj$UnitCode
      } else {
        UnitCodes <- NA
      }
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

  out <- list(IndCodes = IndCodes,
              IndNames = IndNames,
              ind_data = ind_data,
              ind_data_only = ind_data_only,
              UnitCodes = UnitCodes,
              otype = otype # the object type
  )

  return(out)
}
