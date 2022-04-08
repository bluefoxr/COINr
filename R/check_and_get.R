# FUNCTIONS AND METHODS FOR CHECKING AND GETTING DATA FROM COINS AND PURSES

#' Checks a purse
#'
#' Makes sure a purse has the expected format
#'
#' @param x An object to check
#'
#' @examples
#' #
#'
#' @return Error messages otherwise if successful, nothing.
#'
#' @export
check_purse <- function(x){

  if(!is.purse(x)){
    stop("Object is not tagged as an S3 purse class")
  }
  if(!is.data.frame(x)){
    stop("Object is not a data frame, which is required for a purse class")
  }
  if(is.null(x$Time)){
    stop("No 'Time' column found in purse - this is required.")
  }
  if(is.null(x$coin)){
    stop("No coin column found in purse - this is required.")
  }

  not_coins <- !sapply(x$coin, is.coin)
  if(any(not_coins)){
    stop("One or more entries in .$coin is not a coin class")
  }

}


#' Stop if object is NOT coin class
#'
#' This helper function is used inside other functions.
#'
#' @param x An object to be checked.
#'
#' @return An error if input is not coin class.
check_coin_input <- function(x){
  if(!is.coin(x)){
    stop("Input is not recognised as a coin class object.")
  }
}

#' Check for named data set
#'
#' A helper function to check if a named data set is present. If not, will cause an informative error.
#'
#' @param x A purse class object
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#'
#' @examples
#' #
#'
#' @export
#'
#' @return Error message if `dset` not found.
check_dset.purse <- function(x, dset){

  stopifnot(is.purse(x),
            is.character(dset),
            length(dset)==1)

  for(tt in x$Time){
    coin <- x$coin[[which(x$Time == tt)]]
    if(is.null(coin$Data[[dset]])){
      stop("Required data set '", dset, "' not found in coin at Time = ", tt)
    }
  }
}


#' Check for named data set
#'
#' A helper function to check if a named data set is present. If not, will cause an informative error.
#'
#' @param x A coin class object
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#'
#' @examples
#' #
#' @export
#'
#' @return Error message if `dset` not found.
check_dset.coin <- function(x, dset){

  stopifnot(is.coin(x),
            is.character(dset),
            length(dset)==1)

  if(is.null(x$Data[[dset]]) & (dset != "uMeta") ){
    stop("Required data set '", dset, "' not found in coin object.")
  }
}


#' Check for named data set
#'
#' A helper function to check if a named data set is present. If not, will cause an informative error.
#'
#' @param x A coin or purse
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#' @param ... Other arguments to be passed to methods.
#'
#' @examples
#' #
#'
#' @return Error message if `dset` is not found.
#'
#' @export
check_dset <- function(x, dset, ...){
  UseMethod("check_dset")
}


#' Gets a named data set and performs checks
#'
#' A helper function to retrieve a named data set from a purse object. Retrieves the specified data set
#' from each coin in the purse and joins them together in a single data frame using [rbind()], indexed
#' with a `Time` column.
#'
#' @param x A purse class object
#' @param dset A character string corresponding to a named data set within each coin `coin$Data`. E.g. `Raw`.
#' @param Time Optional time index to extract from a subset of the coins present in the purse. Should be a
#' vector containing one or more entries in `x$Time` or `NULL` to return all (default).
#' @param also_get A character vector specifying any columns to attach to the data set that are *not*
#' indicators or aggregates. These will be e.g. `uName`, groups, denominators or columns labelled as "Other"
#' in `iMeta`. These columns are stored in `.$Meta$Unit` to avoid repetition. Set `also_get = "all"` to
#' attach all columns, or set `also_get = "none"` to return only numeric columns, i.e. no `uCode` column.
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data.
#'
#' @export
get_dset.purse <- function(x, dset, Time = NULL, also_get = NULL){

  # check specified dset exists
  check_dset(x, dset)

  if(!is.null(Time)){
    if(any(Time %nin% x$Time)){
      stop("One or more entries in Time not found in the Time column of the purse.")
    }
    coins <- x$coin[x$Time %in% Time]
  } else {
    coins <- x$coin
  }

  # extract data sets in one df
  iDatas <- lapply(coins, function(coin){
    iData <- get_dset(coin, dset = dset, also_get = also_get)
    iData <- cbind(Time = coin$Meta$Unit$Time[[1]], iData)
  })
  Reduce(rbind, iDatas)
}

#' Gets a named data set and performs checks
#'
#' A helper function to retrieve a named data set from the coin object. Also performs input checks at the
#' same time.
#'
#' If `also_get` is not specified, this will return the indicator columns with the `uCode` identifiers
#' in the first column. Optionally, `also_get` can be specified to attach other metadata columns, or
#' to only return the numeric (indicator) columns with no identifiers. This latter option might be useful
#' for e.g. examining correlations.
#'
#' @param x A coin class object
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#' @param also_get A character vector specifying any columns to attach to the data set that are *not*
#' indicators or aggregates. These will be e.g. `uName`, groups, denominators or columns labelled as "Other"
#' in `iMeta`. These columns are stored in `.$Meta$Unit` to avoid repetition. Set `also_get = "all"` to
#' attach all columns, or set `also_get = "none"` to return only numeric columns, i.e. no `uCode` column.
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data.
#'
#' @export
get_dset.coin <- function(x, dset, also_get = NULL){

  # check specified dset exists
  check_dset(x, dset)

  # get dset
  if(dset != "uMeta"){

    iData <- x$Data[[dset]]

    if(!is.null(also_get)){

      if(also_get[1] == "none"){
        iData <- iData[names(iData) != "uCode"]
      } else {

        uMeta <- x$Meta$Unit

        if(is.null(uMeta)){
          stop("Unit metadata not found in coin.")
        }

        if(length(also_get) == 1){
          if(also_get == "all"){
            uMeta_codes <- colnames(uMeta)
          } else {
            uMeta_codes <- also_get
          }
        } else {
          uMeta_codes <- also_get
        }

        # check entries in also_get exist
        if(any(uMeta_codes %nin% colnames(uMeta))){
          stop("Entries in also_get not recognised - see function help.")
        }

        uMeta <- uMeta[union("uCode", uMeta_codes)]

        iData <- merge(uMeta, iData, by = "uCode", all.x = FALSE, all.y = TRUE)

      }
    }

  } else {

    # get uMeta
    iData <- coin$Meta$Unit
    if(is.null(iData)){
      stop("Unit metadata (uMeta) not found in coin!")
    }

  }


  iData
}

#' Gets a named data set and performs checks
#'
#' A helper function to retrieve a named data set from coin or purse objects.
#'
#' @param x A coin or purse
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data, indexed also by time if input is a purse.
#'
#' @export
get_dset <- function(x, dset, ...){
  UseMethod("get_dset")
}


#' Get subsets of indicator data
#'
#' A flexible function for retrieving data from a coin, from a specified data set. Subsets of data can
#' be returned based on selection of columns, using the `iCodes` and `Level` arguments, and by filtering
#' rowwise using the `uCodes` and `use_group` arguments. The `also_get` argument also allows unit metadata
#' columns to be attached, such as names, groups, and denominators.
#'
#' The `iCodes` argument can be used to directly select named indicators, i.e. setting `iCodes = c("a", "b")`
#' will select indicators "a" and "b", attaching any extra columns specified by `also_get`. However,
#' using this in conjunction with the `Level` argument returns named groups of indicators. For example,
#' setting `iCodes = "Group1"` (for e.g. an aggregation group in Level 2) and `Level = 1` will return
#' all indicators in Level 1, belonging to "Group1".
#'
#' Rows can also be subsetted. The `uCodes` argument can be used to select specified units in the same
#' way as `iCodes`. Additionally, the `use_group` argument filters to specified groups. If `uCodes` is
#' specified, and `use_group` refers to a named group column, then it will return all units in the
#' groups that the `uCodes` belong to. This is useful for putting a unit into context with its peers
#' based on some grouping variable.
#'
#' Note that if you want to retreive a whole data set (with no column/row subsetting), use the
#' [get_dset()] function which should be slightly faster.
#'
#' @param x A coin class object
#' @param dset Name of a data set found in `.$Data`.
#' @param iCodes Optional indicator codes to retrieve. If `NULL` (default), returns all iCodes found in
#' the selected data set. Can also refer to indicator groups. See details.
#' @param Level Optionally, the level in the hierarchy to extract data from. See details.
#' @param uCodes Optional unit codes to filter rows of the resulting data set. Can also be used in conjunction
#' with groups. See details.
#' @param use_group Optional group to filter rows of the data set. Specified as `list(Group_Var = Group)`,
#' where `Group_Var` is a Group_ column that must be present in the selected data set, and `Group` is a specified group
#' inside that grouping variable. This filters the selected data to only include rows from the specified group. Can
#' also be used in conjunction with `uCodes` -- see details.
#' @param also_get Optional meta data columns to attach to the data set. See [get_dset()].
#'
#' @return
#' @export
get_data.coin <- function(x, dset, iCodes = NULL, Level = NULL, uCodes = NULL,
                     use_group = NULL, also_get = NULL){

  # CHECKS ------------------------------------------------------------------

  coin <- x

  check_coin_input(coin)

  # get iMeta and maxlev
  iMeta <- coin$Meta$Ind
  maxlev <- coin$Meta$maxlev

  # check Level
  if(!is.null(Level)){
    stopifnot(is.numeric(Level),
              length(Level) == 1)
    if(Level %nin% 1:maxlev){
      stop("Level is not in 1:(max level).")
    }
    if(dset == "uMeta"){
      # if it's uMeta we don't worry about levels
      Level <- NULL
    }
  }

  # check groups and get names
  if(!is.null(use_group)){

    stopifnot(length(use_group)==1)

    if(is.list(use_group)){
      groupcol <- names(use_group)
      groupsel <- use_group[[1]]
    } else if (is.character(use_group)){
      groupcol <- use_group
      groupsel <- NULL
    }
  } else {
    groupcol <- NULL
    groupsel <- NULL
  }

  # GET DSET ----------------------------------------------------------------

  # if we have to filter by group, we also have to get group cols
  # we also probably need uCode in any case (can be deleted later)
  remove_meta <- FALSE
  if(!is.null(also_get)){

    if(is.null(use_group)){
      # we don't need any group cols, take also_get as is
      # if none, we still probably need uCode, so set NULL
      if(also_get[1] == "none"){
        also_get <- NULL
        remove_meta <- TRUE
      }
    } else {
      if(also_get[1] == "none"){
        also_get <- c("uCode", groupcol)
        remove_meta <- TRUE
      } else {
        also_get <- unique(c(also_get, groupcol))
      }
    }
  } else {
    also_get <- groupcol
  }

  iData <- get_dset(coin, dset = dset, also_get = also_get)

  # make sure group can be found in group col, if specified
  if(!is.null(groupsel)){
    if(groupsel %nin% iData[[groupcol]]){
      stop("Selected group not found in specified group column.")
    }
  }

  # col names that are NOT indicators
  not_iCodes <- names(iData)[names(iData) %in% names(coin$Meta$Unit)]

  # COLUMNS -----------------------------------------------------------------

  # We have iCodes and Level to think about here

  if(!is.null(iCodes)){

    # first check iCodes are findable
    if(any(iCodes %nin% iMeta$iCode)){
      stop("One or more iCodes not found in iMeta.")
    }

    # check which level iCodes are from
    Lev_iCodes <- unique(iMeta$Level[iMeta$iCode %in% iCodes])
    # check not from different levels
    if(length(Lev_iCodes) != 1){
      stop("iCodes are from different Levels - this is not allowed.")
    }

    if(is.null(Level)){

      # no Level specified: take iCodes as given
      cols <- iCodes

    } else {

      # get lineage
      lin <- coin$Meta$Lineage
      # get cols to select
      cols <- unique(lin[[Level]][lin[[Lev_iCodes]] %in% iCodes])

    }

    # select columns
    if(any(cols %nin% names(iData))){
      stop("Selected iCodes not found in data set. If Level > 1 you need to target an aggregated data set.")
    }
    if(dset == "uMeta"){
      iData1 <- iData[unique(c("uCode", groupcol, cols, also_get))]
    } else {
      iData1 <- iData[c(not_iCodes, cols)]
    }


  } else if (!is.null(Level)) {

    # iCodes not specified, but Level specified
    # This means we take everything from specified level, if available
    cols <- iMeta$iCode[iMeta$Level == Level]
    cols <- cols[!is.na(cols)]

    # select columns
    if(any(cols %nin% names(iData))){
      stop("Selected iCodes not found in data set. If Level > 1 you need to target an aggregated data set.")
    }
    iData1 <- iData[c(not_iCodes, cols)]

  } else {

    # no iCodes or Level specified
    # no column selection
    iData1 <- iData
  }

  # ROWS --------------------------------------------------------------------

  if(!is.null(uCodes)){

    # check uCodes can be found
    if(any(uCodes %nin% iData$uCode)){
      stop("One or more uCodes not found in the selected data set.")
    }

    if(!is.null(use_group)){
      # We have uCodes AND group specification

      # filter to group(s) containing units
      if(is.null(groupsel)){
        # groups containing units
        uGroups <- unique(iData1[[groupcol]][iData1$uCode %in% uCodes])
        # filter to these groups
        iData2 <- iData1[iData1[[groupcol]] %in% uGroups, ]
      } else {
        # if we have a specified group within a column, AND uCodes, we give preference to uCodes
        iData2 <- iData1[iData1$uCode %in% uCodes, ]
      }

    } else {
      # no groups specified -
      # filter to selected units
      iData2 <- iData1[iData1$uCode %in% uCodes, ]
    }
  } else if (!is.null(use_group)){

    # groups specified, but no uCodes

    # select a whole group
    if(is.null(groupsel)){
      # silly case where only a col is specified, but no actual group. Hence no filtering
      iData2 <- iData1
    } else {
      # proper group selection
      iData2 <- iData1[iData1[[groupcol]] == groupsel, ]
    }

  } else {

    # no row filtering
    iData2 <- iData1

  }

  # OUTPUT ------------------------------------------------------------------

  if(remove_meta){
    iData2 <- iData2[names(iData2) %nin% not_iCodes]
  }

  iData2

}


#' Get subsets of indicator data
#'
#' Purse description.
#'
#' @param x A purse class object
#' @param dset Name of a data set found in `.$Data`.
#' @param iCodes Optional indicator codes to retrieve. If `NULL` (default), returns all iCodes found in
#' the selected data set. Can also refer to indicator groups. See details.
#' @param Level Optionally, the level in the hierarchy to extract data from. See details.
#' @param uCodes Optional unit codes to filter rows of the resulting data set. Can also be used in conjunction
#' with groups. See details.
#' @param use_group Optional group to filter rows of the data set. Specified as `list(Group_Var = Group)`,
#' where `Group_Var` is a Group_ column that must be present in the selected data set, and `Group` is a specified group
#' inside that grouping variable. This filters the selected data to only include rows from the specified group. Can
#' also be used in conjunction with `uCodes` -- see details.
#' @param Time Optional time index to extract from a subset of the coins present in the purse. Should be a
#' vector containing one or more entries in `x$Time` or `NULL` to return all (default).
#' @param also_get Optional meta data columns to attach to the data set. See [get_dset()].
#'
#' @return
#' @export
get_data.purse <- function(x, dset, iCodes = NULL, Level = NULL, uCodes = NULL,
                     use_group = NULL, Time = NULL, also_get = NULL){

  # check specified dset exists
  check_dset(x, dset)

  if(!is.null(Time)){
    if(any(Time %nin% x$Time)){
      stop("One or more entries in Time not found in the Time column of the purse.")
    }
    coins <- x$coin[x$Time %in% Time]
  } else {
    coins <- x$coin
  }

  # extract data sets in one df
  iDatas <- lapply(coins, function(coin){
    iData <- get_data(coin, dset = dset, iCodes = iCodes, Level = Level,
                      uCodes = uCodes, use_group = use_group, also_get = also_get)
    iData <- cbind(Time = coin$Meta$Unit$Time[[1]], iData)
  })
  Reduce(rbind, iDatas)
}


#' Get subsets of indicator data
#'
#' A helper function to retrieve a named data set from coin or purse objects.
#'
#' @param x A coin or purse
#' @param ... Arguments passed to methods
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data, indexed also by time if input is a purse.
#'
#' @export
get_data <- function(x, ...){
  UseMethod("get_data")
}


#' Exract things from iData
#'
#' A helper function to separate indicator cols from metadata columns in iMeta.
#'
#' @param coin A coin
#' @param iData An iData format data frame
#' @param GET What to get
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data, indexed also by time if input is a purse.
extract_iData <- function(coin, iData, GET){

  # indicator cols
  iCodes <- names(iData)[names(iData) %nin% names(coin$Meta$Unit)]

  if(GET == "iCodes"){
    iCodes
  } else if (GET == "iData_"){
    iData[iCodes]
  } else if (GET == "meta"){
    iData[colnames(iData) %nin% iCodes]
  } else if (GET == "mCodes"){
    setdiff(colnames(iData), iCodes)
  }

}


#' Get names from codes
#'
#' Given either uCodes or iCodes, returns uNames or iNames
#'
#' @param coin A coin
#' @param uCodes Some uCodes
#' @param iCodes Some iCodes
#'
#' @examples
#' #
#'
#' @return Vector of names
get_names <- function(coin, uCodes = NULL, iCodes = NULL){

  if(!is.null(uCodes) && !is.null(iCodes)){
    stop("Either uCodes or iCodes, not both.")
  }

  if(!is.null(uCodes)){

    uNames <- coin$Meta$Unit$uName
    if(is.null(uNames)){stop("uNames not found")}
    if(any(uCodes %nin% coin$Meta$Unit$uCode)){
      stop("One or more uCodes not found in .$Meta$Unit$uCode")
    }
    uNames[match(uCodes, coin$Meta$Unit$uCode)]

  } else if (!is.null(iCodes)){

    iNames <- coin$Meta$Ind$iName
    if(is.null(iNames)){stop("iNames not found")}
    if(any(iCodes %nin% coin$Meta$Ind$iCode)){
      stop("One or more iCodes not found in .$Meta$Ind$iCode")
    }
    iNames[match(iCodes, coin$Meta$Ind$iCode)]

  }

}


#' Get units of indicators
#'
#' Given iCodes, returns corresponding units if available.
#'
#' @param coin A coin
#' @param iCodes Some iCodes
#'
#' @examples
#' #
#'
#' @return Vector of names
get_units <- function(coin, iCodes = NULL){

  iUnits <- coin$Meta$Ind$Unit
  if(is.null(iUnits)){
    return(NULL)
  }
  if(any(iCodes %nin% coin$Meta$Ind$iCode)){
    stop("One or more iCodes not found in .$Meta$Ind$iCode")
  }
  iUnits[match(iCodes, coin$Meta$Ind$iCode)]
}
