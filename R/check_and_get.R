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

  if(is.null(x$Data[[dset]])){
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
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data.
#'
#' @export
get_dset.purse <- function(x, dset, Time = NULL){

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
    iData <- get_dset(coin, dset)
    iData <- cbind(Time = coin$Meta$Unit$Time[[1]], iData)
  })
  Reduce(rbind, iDatas)
}

#' Gets a named data set and performs checks
#'
#' A helper function to retrieve a named data set from the coin object. Also performs input checks at the
#' same time.
#'
#' @param x A coin class object
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data.
#'
#' @export
get_dset.coin <- function(x, dset){

  # check specified dset exists
  check_dset(x, dset)
  # get dset
  x$Data[[dset]]
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

