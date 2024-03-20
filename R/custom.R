#' Custom operation
#'
#' Custom operation on a purse. This is an experimental new feature.
#'
#' In this function, the data set named `dset` is extracted from the coin using
#' `get_dset(purse, dset)`. It is passed to the function `f_cust`, which is required
#' to return an equivalent but modified data frame, which is then written as a new
#' data set with name `write_to`. This is intended to allow arbitrary operations
#' on coin data sets while staying within the COINr framework, which means that if
#' `Regen()` is used, these operations will be re-run, allowing them to be included
#' in things like sensitivity analysis.
#'
#' The format of `f_cust` is important. It must be a function whose first argument
#' is called `x`: this will be the argument that the data is passed to. The data will
#' be in the same format as extracted via `get_dset(purse, dset)`, which means it will
#' have `uCode` and `Time` columns. `f_cust` can have other arguments which are passed
#' to it via `f_cust_para`. The function should return a data frame similar to the data
#' that was passed to it, it must contain have the same column names (meaning you can't
#' remove indicators), but otherwise is flexible - this means some caution is necessary
#' to ensure that subsequent operations don't fail. Be careful, for example, to ensure
#' that there are no duplicates in `uCode`, and that indicator columns are numeric.
#'
#' The function assigned to `f_cust` is passed to [base::do.call()], therefore it can
#' be passed either as a string naming the function, or as the function itself. Depending
#' on the context, the latter option may be preferable because this stores the function
#' within the coin, which makes it portable. Otherwise, if the function is simply
#' named as a string, you must make sure it is available to access in the environment.
#'
#' @param x A purse object
#' @param dset The data set to apply the operation to.
#' @param f_cust Function to apply to the data set. See details.
#' @param f_cust_para Optional additional parameters to pass to the function defined
#' by `f_cust`.
#' @param write_to Name of data set to write to
#' @param ... Arguments to pass to/from other methods.
#' @param global Logical: if `TRUE`, the entire data set, over all time points, is passed
#' to the function `f_cust`. This is useful if the custom operation should be different for
#' different time points, for example. Otherwise if `FALSE`, passes the data set within each
#' coin one at a time to `f_cust`.
#'
#' @return An updated purse.
#' @export
#'
#' @examples
#' # build example purse
#' purse <- build_example_purse(up_to = "new_coin")
#'
#' # custom function - set points before 2020 to NA for BEL in FDI due to a
#' # break in the series
#' f_cust <- function(x){x[(x$uCode == "BEL") & (x$Time < 2020), "FDI"] <- NA;
#'                       return(x)}
#'
#'
#'
Custom.purse <- function(x, dset, f_cust, f_cust_para = NULL, global = FALSE,
                         write_to = NULL, ...){

  # input check
  check_purse(x)

  if(global){

    iDatas <- get_dset(x, dset)

    # run global dset through function
    iDatas_c <- do.call(f_cust, c(list(x = iDatas), f_cust_para))

    if("uCode" %nin% names(iDatas_c)){
      stop("Required column 'uCode' not present in the data frame returned by f_cust.")
    }
    if("Time" %nin% names(iDatas_c)){
      stop("Required column 'Time' not present in the data frame returned by f_cust.")
    }
    if(!setequal(names(iDatas_c), names(iDatas))){
      stop("The output of ", f_cust, " has different column names than the input data set.")
    }

    # split by Time
    iDatas_c_l <- split(iDatas_c, iDatas_c$Time)

    # now write dsets to coins
    x$coin <- lapply(x$coin, function(coin){

      # get Time
      tt <- coin$Meta$Unit$Time[[1]]
      if(is.null(tt)){
        stop("Time index is NULL or not found in writing treated data set to coin.")
      }

      if(is.null(write_to)){
        write_to <- "Custom"
      }

      iData_write <- iDatas_c_l[[which(names(iDatas_c_l) == tt)]]
      iData_write <- iData_write[names(iData_write) != "Time"]

      # write dset first
      coin <- write_dset(coin, iData_write, dset = write_to)

      # also write to log - we signal that coin can't be regenerated any more
      coin$Log$can_regen <- FALSE
      coin$Log$message <- "Coin was subject to a custom operation inside a purse with global = TRUE. Cannot be regenerated."

      coin
    })

  } else {

    # apply treatment to each coin
    x$coin <- lapply(x$coin, function(coin){
      Custom.coin(coin, dset = dset, f_cust = f_cust, f_cust_para = f_cust_para,
                  write_to = write_to)
    })

  }

  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}


#' Custom operation
#'
#' Custom operation on a coin. This is an experimental new feature so please check
#' the results carefully.
#'
#' In this function, the data set named `dset` is extracted from the coin using
#' `get_dset(coin, dset)`. It is passed to the function `f_cust`, which is required
#' to return an equivalent but modified data frame, which is then written as a new
#' data set with name `write_to`. This is intended to allow arbitrary operations
#' on coin data sets while staying within the COINr framework, which means that if
#' `Regen()` is used, these operations will be re-run, allowing them to be included
#' in things like sensitivity analysis.
#'
#' The format of `f_cust` is important. It must be a function whose first argument
#' is called `x`: this will be the argument that the data is passed to. The data will
#' be in the same format as extracted via `get_dset(coin, dset)`, which means it will
#' have a `uCode` column. `f_cust` can have other arguments which are passed
#' to it via `f_cust_para`. The function should return a data frame similar to the data
#' that was passed to it, it must contain have the same column names (meaning you can't
#' remove indicators), but otherwise is flexible - this means some caution is necessary
#' to ensure that subsequent operations don't fail. Be careful, for example, to ensure
#' that there are no duplicates in `uCode`, and that indicator columns are numeric.
#'
#' The function assigned to `f_cust` is passed to [base::do.call()], therefore it can
#' be passed either as a string naming the function, or as the function itself. Depending
#' on the context, the latter option may be preferable because this stores the function
#' within the coin, which makes it portable. Otherwise, if the function is simply
#' named as a string, you must make sure it is available to access in the environment.
#'
#' @param x A coin
#' @param dset Target data set
#' @param f_cust Function to apply to the data set. See details.
#' @param f_cust_para Optional additional parameters to pass to the function defined
#' by `f_cust`.
#' @param write_to Name of data set to write to
#' @param write2log Logical: whether or not to write to the log.
#' @param ... Arguments to pass to/from other methods.
#'
#' @return A coin
#' @export
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin")
#'
#' # create function - replaces suspected unreliable point with NA
#' f_NA <- function(x){ x[3, 10] <- NA; return(x)}
#'
#' # call function from Custom()
#' coin <- Custom(coin, dset = "Raw", f_cust = f_NA)
#' stopifnot(is.na(coin$Data$Custom[3,10]))
#'
Custom.coin <- function(x, dset, f_cust, f_cust_para = NULL, write_to = NULL,
                       write2log = TRUE, ...){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x", write2log = write2log)

  # GET DSET, CHECKS --------------------------------------------------------

  iData <- get_dset(coin, dset)

  # APPLY OPERATION ---------------------------------------------------------

  iDatac <- do.call(f_cust, c(list(x = iData), f_cust_para))

  if(!is.data.frame(iDatac)){
    stop("The output of ", f_cust, " is not a data frame. 'f_cust' is required to return a data frame.")
  }
  # if(nrow(iDatac) != nrow(iData)){
  #   stop("The output of ", f_cust, " has a different number of rows than the input data set.")
  # }
  if("uCode" %nin% names(iDatac)){
    stop("Required column 'uCode' not present in the data frame returned by f_cust.")
  }
  if(!setequal(names(iDatac), names(iData))){
    stop("The output of ", f_cust, " has different column names than the input data set.")
  }

  # Write to coin
  if(is.null(write_to)){
    write_to <- "Custom"
  }
  write_dset(coin, iDatac, dset = write_to)

}

#' Custom operation
#'
#' Allows a custom data operation on coins or purses.
#'
#' @param x Object to be operated on (coin or purse)
#' @param ... arguments passed to or from other methods.
#'
#' @return Modified object.
#'
#' @export
Custom <- function (x, ...){
  UseMethod("Custom")
}
