#' Export a coin to Excel
#'
#' Exports the contents of the coin to Excel. This writes all data frames inside the coin to Excel, with each data
#' frame on a separate tab. Tabs are named according to the position in the coin object. You can write other
#' data frames by simply attaching them to the coin object somewhere.
#'
#' @param x A coin class object
#' @param fname The file name/path to write to, as a character string
#' @param include_log Logical: if `TRUE`, also writes data frames from the `.$Log` list inside the coin.
#' @param ... arguments passed to or from other methods.
#'
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' ## Here we write a COIN to Excel, but this is done to a temporary directory
#' ## to avoid "polluting" the working directory when running automatic tests.
#' ## In a real case, set fname to a directory of your choice.
#'
#' # build example coin up to data treatment step
#' coin <- build_example_coin(up_to = "Treat")
#'
#' # write to Excel in temporary directory
#' export_to_excel(coin, fname = paste0(tempdir(), "\\ASEM_results.xlsx"))
#'
#' # spreadsheet is at:
#' print(paste0(tempdir(), "\\ASEM_results.xlsx"))
#'
#' # now delete temporary file to keep things tidy in testing
#' unlink(paste0(tempdir(),"\\ASEM_results.xlsx"))
#'
#' @return .xlsx file at specified path
#'
#' @export
export_to_excel.coin <- function(x, fname = "coin_export.xlsx", include_log = FALSE, ...){

  check_coin_input(x)

  # function to stop tab names exceeding 31 characters, avoiding errors.
  trunc_str <- function(x){
    if(nchar(x) > 31){
      xnew <- substr(x, 1, 31)
      warning("Truncated tab name (", x, ") because exceeds Excel length limit (31 characters).")
    } else {
      xnew <- x
    }
    xnew
  }

  if(!include_log){
    x$Log <- NULL
  }

  # recursive func to get all dfs in coin into a single list
  unlist_2_df <- function(x) {
    if (is.data.frame(x))
      return(list(x))
    if (!is.list(x))
      return(NULL)
    unlist(lapply(x, unlist_2_df), FALSE)
  }

  # unlist and alter any names that are too long
  coinwrite <- unlist_2_df(x)
  names(coinwrite) <- sapply(names(coinwrite), trunc_str)

  # write to excel
  openxlsx::write.xlsx(coinwrite, file = fname, colNames = TRUE)

}


#' Export a purse to Excel
#'
#' Exports the contents of the purse to Excel. This is similar to the coin method [export_to_excel.coin()],
#' but combines data sets from various time points. It also selectively writes metadata since this may be
#' spread across multiple coins.
#'
#' @param x A purse class object
#' @param fname The file name/path to write to, as a character string
#' @param include_log Logical: if `TRUE`, also writes data frames from the `.$Log` list inside the coin.
#' @param ... arguments passed to or from other methods.
#'
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' #
#'
#' @return .xlsx file at specified path
#'
#' @export
export_to_excel.purse <- function(x, fname = "coin_export.xlsx", include_log = FALSE, ...){


  # Prep --------------------------------------------------------------------

  check_purse_input(x)

  # function to stop tab names exceeding 31 characters, avoiding errors.
  trunc_str <- function(x){
    if(nchar(x) > 31){
      xnew <- substr(x, 1, 31)
      warning("Truncated tab name (", x, ") because exceeds Excel length limit (31 characters).")
    } else {
      xnew <- x
    }
    xnew
  }


  # Extract coin ------------------------------------------------------------
  # For the purse method, we take the first coin and modify it:
  # - merge dsets over time
  # - small other tweaks
  # - then write the coin

  # Extract first coin in the purse
  coin <- x$coin[[1]]

  # get names of data sets first
  dset_names <- names(coin$Data)

  # get data sets with all metadata attached
  dsets <- lapply(dset_names, function(dset){
    get_dset(x, dset, also_get = "all")
  })
  names(dsets) <- dset_names

  # replace data list with full dsets over time
  coin$Data <- dsets

  # I have to reconstruct the full uMeta because coins have different numbers of units
  icodes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type %in% c("Indicator", "Aggregate")]
  uMeta <- unique(dsets[[1]][!names(dsets[[1]]) %in% c(icodes, "Time")])
  coin$Meta$Unit <- uMeta

  # remove log if necessary
  if(!include_log){
    coin$Log <- NULL
  }

  # Assemble into list ------------------------------------------------------

  # recursive func to get all dfs in coin into a single list
  unlist_2_df <- function(x) {
    if (is.data.frame(x))
      return(list(x))
    if (!is.list(x))
      return(NULL)
    unlist(lapply(x, unlist_2_df), FALSE)
  }

  # unlist and alter any names that are too long
  coinwrite <- unlist_2_df(coin)
  names(coinwrite) <- sapply(names(coinwrite), trunc_str)

  # write to excel
  openxlsx::write.xlsx(coinwrite, file = fname, colNames = TRUE)

}

#' Export a coin or purse to Excel
#'
#' Writes coins and purses to Excel. See individual method
#' documentation:
#'
#' This function replaces the now-defunct `coin2Excel()` from COINr < v1.0.
#'
#' * [export_to_excel.coin()]
#' * [export_to_excel.purse()]
#'
#' @param x A coin or purse
#' @param fname The file name to write to
#' @param ... Arguments passed to/from methods
#'
#' @examples
#' # see individual method documentation
#'
#' @return An Excel spreadsheet.
#'
#' @export
export_to_excel <- function(x, fname, ...){
  UseMethod("export_to_excel")
}
