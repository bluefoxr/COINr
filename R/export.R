#' Export a coin to Excel
#'
#' Exports the contents of the coin to Excel. This writes all data frames inside the coin to Excel, with each data
#' frame on a separate tab. Tabs are named according to the position in the coin object. You can write other
#' data frames by simply attaching them to the coin object somewhere.
#'
#' @param coin A coin class object
#' @param fname The file name/path to write to, as a character string
#' @param include_log Logical: if `TRUE`, also writes data frames from the `.$Log` list inside the coin.
#'
#' @importFrom openxlsx write.xlsx
#'
#' @examples
#' #
#'
#' @return .xlsx file at specified path
#'
#' @export
export_to_excel <- function(coin, fname = "coin_export.xlsx", include_log = FALSE){

  check_coin_input(coin)

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
    coin$Log <- NULL
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
  coinwrite <- unlist_2_df(coin)
  names(coinwrite) <- sapply(names(coinwrite), trunc_str)

  # write to excel
  openxlsx::write.xlsx(coinwrite, file = fname, colNames = TRUE)

}
