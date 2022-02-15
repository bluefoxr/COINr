# example generation

#' Build ASEM example coin
#'
#' Shortcut function to build the ASEM example coin, using inbuilt example data
#'
#' @param up_to The point up to which to build the index. If `NULL`, builds full index. Else specify a `build_*` function
#' (as a string) - the index will be built up to and including this function. This option is mainly for helping with
#' function examples. Example: `up_to = "build_normalise"`.
#' @param quietly If `TRUE`, suppresses all messages.
#'
#' @examples
#' #
#'
#' @return coin class object
#'
#' @export
build_example_coin <- function(up_to = NULL, quietly = FALSE){

  if(quietly){
    coin <- suppressMessages(build_example_coin(up_to = up_to, quietly = FALSE))
    return(coin)
  }

  if(is.null(up_to)){
    up_to = "theend"
  } else {
    stopifnot(is.character(up_to),
              length(up_to)==1)
  }

  # INITIALISE
  coin <- new_coin(ASEM_iData, ASEM_iMeta, level_names = c("Indicator", "Pillar", "Sub-index", "Index"))
  if(up_to == "new_coin"){
    return(coin)
  }

  # SCREEN economies based on data availability rules
  coin <- screen_units(coin, dset = "Raw", dat_thresh = 0.9, unit_screen = "byNA")
  if(up_to == "screen_units"){
    return(coin)
  }

  # TREAT data
  coin <- treat2(coin, dset = "Screened")
  if(up_to == "treat"){
    return(coin)
  }

  # NORMALISE data
  coin <- normalise2(coin, dset = "Treated")
  if(up_to == "normalise"){
    return(coin)
  }

  # AGGREGATE data
  coin <- aggregate2(coin, dset = "Normalised")
  if(up_to == "aggregate"){
    return(coin)
  }

  coin
}


#' Build example purse
#'
#' Shortcut function to build an artificial purse, to be at some point replaced with a proper example.
#'
#' @param up_to The point up to which to build the index. If `NULL`, builds full index. Else specify a `build_*` function
#' (as a string) - the index will be built up to and including this function. This option is mainly for helping with
#' function examples. Example: `up_to = "build_normalise"`.
#' @param quietly If `TRUE`, suppresses all messages.
#'
#' @examples
#' #
#'
#' @return coin class object
#'
#' @export
build_example_purse <- function(up_to = NULL, quietly = FALSE){

  if(quietly){
    purse <- suppressMessages(build_example_purse(up_to = up_to, quietly = FALSE))
    return(purse)
  }

  if(is.null(up_to)){
    up_to = "theend"
  } else {
    stopifnot(is.character(up_to),
              length(up_to)==1)
  }

  # INITIALISE
  purse <- new_coin(ASEM_iData_p, ASEM_iMeta, level_names = c("Indicator", "Pillar", "Sub-index", "Index"),
                    split_to = "all")
  if(up_to == "new_coin"){
    return(purse)
  }

  # SCREEN
  purse <- screen_units(purse, dset = "Raw", dat_thresh = 0.9, unit_screen = "byNA")
  if(up_to == "screen_units"){
    return(purse)
  }

  # TREAT data
  purse <- treat2(purse, dset = "Screened")
  if(up_to == "treat"){
    return(purse)
  }

  # NORMALISE data
  purse <- normalise2(purse, dset = "Treated", global = TRUE)
  if(up_to == "normalise"){
    return(purse)
  }

  # AGGREGATE data
  purse <- aggregate2(purse, dset = "Normalised")
  if(up_to == "aggregate"){
    return(purse)
  }

  purse

}
