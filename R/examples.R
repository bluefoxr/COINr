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
  coin <- Screen(coin, dset = "Raw", dat_thresh = 0.9, unit_screen = "byNA")
  if(up_to == "Screen"){
    return(coin)
  }

  # TREAT data
  # Explicitly set winmax so that it is easy to find for SA
  coin <- Treat(coin, dset = "Screened", default_specs = list(f1_para = list(winmax = 5)))
  if(up_to == "treat"){
    return(coin)
  }

  # NORMALISE data
  # explicitly set normalisation specs to find for SA
  coin <- Normalise(coin, dset = "Treated", default_specs = list(f_n = "n_minmax",
                                                                  f_n_para = list(c(0,100))))
  if(up_to == "normalise"){
    return(coin)
  }

  # AGGREGATE data
  coin <- Aggregate(coin, dset = "Normalised", f_ag = "a_amean")
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
  purse <- Screen(purse, dset = "Raw", dat_thresh = 0.9, unit_screen = "byNA")
  if(up_to == "Screen"){
    return(purse)
  }

  # TREAT data
  purse <- Treat(purse, dset = "Screened")
  if(up_to == "treat"){
    return(purse)
  }

  # NORMALISE data
  purse <- Normalise(purse, dset = "Treated", global = TRUE)
  if(up_to == "normalise"){
    return(purse)
  }

  # AGGREGATE data
  purse <- Aggregate(purse, dset = "Normalised")
  if(up_to == "aggregate"){
    return(purse)
  }

  purse

}
