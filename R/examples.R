# example generation

#' Build ASEM example coin
#'
#' Shortcut function to build the ASEM example coin, using inbuilt example data
#'
#' @param up_to The point up to which to build the index. If `NULL`, builds full index. Else specify a `build_*` function
#' (as a string) - the index will be built up to and including this function. This option is mainly for helping with
#' function examples. Example: `up_to = "build_normalise"`.
#'
#' @examples
#' #
#'
#' @return coin class object
#'
#' @export
build_example_coin <- function(up_to = NULL){

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

  coin

  # # TREAT
  # # individual specifications
  # indiv_spec <- data.frame(
  #   CODE = c("FDINetInfl", "PatFam", "MicroLoan"),
  #   Treat = c("log", "win", "win"),
  #   Winmax = c(NA, 5, 6),
  #   forced_win = c(FALSE, FALSE, TRUE)
  # )
  # # run data treatment function
  # GII2020 <- build_treat(GII2020, indiv_spec = indiv_spec,
  #                        reset_points = data.frame(ISO3 = "RWA", CODE = "MicroLoan"))
  # if(up_to == "build_treat"){
  #   return(GII2020)
  # }
  #
  # # NORMALISE
  # # assemble df for specification of "special cases", i.e. the scaled indicators
  # nspecs_indiv <- data.frame(
  #   CODE = GII2::ScalePara_2020$IndCode,
  #   ntype = "scaled"
  # )
  # # add the parameters separately. This is a "list column"
  # nspecs_indiv$npara <- GII2::ScalePara_2020[c("l","u")] |>
  #   t() |>
  #   as.data.frame() |>
  #   as.list()
  # # now run the normalise function
  # GII2020 <- build_normalise(GII2020, default_ntype = "minmax", default_npara = c(0,100),
  #                            indiv_spec = nspecs_indiv)
  # if(up_to == "build_normalise"){
  #   return(GII2020)
  # }
  #
  # # AGGREGATE
  # GII2020 <- build_aggregate(GII2020)
  #
  # if(up_to == "build_aggregate"){
  #   return(GII2020)
  # }
  #
  # # country profiles
  # GII2020 <- get_profiles(GII2020)
  #
  # GII2020

}


#' Build example purse
#'
#' Shortcut function to build an artificial purse, to be at some point replaced with a proper example.
#'
#' @param up_to The point up to which to build the index. If `NULL`, builds full index. Else specify a `build_*` function
#' (as a string) - the index will be built up to and including this function. This option is mainly for helping with
#' function examples. Example: `up_to = "build_normalise"`.
#'
#' @examples
#' #
#'
#' @return coin class object
#'
#' @export
build_example_purse <- function(up_to = NULL){

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

  # SCREEN economies based on data availability rules
  purse <- screen_units(purse, dset = "Raw", dat_thresh = 0.9, unit_screen = "byNA")
  if(up_to == "screen_units"){
    return(purse)
  }

  purse

}
