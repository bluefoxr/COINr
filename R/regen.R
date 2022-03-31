# COIN REGENERATION


#' Regenerate a purse
#'
#' Regenerates the `.$Data` entries in all coins by rerunning the construction functions according to the specifications in
#' `.$Log`, for each coin in the purse. This effectively regenerates the results.
#'
#' The `from` argument allows partial regeneration, starting from a
#' specified function. This can be helpful to speed up regeneration in some cases. However, keep in mind that
#' if you change a `.$Log` argument from a function that is run before the point that you choose to start running
#' from, it will not affect the results.
#'
#' Note that for the moment, regeneration of purses is only partially supported. This is because usually, in the
#' normalisation step, it is necessary to normalise across the full panel data set (see the `global` argument in
#' [normalise()]). At the moment, purse regeneration is performed by regenerating each coin individually, but this
#' does not allow for global normalisation which has to be done at the purse level. This may be fixed in future
#' releases.
#'
#' @param x A purse class object
#' @param from Optional: a construction function name. If specified, regeneration begins from this function, rather
#' than re-running all functions.
#' @param quietly If `TRUE` (default), messages are suppressed during building.
#'
#' @examples
#' #
#'
#' @return Updated purse object with regenerated results.
#'
#' @export
regen2.purse <- function(x, from = NULL, quietly = TRUE){

  # input check
  check_purse(x)

  # regen each coin
  x$coin <- lapply(x$coin, function(coin){
    regen2.coin(coin, from = from, quietly = quietly)
  })
  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}

#' Regenerate a coin
#'
#' Regenerates the `.$Data` entries by rerunning the construction functions according to the specifications in `.$Log`.
#' This effectively regenerates the results. Different variations of coins can be quickly achieved by editing the
#' saved arguments in `.$Log` and regenerating.
#'
#' The `from` argument allows partial regeneration, starting from a
#' specified function. This can be helpful to speed up regeneration in some cases. However, keep in mind that
#' if you change a `.$Log` argument from a function that is run before the point that you choose to start running
#' from, it will not affect the results.
#'
#' Note that while sets of weights will be passed to the regenerated COIN, anything in `.$Analysis` will be removed
#' and will have to be recalculated.
#'
#' @param x A coin class object
#' @param from Optional: a construction function name. If specified, regeneration begins from this function, rather
#' than re-running all functions.
#' @param quietly If `TRUE` (default), messages are suppressed during building.
#'
#' @examples
#' #
#'
#' @return Updated coin object with regenerated results.
#'
#' @export
regen2.coin <- function(x, from = NULL, quietly = TRUE){

  coin <- x

  stopifnot(is.coin(coin))

  # the full list of function arguments, for each build_ function
  f_logs <- coin$Log
  f_names <- names(f_logs)

  # check if can regenerate
  stopifnot(!is.null(f_logs$can_regen))
  if(!f_logs$can_regen){
    stop("Cannot regenerate coin. This may be because it has been normalised with global = TRUE, or
         it has been converted from an older COIN class.")
  }
  # remove can_regen from here
  f_names <- setdiff(f_names, "can_regen")
  f_logs <- f_logs[f_names]

  # here we exclude any function names that are before "from", if it is specified
  if(!is.null(from)){
    if(from %nin% f_names){
      stop("Function name specified by 'from' is not found in the coin log.")
    }
    i_name <- which(f_names == from) - 1
    if(i_name > 0){
      f_names <- f_names[-(1:i_name)]
    }
  }

  # looping over build_ functions
  for (func in f_names){

    # the arguments of the same func, stored in Log
    f_log <- f_logs[[func]]

    # the declared arguments of the function
    # NOTE this doesn't work here since construction funcs are now methods, so args are all (x, ...)
    #f_args <- names(formals(func))
    # check if what is in Log agrees with function arguments
    # if(!all(names(f_log) %in% f_args)){
    #   stop(paste0("Mismatch between function arguments of ", func, " and .$Log entry. Cannot regenerate."))
    # }

    # run function at arguments
    if(func == "new_coin"){
      if(quietly){
        coin <- suppressMessages( do.call(func, args = f_log) )
      } else {
        coin <- do.call(func, args = f_log)
      }

    } else {
      # add coin obj to arg list (not logged for obvious inception reasons)
      if(quietly){
        coin <- suppressMessages( do.call(func, args = c(list(x = coin), f_log) ) )
      } else {
        coin <- do.call(func, args = c(list(x = coin), f_log) )
      }

    }

  }
  coin
}


#' Regenerate a coin or purse
#'
#' Methods for regenerating coins and purses.
#'
#' @param x A coin or purse object to be regenerated
#' @param from Optional: a construction function name. If specified, regeneration begins from this function, rather
#' than re-running all functions.
#' @param quietly If `TRUE` (default), messages are suppressed during building.
#'
#' @examples
#' #
#'
#' @return A regenerated object
#'
#' @export
regen2 <- function(x, from = NULL, quietly = TRUE){
  UseMethod("regen2")
}


#' Add and remove indicators
#'
#' A shortcut function to add and remove indicators. This will make the relevant changes
#' and recalculate the index if asked. Adding and removing is done relative to the current set of
#' indicators used in calculating the index results. Any indicators that are added must of course be
#' present in the original `iData` and `iMeta` that were input to `new_coin()`.
#'
#' @param coin coin object
#' @param add A character vector of indicator codes to add (must be present in the original input data)
#' @param drop A character vector of indicator codes to remove (must be present in the original input data)
#' @param regen Logical (default): if `TRUE`, automatically regenerates the results based on the new specs
#' Otherwise, just updates the `.$Log` parameters. This latter might be useful if you want to
#' Make other changes before re-running using the [regen()] function.
#'
#' @examples
#' # build ASEM example
#' ASEM <- build_ASEM()
#' # remove one indicator and regenerate results
#' ASEM2 <- indChange(ASEM, drop = "UNVote", regen = TRUE)
#' # compare the differences
#' CT <- compTable(ASEM, ASEM2, dset = "Aggregated", isel = "Index")
#'
#' @seealso
#' * [regen()] regenerate a coin
#' * [compTable()] compare two different coins
#' * [compTableMulti()] compare multiple coins
#'
#' @return An updated coin, with regenerated results if `regen = TRUE`.
#'
#' @export
change_ind <- function(coin, add = NULL, drop = NULL, regen = FALSE){

  # find existing indicator set
  iCodes_1 <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Indicator"]

  # full set of codes from iMeta
  iCodes_0 <- coin$Log$new_coin$iMeta$iCode[coin$Log$new_coin$iMeta$Type == "Indicator"]

  # CHECKS
  if(!is.null(add)){
    if(any(add %nin% iCodes_0)){
      stop("One or more iCodes in 'add' not found in original indicator data set.")
    }
  }
  if(!is.null(drop)){
    if(any(drop %nin% iCodes_1)){
      stop("One or more iCodes in 'drop' not found in existing indicator data set.")
    }
  }
  if(!is.null(drop) & !is.null(add)){
    if(length(intersect(add, drop)) > 0){
      stop("One or more iCodes in 'drop' also found in 'add'!")
    }
  }

  # NOW GET SET OF iCODES TO USE

  # add first
  iCodes_2 <- union(iCodes_1, add)
  # then drop
  iCodes_2 <- setdiff(iCodes_2, drop)
  # now find the exclude parameter: diff between iCodes_2 and iCodes_0
  exclude <- setdiff(iCodes_0, iCodes_2)

  # EDIT .$Log

  coin$Log$new_coin$exclude <- exclude

  # REGEN if asked (nicely)
  if(regen==TRUE){
    coin <- regen2(coin, quietly = TRUE)
    message("coin has been regenerated using new specs.")
  } else {
    message("coin parameters changed but results NOT updated. Use coinr::regen() to regenerate
results or set regen = TRUE in change_inds().")
  }

  coin
}