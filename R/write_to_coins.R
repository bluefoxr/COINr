# WRITING TO COINS

# Write function arguments to log
#
# This used inside `build_*` functions. It takes the coin object as an input, then writes the arguments of the
# current function into the `.$Log` list of the coin object. This is then used as a record of the operations used
# to build the coin, and can be edited.
#
# @param coin A coin class object
# @param dont_write Any variables not to write to the coin
# @param write2log If `FALSE`, just passes the coin back without writing anything.
#
# @examples
# #
#
# @return Updated GII2 object with function arguments written to `.$Log`
write_log <- function(coin, dont_write = NULL, write2log = TRUE){

  if(!write2log){
    return(coin)
  }

  # get calling function name and its arguments
  func_args <- as.list(sys.frame(-1))
  func_name <- deparse(as.list(sys.call(-1))[[1]])

  # tweak list first (exclude args we don't want)
  dont_write <- c(dont_write, "coin", "*tmp*")
  func_args <- func_args[!(names(func_args) %in% dont_write)]

  # check that we are getting function arguments and nothing else
  if(!all(names(func_args) %in% names(formals(func_name)))){
    stop(paste0("Mismatch between function arguments of ", func_name, " and attempt to write to .$Log."))
  }

  # remove method .coin or similar from func_name
  func_name2 <- unlist(strsplit(func_name, "\\.")[[1]])[1]

  # write to coin
  coin$Log[[func_name2]] <- func_args
  coin

}

# Direct function outputs
#
# Shortcut to be used at end of functions, either attach to coin or output as list.
#
# @param coin A coin class object
# @param l The list to direct
# @param out2 Whether list or attach to coin
# @param lev1 Address at first lev of coin
# @param lev2 Address at second lev of coin
# @param lev3 Address at third lev of coin
#
# @examples
# #
#
# @return Either a list or an updated coin
write2coin <- function(coin, l, out2, lev1, lev2, lev3 = NULL){

  check_coin_input(coin)

  if(out2 %nin% c("list", "coin")){
    stop("out2 not recognised, should be either 'coin' or 'list'")
  }

  if(out2 == "list"){
    l
  } else if (out2 == "coin"){
    if(is.null(lev3)){
      coin[[lev1]][[lev2]] <- l
    } else {
      coin[[lev1]][[lev2]][[lev3]] <- l
    }
    coin
  }

}


# Write a named data set to coin
#
# Writes a data set to the coin, and performs some checks in the process.
#
# @param coin A coin class object
# @param x The data to write
# @param dset A character string for naming the data, e.g. `Raw`.
# @param quietly If `TRUE`, suppresses messages.
# @param ignore_class If `TRUE` ignores the class of the input (used for [new_coin()]).
#
# @examples
# #
#
# @return Updated coin
write_dset <- function(coin, x, dset, quietly = FALSE, ignore_class = FALSE){

  # checks
  if(!ignore_class){
    stopifnot(is.coin(coin))
  }
  stopifnot(is.character(dset),
            length(dset)==1,
            is.data.frame(x))

  # further checks
  if(is.null(x$uCode)){
    stop("Required col uCode not found in data set to write to coin.")
  }
  icodes <- names(x)[names(x) != "uCode"]
  not_numeric <- !(sapply(x[icodes], is.numeric))
  if(any(not_numeric)){
    stop("Non-numeric cols detected in data set to be written to coin (excluding uCode).")
  }
  if(any(icodes %nin% coin$Meta$Ind$iCode[coin$Meta$Ind$Type %in% c("Indicator", "Aggregate")])){
    stop("Names of columns do not correspond to entries in .$Meta$Ind$iCode in data to write to coin.")
  }

  # flag if dset exists
  dset_exists <- !is.null(coin$Data[[dset]])

  # write to coin
  coin$Data[[dset]] <- x

  if(!quietly){
    message("Written data set to .$Data$", dset)
    if(dset_exists){
      message("(overwritten existing data set)")
    }
  }

  coin

}
