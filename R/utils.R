# UTILITY FUNCTIONS

#' Write function arguments to log
#'
#' This used inside `build_*` functions. It takes the coin object as an input, then writes the arguments of the
#' current function into the `.$Log` list of the coin object. This is then used as a record of the operations used
#' to build the coin, and can be edited.
#'
#' @param coin A coin class object
#'
#' @examples
#' #
#'
#' @return Updated GII2 object with function arguments written to `.$Log`
write_log <- function(coin){

  # get calling function name and its arguments
  func_args <- as.list(sys.frame(-1))
  func_name <- deparse(as.list(sys.call(-1))[[1]])

  # tweak list first
  func_args <- func_args[!(names(func_args) %in% c("coin", "*tmp*"))]

  # check that we are getting function arguments and nothing else
  if(!all(names(func_args) %in% names(formals(func_name)))){
    stop(paste0("Mismatch between function arguments of ", func_name, " and attempt to write to .$Log."))
  }

  # write to coin
  coin$Log[[func_name]] <- func_args
  coin

}


#' Not in operator
#'
#' For convenience, rather than always `!(x, %in% y)`
#'
#' @param x A scalar or vector
#' @param y A scalar or vector
#'
#' @return TRUE if x is not in y, FALSE otherwise
'%nin%' <- function(x,y){
  !('%in%'(x,y))
}

#' Print coin
#'
#' Some details about the coin
#'
#' @param x A coin
#' @param ... Arguments to be passed to or from other methods.
#'
#' @examples
#' #
#'
#' @importFrom utils head
#'
#' @return Text output
#'
#' @export
print.coin <- function(x, ...){

  coin <- x

  cat("--------------\n")
  cat("A coin with...\n")
  cat("--------------\n")
  # Input
  # Units
  firstunits <- paste0(utils::head(coin$Data$Raw$uCode, 3), collapse = ", ")
  if(length(coin$Data$Raw$uCode)>3){
    firstunits <- paste0(firstunits, ", ...")
  }

  # Indicators
  iCodes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Indicator"]
  firstinds <- paste0(utils::head(iCodes, 3), collapse = ", ")
  if(length(iCodes)>3){
    firstinds <- paste0(firstinds, ", ...")
  }

  # Denominators
  denoms <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Denominator"]
  if(!is.null(denoms)){
    ndenom <- length(denoms)
    denoms <- paste0(utils::head(denoms, 3), collapse = ", ")
    if(ndenom>3){
      denoms <- paste0(denoms, ", ...")
    }
  } else {
    denoms <- "none"
    ndenom <- 0
  }

  # groups
  grps <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Group"]
  if(length(grps)>0){
    ngrp <- length(grps)
    grps <- paste0(utils::head(grps, 3), collapse = ", ")
    if(ngrp>3){
      grps <- paste0(grps, ", ...")
    }
  } else {
    grps <- "none"
    ngrp <- 0
  }


  cat("Input:\n")
  cat("  Units: ", nrow(coin$Data$Raw), " (", firstunits, ")\n", sep = "")
  cat(paste0("  Indicators: ", length(iCodes), " (", firstinds, ")\n"))
  cat(paste0("  Denominators: ", ndenom, " (", denoms, ")\n"))
  cat(paste0("  Groups: ", ngrp, " (", grps, ")\n\n"))


  # Structure
  fwk <- coin$Meta$Lineage

  cat("Structure:\n")

  for(ii in 1:ncol(fwk)){

    codes <- unique(fwk[[ii]])
    nuniq <- length(codes)
    first3 <- utils::head(codes, 3)
    if(length(codes)>3){
      first3 <- paste0(first3, collapse = ", ")
      first3 <- paste0(first3, ", ...")
    } else {
      first3 <- paste0(first3, collapse = ", ")
    }

    # colnames are level names
    levnames <- colnames(fwk)
    # check if auto-generated, if so we don't additionally print.
    if(levnames[1] == "Level_1"){
      levnames <- NULL
    }

    if(ii==1){
      cat(paste0("  Level ", ii, " ", levnames[ii], ": ", nuniq, " indicators (", first3,") \n"))
    } else {
      cat(paste0("  Level ", ii, " ", levnames[ii], ": ", nuniq, " groups (", first3,") \n"))
    }

  }
  cat("\n")

  # Data sets
  cat("Data sets:\n")
  dsets <- names(coin$Data)
  for(dset in dsets){
    nunit <- nrow(coin$Data[[dset]])
    cat(paste0("  ", dset, " (", nunit, " units)\n"))
  }
}

#' Print purse
#'
#' Some details about the purse
#'
#' @param x A purse
#' @param ... Arguments to be passed to or from other methods.
#'
#' @examples
#' #
#'
#' @importFrom utils head
#'
#' @return Text output
#'
#' @export
print.purse <- function(x, ...){

  coin <- x$coin[[1]]

  cat("-----------------------------\n")
  cat("A purse with...", nrow(x), "coins \n")
  cat("-----------------------------\n\n")

  dfdisplay <- data.frame(
    Time = x$Time,
    n_Units = sapply(x$coin, function(coin){nrow(coin$Data$Raw)}),
    n_Inds = sapply(x$coin, function(coin){sum(coin$Meta$Ind$Type == "Indicator")}),
    n_dsets = sapply(x$coin, function(coin){length(coin$Data)})
  )

  print(dfdisplay, row.names = FALSE)

  cat("\n")

  cat("-----------------------------------\n")
  cat("Sample from first coin (", x$Time[1],"):\n", sep = "")
  cat("-----------------------------------\n\n")

  # Input
  # Units
  firstunits <- paste0(utils::head(coin$Data$Raw$uCode, 3), collapse = ", ")
  if(length(coin$Data$Raw$uCode)>3){
    firstunits <- paste0(firstunits, ", ...")
  }

  # Indicators
  iCodes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Indicator"]
  firstinds <- paste0(utils::head(iCodes, 3), collapse = ", ")
  if(length(iCodes)>3){
    firstinds <- paste0(firstinds, ", ...")
  }

  # Denominators
  denoms <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Denominator"]
  if(!is.null(denoms)){
    ndenom <- length(denoms)
    denoms <- paste0(utils::head(denoms, 3), collapse = ", ")
    if(ndenom>3){
      denoms <- paste0(denoms, ", ...")
    }
  } else {
    denoms <- "none"
    ndenom <- 0
  }

  # groups
  grps <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Group"]
  if(length(grps)>0){
    ngrp <- length(grps)
    grps <- paste0(utils::head(grps, 3), collapse = ", ")
    if(ngrp>3){
      grps <- paste0(grps, ", ...")
    }
  } else {
    grps <- "none"
    ngrp <- 0
  }


  cat("Input:\n")
  cat("  Units: ", nrow(coin$Data$Raw), " (", firstunits, ")\n", sep = "")
  cat(paste0("  Indicators: ", length(iCodes), " (", firstinds, ")\n"))
  cat(paste0("  Denominators: ", ndenom, " (", denoms, ")\n"))
  cat(paste0("  Groups: ", ngrp, " (", grps, ")\n\n"))


  # Structure
  fwk <- coin$Meta$Lineage

  cat("Structure:\n")

  for(ii in 1:ncol(fwk)){

    codes <- unique(fwk[[ii]])
    nuniq <- length(codes)
    first3 <- utils::head(codes, 3)
    if(length(codes)>3){
      first3 <- paste0(first3, collapse = ", ")
      first3 <- paste0(first3, ", ...")
    } else {
      first3 <- paste0(first3, collapse = ", ")
    }

    # colnames are level names
    levnames <- colnames(fwk)
    # check if auto-generated, if so we don't additionally print.
    if(levnames[1] == "Level_1"){
      levnames <- NULL
    }

    if(ii==1){
      cat(paste0("  Level ", ii, " ", levnames[ii], ": ", nuniq, " indicators (", first3,") \n"))
    } else {
      cat(paste0("  Level ", ii, " ", levnames[ii], ": ", nuniq, " groups (", first3,") \n"))
    }

  }
  cat("\n")

  # Data sets
  cat("Data sets:\n")
  dsets <- names(coin$Data)
  for(dset in dsets){
    nunit <- nrow(coin$Data[[dset]])
    cat(paste0("  ", dset, " (", nunit, " units)\n"))
  }
}

#' Check if object is coin class
#'
#' @param x An object to be checked.
#'
#' @examples
#' is.coin("not_a_coin")
#' # add example with coin
#'
#' @return Logical
#'
#' @export

is.coin <- function(x){
  inherits(x, "coin")
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
#' @param coin A coin class object
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#'
#' @examples
#' #
#'
#' @return Error message if `dset` not found.
check_dset <- function(coin, dset){

  stopifnot(is.coin(coin),
            is.character(dset),
            length(dset)==1)

  if(is.null(coin$Data[[dset]])){
    stop("Required data set '", dset, "' not found in coin object.")
  }
}

#' Gets a named data set and performs checks
#'
#' A helper function to retrieve a named data set from the coin object. Also performs input checks at the
#' same time.
#'
#' @param coin A coin class object
#' @param dset A character string corresponding to a named data set within `coin$Data`. E.g. `Raw`.
#'
#' @examples
#' #
#'
#' @return Data frame of indicator data.
#'
#' @export
get_dset <- function(coin, dset){

  # Make sure we have a coin class
  check_coin_input(coin)
  # check specified dset exists
  check_dset(coin, dset)
  # get dset
  coin$Data[[dset]]
}
