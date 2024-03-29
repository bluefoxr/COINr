# METHODS ADDED TO NON-COINR GENERICS

#' Print coin
#'
#' Some details about the coin
#'
#' @param x A coin
#' @param ... Arguments to be passed to or from other methods.
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
#' @return Logical
#'
#' @export
is.coin <- function(x){
  inherits(x, "coin")
}

#' Check if object is purse class
#'
#' @param x An object to be checked.
#'
#' @return Logical
#'
#' @export

is.purse <- function(x){
  inherits(x, "purse")
}
