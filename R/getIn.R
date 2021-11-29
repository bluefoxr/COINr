#' Get subsets of indicator data
#'
#' This function does a number of things that are useful for many COINr functions and operations.
#' First, it checks to see what kind of input object is input. Then, it selects the indicator data
#' according to the specs supplied.
#'
#' For example, specifying `dset = "Raw"` and `icodes = c("Ind1", "Ind5")`, it will return the indicator columns
#' named `"Ind1" and "Ind5"` (if they exist), in the format described below. `icodes` can be indicators
#' or aggregation groups, and can call multiple groups.
#'
#' You can also specify which aggregation level to target, using the `aglev` argument. See examples
#' below, and in particular the [COINr online documentation](https://bluefoxr.github.io/COINrDoc/helper-functions.html#selecting-data-sets-and-indicators).
#'
#' As well as selection of columns, you can also filter to specific rows using unit codes as a reference, and/or
#' grouping variables. This is done using the `usel` and `use_group` arguments. This gives the ability to isolate
#' a unit inside a given group, for example.
#'
#' [getIn()] is used by many COINr functions for plotting, accessing and reporting subsets of indicator data.
#'
#' @param obj An input object. The function can handle either the COIN object, or a data frame.
#' The data frame should have each column as an indicator, and optional columns `UnitCode` and `UnitName` which
#' specify the code (or name) of each unit. Any columns except these latter two will be treated as indicators. Any other type of object will return an error.
#' @param dset If input is a COIN object, this specifies which data set in `.$Data` to use.
#' @param icodes An optional character vector of indicator codes to subset the indicator data. Usefully, can also refer to
#' an aggregation group name, and data will be subsetted accordingly. NOTE does not work with multiple aggregate group names.
#' @param aglev The aggregation level to take indicator data from. Integer from 1 (indicator level)
#' to N (top aggregation level, typically the index).
#' @param usel An optional unit code, or character vector of unit codes to use to filter the data. The returned data will
#' only include rows corresponding to the `usel`, unless `usel = NULL` (default).
#' @param use_group An optional grouping variable and group to filter data from. Of the format `list(Group_Var = Group)`,
#' where `Group_Var` is a Group_ column that must be present in the selected data set, and `Group` is a specified group
#' inside that grouping variable. This filters the selected data to only include rows from the specified group. Alternatively,
#' this argument can work in conjunction with `usel`: if `usel` is specified, `use_group` may be input as a string simply
#' representing a group column. In that case the data will be filtered to include only rows from the group(s) which the `usel`
#' belong to. If `usel` is specified and `use_group` is specified as a list, `usel` will take precedence and `use_group`
#' will be ignored.
#' @param justnumeric Logical: if `TRUE`, removes any non-numeric columns from `ind_data_only`. Otherwise keeps all except those.
#'
#' @importFrom magrittr extract
#' @importFrom dplyr select starts_with ends_with
#' @importFrom rlang .data
#'
#' @examples
#' # assemble ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # get indicator data from Social pillar
#' SocData <- getIn(ASEM, dset = "Raw", icodes = "Social", aglev = 1)
#' # Indicator codes
#' SocData$IndCodes
#' # Indicator data (no other columns)
#' SocData$ind_data_only
#'
#' @return A list with the following entries:
#' * `.$IndCodes`  The indicator codes
#' * `.$IndNames`  The indicator names (if a COIN object is input)
#' * `.$ind_data` A data frame of indicator data, according to the input specifications, including any unit codes, names and groups
#' * `.$ind_data_only` A data frame, as above, but without unit codes, names, groups.
#' * `.$UnitCodes` Unit codes of selected data set.
#' * `.$otype` Object type (a string: either `"COINobj"` or `"df"`).
#'
#' @export

getIn <- function(obj, dset = "Raw", icodes = NULL, aglev = NULL,
                  usel = NULL, use_group = NULL, justnumeric = TRUE){

  # Check to see what kind of input we have.
  if (is.COIN(obj)){ # COIN obj

    otype <- "COINobj"

    if(!is.null(aglev)){
      if(aglev>1 & !(dset %in% c("Aggregated", "PreAggregated"))){
        stop("Cannot select data from higher levels (aglev > 1) unless it is aggregated first. dset must be 'Aggregated' or 'PreAggregated'.")
      }
    }


    # The full table of indicator data
    # If looking for denominators, it is in a separate place
    if (dset=="Denominators"){
      ind_data = tryCatch({
        obj$Input$Denominators
      }, error = function(e) {
        stop("Denominators not found.")
      })
    } else {
      # all other dsets are here in the .$Data folder
      ind_data = tryCatch({
        obj$Data[[dset]]
      }, error = function(e) {
        stop("dset name not recognised...")
      })
    }
    if(is.null(ind_data)){
      stop("dset not found. Did you make a typo or forget to create it first?")
    }

    # get unit codes. Have to do this because if the units have been screened, then it may
    # not be the same set as when the data was input.
    UnitCodes <- ind_data$UnitCode

    # If no indicator names specified, return all
    if(is.null(icodes)){
      # get indicator names, i.e. columns excluding groups, denominators, names etc.
      icodes <- ind_data %>% dplyr::select(!dplyr::starts_with(
        c("UnitCode", "UnitName", "Year", "Group_", "x_")) ) %>% colnames()
    }

    if (is.null(aglev) | (dset=="Denominators")){ # take icodes as it is given
      IndCodes <- icodes
    } else {

      # get index structure from Indicator metadata
      aggcols <- dplyr::select(obj$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg")) %>% as.data.frame()
      # filter any rows containing the specified string(s), pick column corresponding to agg level
      # Might look strange, but checks whether each row has any of the specified codes in.
      # Then, filters to those rows, and selects the column corresponding to the ag level.
      IndCodes <- aggcols[rowSums(sapply(aggcols, "%in%", icodes))>0,aglev] %>% unique()

    }

    # get indicator names
    if (dset!="Denominators"){
      IndNames <- obj$Parameters$Code2Name$AggName[
        obj$Parameters$Code2Name$AggCode %in% IndCodes]
    } else {
      # if data set is denominators, then just use codes (may update this at some point)
      IndNames <- IndCodes
    }

    # select indicator data columns
    # first, check if cols are present
    indcheck <- IndCodes %in% colnames(ind_data)
    if(all(!indcheck)){
      stop("Indicator code(s) not found in selected data set")
    } else if (any(!indcheck)){
      warning("One or more indicator codes not found in selected data set. Returning what I can.")
    }

    ind_data <- ind_data %>% dplyr::select(dplyr::starts_with(
      c("UnitCode", "UnitName", "Year", "Group_", "x_", IndCodes)) )

  } else if (is.data.frame(obj)){ # Data frame

    otype <- "df"

    ind_data <- obj

    if (is.null(icodes)){ # no ind codes given
      if (exists("UnitCode",ind_data)){
        # If there are unit codes, record them and assume all other cols are indicator names
        IndCodes <- obj[colnames(obj) != "UnitCode"] %>% colnames()
        UnitCodes <- obj$UnitCode
      } else {
        # All cols are indicators. No names supplied.
        IndCodes <- colnames(obj)
        UnitCodes <- NA
      }
    } else { # indicator names are supplied
      IndCodes <- icodes
      if (exists("UnitCode",ind_data)){
        UnitCodes <- obj$UnitCode
      } else {
        UnitCodes <- NA
      }
    }

    if (exists("UnitName",ind_data)){
      IndNames <- ind_data$UnitName
      IndCodes <- IndCodes[IndCodes != "UnitName"]
    } else {
      IndNames <- IndCodes # we don't know names, so use codes
    }


  } else { # Not COIN obj OR df
    stop("Input should either be COIN object or data frame.")
  }

  # Check if indicator codes actually found in data set
  # This can happen e.g. if we call the Index from the "Raw" data set (not yet aggregated).
  if (any(IndCodes %in% colnames(ind_data))){
    ind_data_only = ind_data[IndCodes]
  } else {
    ind_data_only = ind_data[NULL]
    warning("Indicator codes not found in selected level or data set.")
  }

  # finally, remove any non-numeric columns in ind_data_only
  if(justnumeric){
    numcols <- unlist(lapply(ind_data_only, is.numeric))
    ind_data_only <- ind_data_only[numcols]
    IndCodes <- IndCodes[numcols]
    if (length(numcols)>ncol(ind_data_only)){
      warning(paste0("Removed ",length(numcols)-ncol(ind_data_only), " non-numeric column(s)."))
    }
  }

  # finally 2: we have to do some row filtering by usel and groups
  # we find which rows to filter first depending on settings
  if(!is.null(usel)){

    if(!exists("UnitCode",ind_data)){
      stop("No UnitCode column found in data.")
    }
    if(all(!(usel %in% ind_data$UnitCode))){
      stop("None of usel are found in the selected data.")
    }
    if(any(!(usel %in% ind_data$UnitCode))){
      warning("One or more usel not found in the selected data - returning what was found.")
    }

    # we need to filter by usel
    if(is.null(use_group)){
      # simple case, pick usel rows
      rowfilter <- ind_data$UnitCode %in% usel
    } else {
      # more complicated, also have groups to think about
      if(is.list(use_group)){
        # a specific group is specified, however, usel takes precedence
        rowfilter <- ind_data$UnitCode %in% usel
      } else if (is.character(use_group)){
        # filter to group(s) of usel
        usel_groups <- unique(ind_data[[use_group]][ind_data$UnitCode %in% usel])
        rowfilter <- ind_data[[use_group]] %in% usel_groups
      } else {
        stop("use_group must either be a list or string.")
      }
    }

  } else if (!is.null(use_group) & is.null(usel)){

    # here, only use_group is specified
    if(is.list(use_group)){
      # some checks first
      stopifnot(length(use_group)==1)
      stopifnot(names(use_group) %in% colnames(ind_data))
      stopifnot(use_group[[1]] %in% ind_data[[names(use_group)]])
      # a specific group is specified, however, usel takes precedence
      rowfilter <- ind_data[[names(use_group)]] == use_group[[1]]
    } else if (is.character(use_group)){
      stop("If usel is not specified, use_group needs to be specified as a list.")
    } else {
      stop("use_group must either be a list or string.")
    }

  }

  # here is where the rows are actually filtered
  if(!is.null(usel)|!is.null(use_group)){
    ind_data <- ind_data[rowfilter,]
    ind_data_only <- ind_data_only[rowfilter,]
    UnitCodes <- UnitCodes[rowfilter]
  }

  out <- list(IndCodes = IndCodes,
              IndNames = IndNames,
              ind_data = ind_data,
              ind_data_only = ind_data_only,
              UnitCodes = UnitCodes,
              otype = otype # the object type
  )

  return(out)
}


#' Round down a data frame
#'
#' Tiny function just to round down a data frame for display in a table.
#'
#' @param df A data frame to input
#' @param decimals The number of decimal places to round to (default 2)
#'
#' @examples
#' roundDF( as.data.frame(matrix(runif(20),10,2)), decimals = 3)
#'
#' @seealso
#' * [rankDF()] Replace data frame numbers with ranks.
#'
#' @return A data frame, with any numeric columns rounded to the specified amount.
#'
#' @export

roundDF <- function(df, decimals = 2){
  df <- lapply(df, function(y) if(is.numeric(y)) round(y, decimals) else y) %>%
    data.frame()
  rownames(df) <- NULL
  df
}

#' Check if an object is a COIN
#'
#' Returns TRUE if an input object is a COIN, otherwise FALSE if not.
#'
#' @param obj An input object to test
#'
#' @examples
#' # build the ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # check class
#' stopifnot(is.COIN(ASEM))
#'
#' @seealso
#' * [getIn()] Get subset of indicator data from either a COIN or data frame.
#' * [assemble()] Assemble a COIN from indicator data and metadata
#'
#' @return Logical: TRUE if input is a COIN, otherwise FALSE
#'
#' @export

is.COIN <- function(obj){
  inherits(obj, "COIN")
}


#' Print COIN
#'
#' Some details about the COIN
#'
#' @param x A COIN
#' @param ... Arguments to be passed to or from other methods.
#'
#' @examples
#' ASEM <- build_ASEM()
#' print(ASEM)
#'
#' @importFrom utils head
#'
#' @return Text output
#'
#' @export

print.COIN <- function(x, ...){

  COIN <- x

  cat("--------------\n")
  cat("A COIN with...\n")
  cat("--------------\n")
  # Input
  # Units
  firstunits <- paste0(utils::head(COIN$Input$IndData$UnitCode, 3), collapse = ", ")
  if(length(COIN$Input$IndData$UnitCode)>3){
    firstunits <- paste0(firstunits, ", ...")
  }

  # Indicators
  firstinds <- paste0(utils::head(COIN$Input$IndMeta$IndCode, 3), collapse = ", ")
  if(length(COIN$Input$IndMeta$IndCode)>3){
    firstinds <- paste0(firstinds, ", ...")
  }

  # Denominators
  denoms <- names(COIN$Input$Denominators)
  if(!is.null(denoms)){
    denoms <- denoms[startsWith(denoms, "Den_")]
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
  grps <- names(COIN$Input$IndData)[startsWith(names(COIN$Input$IndData), "Group_")]
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
  cat("  Units: ", nrow(COIN$Input$IndData), " (", firstunits, ")\n", sep = "")
  cat(paste0("  Indicators: ", COIN$Parameters$NInd, " (", firstinds, ")\n"))
  cat(paste0("  Denominators: ", ndenom, " (", denoms, ")\n"))
  cat(paste0("  Groups: ", ngrp, " (", grps, ")\n\n"))


  # Structure
  fwk <- COIN$Parameters$Structure

  cat("Structure:\n")
  if(is.null(fwk)){
    fwk <- COIN$Input$IndMeta[
        startsWith(colnames(COIN$Input$IndMeta), c("Agg"))
    ]
    fwk <- cbind(COIN$Input$IndMeta$IndCode, fwk)
  }
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

    if(ii==1){
      cat(paste0("  Level ", ii, ": ", nuniq, " indicators (", first3,") \n"))
    } else {
      cat(paste0("  Level ", ii, ": ", nuniq, " groups (", first3,") \n"))
    }

  }
  cat("\n")

  # Data sets
  cat("Data sets:\n")
  dsets <- names(COIN$Data)
  for(dset in dsets){
    nunit <- nrow(COIN$Data[[dset]])
    cat(paste0("  ", dset, " (", nunit, " units)\n"))
  }
}
