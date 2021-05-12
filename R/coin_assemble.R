#' Build COIN object
#'
#' This takes the raw data provided by the user and puts it into an list format (COIN object) that is recognised by COINr.
#' It also checks whether there are any syntax errors in the data provided. Optionally, you can exclude
#' or include indicators using the "include" and "exclude" arguments. Note that if an indicator is specified in
#' BOTH include and exclude, it will be excluded.
#'
#' @param IndData A dataframe of indicator data.
#' @param IndMeta A dataframe containing auxilliary information for each indicator
#' @param AggMeta A dataframe specifying the names and weights of each aggregation group
#' @param include Optional argument specifying a subset of indicator codes to include (default all indicators included)
#' @param exclude Optional argument specifying a subset of indicator codes to exclude (default none excluded)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "select"
#' @importFrom dplyr "arrange"
#' @importFrom dplyr "across"
#' @importFrom dplyr "starts_with"
#' @importFrom dplyr "ends_with"
#' @importFrom dplyr "n_distinct"
#' @importFrom purrr "map_lgl"
#' @importFrom stats "na.omit"
#'
#' @examples \dontrun{COINobj <- assemble(IndData, IndMeta, AggMeta)}
#'
#' @return A "COIN object" (list) formatted to the specifications of COINr.
#' Note that the COIN object is just a tag. It doesn't impose restrictions on the structure of the list.
#'
#' @export

assemble <- function(IndData, IndMeta, AggMeta, include = NULL, exclude = NULL){

  ##----- INITIAL CHECKS -----##

  # Do some checks first - make sure required cols are present
  if(!exists("UnitCode", IndData)){
    stop("No UnitCode column found in IndData. This column is required for assembling a COIN object.")
  }
  if(!exists("UnitName", IndData)){
    stop("No UnitName column found in IndData. This column is required for assembling a COIN object.")
  }
  if(!exists("IndName", IndMeta)){
    stop("No IndName column found in IndMeta. This column is required for assembling a COIN object.")
  }
  if(!exists("IndCode", IndMeta)){
    stop("No IndCode column found in IndMeta. This column is required for assembling a COIN object.")
  }
  if(!exists("Direction", IndMeta)){
    stop("No Direction column found in IndMeta. This column is required for assembling a COIN object.")
  }

  ##----- IND CODES AND DENOMS -----##

  # copy ind data before going any further. Used in .$Input$Original
  IndDataOrig <- IndData

  # Extract indicator codes from raw data
  cnames1 <- IndData %>% dplyr::select(!dplyr::starts_with(
    c("UnitCode", "UnitName", "Year", "Group_","Den_", "IndUnit", "x_")) ) %>% colnames()

  # check for any non-numeric cols and stop if any present
  ind_data_only <- IndData[cnames1]
  not_num <- cnames1[!purrr::map_lgl(ind_data_only, is.numeric)]
  if(length(not_num)>0){
    # stop, print any non-numeric
    message("Non-numerical columns in IndData (probably character/text?):")
    print(not_num)
    stop(paste0("Non-numeric columns detected. Only numerical indicators are allowed"))
  }

  # In case no indicator cols present
  if(is.null(cnames1)){
    stop("No indicators found. Please check column names.")
  }

  # everything apart from denoms is IndData
  IndData1 <- IndData %>% dplyr::select(!dplyr::starts_with("Den_"))

  # if the everything-apart-from-denoms is diff from the original data, means there are denoms
  # So, extract denoms
  message("-----------------")
  if(ncol(IndData1) < ncol(IndData)){
    denoms <- IndData %>% dplyr::select(
      dplyr::starts_with(c("UnitCode", "UnitName", "Year", "Group_", "Den_", "x_"))) # keep denominators to one side for the moment
    message("Denominators detected - stored in .$Input$Denominators")
  } else {
    denoms <- NULL # this will have the effect of not attaching to the list
    message("No denominators detected.")
  }
  message("-----------------")

  # from this point, IndData is minus any denominators
  IndData <- IndData1


  ##------- Select indicators, if needed -----##

  include0 <- include
  exclude0 <- exclude

  # if include is not specified, include everything
  if(is.null(include)){include <- cnames1}

  # the vector of indicators to include is everything in include, minus anything in exclude
  include <- setdiff(include,exclude)

  # select data and metadata accordingly
  if(!is.null(include)){
    ind_data <- IndData %>% dplyr::select(dplyr::starts_with(
      c("UnitCode", "UnitName", "Year", "Group_", "IndUnit", "x_")), include )
    ind_meta <- IndMeta[IndMeta$IndCode %in% include,]
  }

  # sort ind_meta properly according to structure of index
  # note: I sort according to the reversed order of the agg columns, so starting with the
  # highest level of aggregation first then working down

  ind_meta <- dplyr::arrange(ind_meta,
                             rev(dplyr::across(dplyr::starts_with("Agg"))))

  # I want the indicator cols to be in the same order as ind_meta, to avoid surprises
  ind_data <- dplyr::select(ind_data,
                            dplyr::starts_with(c("UnitCode", "UnitName",
                                                 "Year", "Group_", "IndUnit", "x_")), ind_meta$IndCode)

  # Build list
  COINobj <- list(Input = list(
    IndData = ind_data,
    IndMeta = ind_meta,
    AggMeta = AggMeta,
    Original = list(
      IndData = IndDataOrig,
      IndMeta = IndMeta,
      AggMeta = AggMeta
    )),
    Data = list(Raw = ind_data), # for various datasets as they emerge (raw, treated, etc.)
    Parameters = NULL, # for various model parameters (will be populated in a min)
    Analysis = NULL, # for analyisis of missing data, correlation etc
    Method = NULL) # a record of the methodology applied to build the index

  # add denoms separately - this is because if it is NULL it will disappear
  COINobj$Input$Denominators = denoms

  # Check that codes in the two tables match, save to list
  if (length(setdiff(cnames1,IndMeta$IndCode)) > 0){

    stop("Indicator codes in metadata table and indicator table are not the same. Please correct.")

  } else {

    message("-----------------")
    message("Indicator codes cross-checked and OK.")
    message("-----------------")
    COINobj$Parameters$NInd <- length(ind_meta$IndCode) # save to list
    COINobj$Parameters$IndCodes <- ind_meta$IndCode # save to list
    COINobj$Parameters$UnitCodes <- dplyr::pull(ind_data, "UnitCode") %>% unique()
    COINobj$Parameters$NUnit <- dplyr::n_distinct(ind_data$UnitCode, na.rm = TRUE)
    message(paste("Number of indicators =",COINobj$Parameters$NInd))
    message(paste("Number of units =",COINobj$Parameters$NUnit))

    if (ncol(dplyr::select(ind_data,dplyr::starts_with("Year"))) > 0){
      message(paste("Number of reference years of data =",
                    dplyr::n_distinct(dplyr::select(ind_data,dplyr::starts_with("Year")))))
      message(paste("Years from",min(dplyr::select(ind_data,starts_with("Year"))),
                    "to", max(dplyr::select(ind_data,dplyr::starts_with("Year")))))
    } else {
      message("No Year column detected in input data. Assuming you only have one year of data.")
    }

  }

  # Check aggregation levels present and say how many
  agg_cols <- ind_meta %>% dplyr::select(dplyr::starts_with("Agg"))
  # In case no aggregation columns present
  if(is.null(agg_cols)){
    stop("No aggregation columns detected in IndMeta.")
  }
  n_agg_levels <- length(agg_cols)
  COINobj$Parameters$Nlevels <- n_agg_levels + 1 # add 1 because indicator level not included

  # # list with aggregation group names in, for each level
  AggCodeslist <- vector(mode = "list", length = n_agg_levels)
  for (ii in 1:n_agg_levels){
    AggCodeslist[[ii]] <- AggMeta$Code[AggMeta$AgLevel==ii+1]
  }
  COINobj$Parameters$AggCodes <- AggCodeslist

  if (n_agg_levels > 0){

    message(paste("Number of aggregation levels =", n_agg_levels, "above indicator level."))
    message("-----------------")

    # Loop through aggregation levels. Get names of agg groups and store, plus print to console.
    for (agg_no in 1:n_agg_levels){

      agg_names <- COINobj$Parameters$AggCodes[[agg_no]]
      n_agg_groups <- length(agg_names)
      message(paste("Aggregation level",agg_no,"with",n_agg_groups,"aggregate groups:",paste0(agg_names, collapse=", ")))

      agg_names2 <- unique(agg_cols[[agg_no]]) # cross check to see if agg names match

      if (length(setdiff(agg_names,agg_names2)) > 0){
        stop("Aggregation codes in framework are not consistent with metadata")
      }  else {
        message("Cross-check between metadata and framework = OK.")
      }

    }

  } else {

    warning("No aggregation levels were detected.")

  }

  message("-----------------")

  #------- Also get weights for all levels
  agg_cols <- ind_meta %>% dplyr::select(dplyr::starts_with("Agg"))
  n_agg_levels <- length(agg_cols)

  agweights <- data.frame(AgLevel = 1,
                          Code = ind_meta$IndCode,
                          Weight = ind_meta$IndWeight)

  agweights <- rbind(agweights,
                     AggMeta[c("AgLevel", "Code", "Weight")])

  # squirrel away in object
  COINobj$Parameters$Weights$Original <- agweights

  #------- Create a lookup dictionary for codes <--> names
  # this is like a lookup table of all indicator/agg codes and names
  COINobj$Parameters$Code2Name <- rbind(
    cbind(ind_meta$IndCode, ind_meta$IndName),
    cbind(AggMeta$Code, AggMeta$Name) ) %>% as.data.frame()
  colnames(COINobj$Parameters$Code2Name) <- c("AggCode", "AggName")

  #------- Last bits

  # record inclusion/exclusion choices
  COINobj$Method$assemble$include <- include0
  COINobj$Method$assemble$exclude <- exclude0

  class(COINobj) <- "COIN object" # assigns a "COIN object" class to the list. Helpful for later on.

  return(COINobj)

}
