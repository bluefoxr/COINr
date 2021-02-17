#' Build COIN object
#'
#' This takes the raw data provided by the user and puts it into an list format (COIN object) that is recognised by COINr.
#' It also checks whether there are any syntax errors in the data provided.
#'
#' @param IndData A dataframe of indicator data.
#' @param IndMeta A dataframe containing auxilliary information for each indicator
#' @param AggMeta A dataframe specifying the names and weights of each aggregation group
#' @param include Optional argument specifying a subset of indicator codes to include (default all indicators included)
#' @param exclude Optional argument specifying a subset of indicator codes to exclude (default none excluded)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "select"
#' @importFrom dplyr "starts_with"
#' @importFrom dplyr "ends_with"
#' @importFrom dplyr "n_distinct"
#' @importFrom stats "na.omit"
#'
#' @examples \dontrun{COINobj <- assemble(IndData, IndMeta, AggMeta)}
#'
#' @return A "COIN object" (list) formatted to the specifications of COINr.
#' Note that the COIN object is just a tag. It doesn't impose restrictions on the structure of the list.
#'
#' @export

assemble <- function(IndData, IndMeta, AggMeta, include = NULL, exclude = NULL){

  # Do some checks first - make sure required cols are present
  if(!exists("UnitCode", IndData)){
    stop("No UnitCode column found in IndData. This column is required for assembling a COIM object.")
  }
  if(!exists("UnitName", IndData)){
    stop("No UnitName column found in IndData. This column is required for assembling a COIM object.")
  }
  if(!exists("IndName", IndMeta)){
    stop("No IndName column found in IndMeta. This column is required for assembling a COIM object.")
  }
  if(!exists("IndCode", IndMeta)){
    stop("No IndCode column found in IndMeta. This column is required for assembling a COIM object.")
  }
  if(!exists("Direction", IndMeta)){
    stop("No Direction column found in IndMeta. This column is required for assembling a COIM object.")
  }

  # Extract indicator codes from raw data
  cnames1 <- IndData %>% dplyr::select(!dplyr::starts_with(
    c("UnitCode", "UnitName", "Year", "Group_","Den_", "IndUnit")) ) %>% colnames()

  # In case no indicator cols present
  if(is.null(cnames1)){
    stop("No indicators found. Please check column names.")
  }

  denoms <- IndData %>% dplyr::select(
    dplyr::starts_with(c("UnitCode", "UnitName", "Year", "Group_", "Den_"))) # keep denominators to one side for the moment
  # everything apart from denoms is IndData
  IndData <- IndData %>% dplyr::select(!dplyr::starts_with("Den_"))

  #------- Select indicators, if needed -----

  # if include is specified
  if(!is.null(include)){
    ind_data <- IndData %>% dplyr::select(dplyr::starts_with(
      c("UnitCode", "UnitName", "Year", "Group_", "IndUnit")) & include )
    ind_meta <- IndMeta[IndMeta$IndCode %in% include,]
  } else {
    ind_data <- IndData
    ind_meta <- IndMeta
  }

  # if exclude is specified
  if(!is.null(exclude)){
    ind_data <- ind_data[setdiff(colnames(ind_data), exclude)]
    ind_meta <- ind_meta[!(ind_meta$IndCode %in% exclude),]
  }

  # Build list
  COINobj <- list(Input = list(
    IndData = ind_data,
    IndMeta = ind_meta,
    AggMeta = AggMeta,
    Denominators = denoms,
    Original = list(
      IndData = IndData,
      IndMeta = IndMeta,
      AggMeta = AggMeta
    )),
    Data = list(Raw = ind_data), # for various datasets as they emerge (raw, treated, etc.)
    Parameters = NULL, # for various model parameters (will be populated in a min)
    Analysis = NULL, # for analyisis of missing data, correlation etc
    Method = NULL) # a record of the methodology applied to build the index

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

  # list with aggregation group names in, for each level
  COINobj$Parameters$AggCodes <- dplyr::select(AggMeta,dplyr::ends_with("Code")) %>%
    as.list() %>% lapply(function(x) x[!is.na(x)])

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

  #------- Also get weights and put somewhere sensible

  # first, indicator weights
  agweights <- list(IndWeight = ind_meta$IndWeight)
  # now the other weights
  otherweights <- AggMeta %>% dplyr::select(dplyr::ends_with("Weight"))
  # join together in one list
  agweights <- c(agweights, as.list(otherweights))
  # we just need to remove NAs
  agweights <- lapply(agweights, function(x) x[!is.na(x)])
  # squirrel away in object
  COINobj$Parameters$Weights <- agweights

  #------- Create a lookup dictionary for codes <--> names

  # all agg codes in one column
  aggcodes <- dplyr::select(AggMeta, dplyr::ends_with("Code")) %>%
    as.matrix() %>% c()
  aggcodes <- aggcodes[!is.na(aggcodes)]
  # all agg names in one column
  aggnames <- dplyr::select(AggMeta, dplyr::ends_with("Name")) %>%
    as.matrix() %>% c()
  aggnames <- aggnames[!is.na(aggnames)]

  # this is like a lookup table of all indicator/agg codes and names
  COINobj$Parameters$Code2Name <- rbind(
    cbind(ind_meta$IndCode, ind_meta$IndName),
    cbind(aggcodes, aggnames) ) %>% as.data.frame()
  colnames(COINobj$Parameters$Code2Name) <- c("AggCode", "AggName")

  #------- Last bits

  # record inclusion/exclusion choices
  COINobj$Method$Assemble$include <- include
  COINobj$Method$Assemble$exclude <- exclude

  class(COINobj) <- "COIN object" # assigns a "COIN object" class to the list. Helpful for later on.

  return(COINobj)

}
