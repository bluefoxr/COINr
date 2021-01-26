#' Build COIN object
#'
#' This takes the raw data provided by the user and puts it into an list format (COIN object) that is recognised by COINr. It also checks whether there are any syntax errors in the data provided.
#'
#' @param IndData A dataframe of indicator data.
#' @param IndMeta A dataframe containing auxilliary information for each indicator
#' @param AggMeta A dataframe specifying the names and weights of each aggregation group
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "select"
#' @importFrom dplyr "starts_with"
#' @importFrom dplyr "ends_with"
#' @importFrom dplyr "n_distinct"
#' @importFrom stats "na.omit"
#'
#' @examples \dontrun{COINobj <- coin_assemble(IndData, IndMeta, AggMeta)}
#'
#' @return A "COIN object" (list) formatted to the specifications of COINr. Note that the COIN object is just a tag. It doesn't impose restrictions on the structure of the list.
#'
#' @export

coin_assemble <- function(IndData, IndMeta, AggMeta){

  # Extract indicator codes from raw data
  cnames1 <- IndData %>% dplyr::select(!dplyr::starts_with(
    c("UnitCode", "UnitName", "Year", "Group_","Den_")) ) %>% colnames()

  denoms <- IndData %>% dplyr::select(dplyr::starts_with(c("UnitCode","Year","Den_"))) # keep denominators to one side for the moment
  IndData <- IndData %>% dplyr::select(!dplyr::starts_with("Den_"))


  # Build list
  COINobj <- list(Input = list(
    IndData = IndData,
    IndMeta = IndMeta,
    AggMeta = AggMeta,
    Denominators = denoms),
    Data = list(Raw = IndData), # for various datasets as they emerge (raw, treated, etc.)
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
    COINobj$Parameters$NInd <- length(IndMeta$IndCode) # save to list
    COINobj$Parameters$IndCodes <- IndMeta$IndCode # save to list
    COINobj$Parameters$UnitCodes <- dplyr::pull(IndData, "UnitCode") %>% unique()
    COINobj$Parameters$NUnit <- dplyr::n_distinct(IndData$UnitCode, na.rm = TRUE)
    message(paste("Number of indicators =",COINobj$Parameters$NInd))
    message(paste("Number of units =",COINobj$Parameters$NUnit))

    if (ncol(dplyr::select(IndData,dplyr::starts_with("Year"))) > 0){
      message(paste("Number of reference years of data =",
                    dplyr::n_distinct(dplyr::select(IndData,dplyr::starts_with("Year")))))
      message(paste("Years from",min(dplyr::select(IndData,starts_with("Year"))),
                    "to", max(dplyr::select(IndData,dplyr::starts_with("Year")))))
    } else {
      message("No Year column detected in input data. Assuming you only have one year of data.")
    }

  }

  # Check aggregation levels present and say how many
  agg_cols <- IndMeta %>% dplyr::select(dplyr::starts_with("Agg"))
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
  agweights <- list(IndWeight = IndMeta$IndWeight)
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
  COINobj$Parameters$Code2Name <- rbind(cbind(ASEM$Input$IndMeta$IndCode,
                            ASEM$Input$IndMeta$IndName),
                      cbind(aggcodes, aggnames) ) %>% as.data.frame()
  colnames(COINobj$Parameters$Code2Name) <- c("AggCode", "AggName")

  #------- Last bits

  class(COINobj) <- "COIN object" # assigns a "COIN object" class to the list. Helpful for later on.

  return(COINobj)

}
