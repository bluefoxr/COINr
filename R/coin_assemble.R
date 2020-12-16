#' Build COIN object
#'
#' This takes the raw data provided by the user and puts it into an list format (COIN object) that is recognised by COINr. It also checks whether there are any syntax errors in the data provided.
#'
#' @param data_raw A dataframe of indicator data.
#' @param metad A dataframe containing auxilliary information for each indicator
#' @param framewk A dataframe specifying the names and weights of each aggregation group
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "select"
#' @importFrom dplyr "starts_with"
#' @importFrom dplyr "ends_with"
#' @importFrom dplyr "n_distinct"
#' @importFrom stats "na.omit"
#'
#' @examples \dontrun{COINobj <- coin_assemble(data_raw, metad, framewk)}
#'
#' @return A "COIN object" (list) formatted to the specifications of COINr. Note that the COIN object is just a tag. It doesn't impose restrictions on the structure of the list.
#'
#' @export

coin_assemble <- function(data_raw, metad, framewk){

  # Extract indicator codes from raw data
  cnames1 <- data_raw %>% dplyr::select(!dplyr::starts_with(
    c("UnitCode", "UnitName", "Year", "Group_","Den_")) ) %>% colnames()

  denoms <- data_raw %>% dplyr::select(dplyr::starts_with(c("UnitCode","Year","Den_"))) # keep denominators to one side for the moment
  data_raw <- data_raw %>% dplyr::select(!dplyr::starts_with("Den_"))


  # Build list
  COINobj <- list(Input = list(
    IndData = data_raw,
    IndMeta = metad,
    AggMeta = framewk,
    Denominators = denoms),
    Data = list(Raw = data_raw), # for various datasets as they emerge (raw, treated, etc.)
    Parameters = NULL, # for various model parameters (will be populated in a min)
    Analysis = NULL, # for analyisis of missing data, correlation etc
    Method = NULL) # a record of the methodology applied to build the index

  # Check that codes in the two tables match, save to list
  if (length(setdiff(cnames1,metad$IndCode)) > 0){

    stop("Indicator codes in metadata table and indicator table are not the same. Please correct.")

  } else {

    message("-----------------")
    message("Indicator codes cross-checked and OK.")
    message("-----------------")
    COINobj$Parameters$NInd <- length(metad$IndCode) # save to list
    COINobj$Parameters$IndCodes <- metad$IndCode # save to list
    COINobj$Parameters$UnitCodes <- dplyr::pull(data_raw, "UnitCode") %>% unique()
    COINobj$Parameters$NUnit <- dplyr::n_distinct(data_raw$UnitCode, na.rm = TRUE)
    message(paste("Number of indicators =",COINobj$Parameters$NInd))
    message(paste("Number of units =",COINobj$Parameters$NUnit))

    if (ncol(dplyr::select(data_raw,dplyr::starts_with("Year"))) > 0){
      message(paste("Number of reference years of data =",
                    dplyr::n_distinct(dplyr::select(data_raw,dplyr::starts_with("Year")))))
      message(paste("Years from",min(dplyr::select(data_raw,starts_with("Year"))),
                    "to", max(dplyr::select(data_raw,dplyr::starts_with("Year")))))
    } else {
      message("No Year column detected in input data. Assuming you only have one year of data.")
    }

  }

  # Check aggregation levels present and say how many
  agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg"))
  n_agg_levels <- length(agg_cols)
  COINobj$Parameters$Nlevels <- n_agg_levels + 1 # add 1 because indicator level not included

  # list with aggregation group names in, for each level
  COINobj$Parameters$AggCodes <- dplyr::select(framewk,dplyr::ends_with("Code")) %>%
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
  agweights <- list(IndWeight = metad$IndWeight)
  # now the other weights
  otherweights <- framewk %>% dplyr::select(dplyr::ends_with("Weight"))
  # join together in one list
  agweights <- c(agweights, as.list(otherweights))
  # we just need to remove NAs
  agweights <- lapply(agweights, function(x) x[!is.na(x)])
  # squirrel away in object
  COINobj$Parameters$Weights <- agweights

  #------- Last bits

  class(COINobj) <- "COIN object" # assigns a "COIN object" class to the list. Helpful for later on.

  return(COINobj)

}
