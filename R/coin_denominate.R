#' Denominate indicator data sets
#'
#' Indicators can be denominated (divided) by other external indicators. Typically, the aim here is to convert extensive (size-related) variables into intensive variables (comparable between units of different sizes).
#'
#' @param COINobj COIN object (data frames are not currently supported in this function)
#' @param specby Selects the source of the specifications for denomination. If "metadata", uses the denominator column in .$metadata. If "user", takes a character vector of denominator codes (one for each indicator, with NA for indicators that should not be denominated, and in the same order as the indicators).
#' @param denomby Character vector specifying which indicators to use as denominators. For indicators with no denomination, set elements to NA.
#'
#' @examples \dontrun{
#' COINobj <- coin_denominate(COINobj, specby="metadat", denomby = NULL)}
#'
#' @return An updated COIN object, with new dataset .$Data$Denominated of denominated indicators.
#'
#' @export

coin_denominate <- function(COINobj, specby = "metadata", denomby = NULL){

  if (!("COIN object" %in% class(COINobj))){stop("This function only supports COIN object as an input.")}

  ind_names <- COINobj$Parameters$IndCodes

  if (specby == "metadata"){ # use the metadata table to specify which indicators to use for denomination

    denoms <- cbind(COINobj$Input$Denominators,"Ones"=1) # get denominator cols and add a dummy column of ones
    den_spec <- COINobj$Input$IndMeta$Denominator %>% replace(is.na(COINobj$Input$IndMeta$Denominator),"Ones") # the vector specifying which denominators to use. Replace NAs with "Ones"
    denomtrix <- denoms[den_spec] # build data frame, same size as indicator data frame, with corresponding denominator columns
    data_denom <- COINobj$Data$Raw # make a copy just to be safe
    data_denom[ind_names] <- COINobj$Data$Raw[ind_names]/denomtrix # divide one by the other to give denominated data.

  } else if (specby == "user"){

    denoms <- cbind(COINobj$Input$Denominators,"Ones"=1) # get denominator cols and add a dummy column of ones
    den_spec <- denomby %>% replace(is.na(denomby),"Ones") # the vector specifying which denominators to use. Replace NAs with "Ones"
    denomtrix <- denoms[den_spec] # build data frame, same size as indicator data frame, with corresponding denominator columns
    data_denom <- COINobj$Data$Raw # make a copy just to be safe
    data_denom[ind_names] <- COINobj$Data$Raw[ind_names]/denomtrix # divide one by the other to give denominated data.

  }

  COINobj$Data$Denominated <- data_denom

  # Record to Method
  COINobj$Method$Denomination$specby <- specby
  COINobj$Method$Denomination$denomby <- denomby

  return(COINobj)
}
