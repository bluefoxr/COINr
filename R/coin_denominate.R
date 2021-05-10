#' Denominate indicator data sets
#'
#' Indicators can be denominated (divided) by other external indicators.
#' Typically, the aim here is to convert extensive (size-related) variables into intensive variables
#' (comparable between units of different sizes).
#'
#' @param obj COIN object (data frames are not currently supported in this function)
#' @param dset The data set to denominate (only if COIN used as input)
#' @param specby Selects the source of the specifications for denomination.
#' If "metadata", uses the denominator column in .$metadata.
#' If "user", takes a character vector of denominator codes (one for each indicator, with NA for indicators that should not be denominated, and in the same order as the indicators).
#' @param denomby Character vector specifying which denominators to use for each indicator. Only used if
#' specby = "user". For indicators with no denomination, set elements to NA. Elements must be column names
#' of denominators.
#' @param denominators A data frame of denominator data
#' @param out2 Where to output the results. If "COIN" (default for COIN input), appends to updated COIN,
#' otherwise if "df" outputs to data frame.
#'
#' @examples \dontrun{
#' COIN <- denominate(COIN, specby="metadata", denomby = NULL)}
#'
#' @return An updated COIN object, with new dataset .$Data$Denominated of denominated indicators.
#'
#' @export

denominate <- function(obj, dset = "Raw", specby = "metadata", denomby = NULL, denominators = NULL,
                       out2 = "COIN"){

  # run through object check
  out1 <- getIn(obj, dset = dset)

  # output to object if requested
  if(out1$otype=="COINobj") {

    # Record to Method
    obj$Method$denominate$dset <- dset
    obj$Method$denominate$specby <- specby
    obj$Method$denominate$denomby <- denomby
    obj$Method$denominate$denominators <- denominators
  }

  # some checks first
  if( ("data.frame" %in% class(obj)) & (is.null(denomby)|is.null(denominators)) ){
    stop("If data frame is input, you need to specify both denomby and denominators.")
  }

  # get denominator data frame and indicator data frame
  if (("COIN object" %in% class(obj)) & is.null(denominators)){
    # if input is COIN and no denominators are specified, look in COIN
    dfDenoms <- cbind(obj$Input$Denominators,"Ones"=1)
  } else {
    # if it passed the checks above, should be a df with denoms present
    dfDenoms <- cbind(denominators,"Ones"=1)
  }

  # get the data set to denominate
  data_denom <- out1$ind_data

  # the vector specifying which denominators to use. Replace NAs with "Ones"
  if ((specby == "metadata") & (out1$otype == "COINobj")){
    # only spec by metadata if COIN and specby = metadata
    den_spec <- obj$Input$IndMeta$Denominator %>% replace(is.na(obj$Input$IndMeta$Denominator),"Ones")
  } else {
    # anything else, look for specs in function argument
    den_spec <- denomby %>% replace(is.na(denomby),"Ones")
  }

  # build data frame, same size as indicator data frame, with corresponding denominator columns
  denomtrix <- dfDenoms[den_spec]

  # divide one by the other to give denominated data.
  data_denom[out1$IndCodes] <- data_denom[out1$IndCodes]/denomtrix

  # output to object if requested
  if( (out1$otype=="COINobj") & (out2 !=  "df") ) {

    obj$Data$Denominated <- data_denom
    return(obj)

  } else {
    return(data_denom)
  }
}
