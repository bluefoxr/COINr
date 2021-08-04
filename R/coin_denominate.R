#' Denominate indicator data sets
#'
#' Indicators can be denominated (divided) by other external indicators.
#' Typically, the aim here is to convert extensive (size-related) variables into intensive variables
#' (comparable between units of different sizes).
#'
#' @param obj COIN object or a data frame of indicator data to be denominated. If a data frame, must include a UnitCode column.
#' @param dset The data set to denominate (only if COIN used as input)
#' @param specby Selects the source of the specifications for denomination.
#' If "metadata", uses the denominator column in .$metadata.
#' If "user", takes a character vector of denominator codes (one for each indicator, with NA for indicators that should not be denominated, and in the same order as the indicators).
#' @param denomby Character vector specifying which denominators to use for each indicator. Only used if
#' specby = "user". For indicators with no denomination, set elements to NA. Elements must be column names
#' of denominators.
#' @param scaledenoms This allows the possibility to scale denominators if needed. For example, if GDP is a denominator and is meausured in
#' dollars, dividing will create very small numbers (order 1e-10 and smaller) which could cause problems with numerical precision. This should be
#' a named list of the form e.g. `list(Den_GDP = 1e-9)`, where the name is the denominator to be scaled, and the entry is a factor to multiply
#' the denominator values by. In the example, this would multiply GDP values by 1e-9, which (if the original values are in dollars) would
#' scale them to billions of dollars. The list can include more than one entry, corresponding to any denominators that are present.
#' @param denominators A data frame of denominator data. Columns should be denominator data, with column names corresponding
#' to entries in denomby. This must also include a UnitCode column to match units (ordering is unimportant, this is done inside the function).
#' Ensure that the unit codes correspond to the unit codes in the indicator data.
#' @param out2 Where to output the results. If "COIN" (default for COIN input), appends to updated COIN,
#' otherwise if "df" outputs to data frame.
#'
#' @examples \dontrun{
#' COIN <- denominate(COIN, specby="metadata", denomby = NULL)}
#'
#' @return An updated COIN object, with new dataset .$Data$Denominated of denominated indicators.
#'
#' @export

denominate <- function(obj, dset = NULL, specby = NULL, denomby = NULL, scaledenoms = NULL, denominators = NULL,
                       out2 = "COIN"){

  # Check for dset. If not specified, exit.
  if (is.null(dset) & !("data.frame" %in% class(obj)) ){
    stop("dset is NULL. Please specify which data set to operate on.")
  }

  # reset defaults - sometimes this can go wrong during regen when the inputs are called from Method and are forced to be NULL
  # (if not present), which causes an error.
  if(is.null(specby)){
    specby <- "metadata"
  }
  if(is.null(out2)){
    out2 <- "COIN"
  }

  # run through object check
  out1 <- getIn(obj, dset = dset)

  # output to object if requested
  if(out1$otype=="COINobj") {

    # Record to Method
    obj$Method$denominate$dset <- dset
    obj$Method$denominate$specby <- specby
    obj$Method$denominate$denomby <- denomby
    obj$Method$denominate$scaledenoms <- scaledenoms
    obj$Method$denominate$denominators <- denominators
  }

  # some checks first
  if( ("data.frame" %in% class(obj)) & (is.null(denomby)|is.null(denominators)) ){
    stop("If data frame is input, you need to specify both denomby and denominators.")
  }

  # get denominator data frame and indicator data frame
  if (("COIN" %in% class(obj)) & is.null(denominators)){
    # if input is COIN and no denominators are specified, look in COIN
    dfDenoms <- cbind(obj$Input$Denominators,"Ones"=1)
  } else {
    # if it passed the checks above, should be a df with denoms present
    dfDenoms <- cbind(denominators,"Ones"=1)
  }

  # scale denominators if required
  if(!is.null(scaledenoms)){
    # check list
    if(!is.list(scaledenoms)){stop("scaledenoms is not a list")}
    for (ii in 1:length(scaledenoms)){

      # get name and value, and do some checks
      den_name <- names(scaledenoms)[ii]
      if(is.null(den_name)){stop("No name found for an entry in scaledenoms. This must be a named list.")}
      if(!(den_name %in% colnames(dfDenoms))){stop("A denominator name in scaledenoms is not found in the denominator data.")}
      den_fac <- scaledenoms[[ii]]
      if(!is.numeric(den_fac)){stop("Scale factor is not numeric in scaledenoms.")}
      if(length(den_fac)>1){stop("Scale factor is a vector, this should be a single number.")}

      # scale
      dfDenoms[den_name] <- dfDenoms[den_name]*den_fac
    }
  }

  # make sure UnitCode col exists
  if(is.null(dfDenoms$UnitCode)){
    stop("UnitCode column not found in data frame of denominators. This is required to correctly match rows with indicator data.")
  }

  # make sure Unit Codes match up
  # first, we only include rows (units) from the denoms that are present in the data set to be denominated
  dfDenoms <- dfDenoms[dfDenoms$UnitCode %in% out1$UnitCodes, ]

  # now check that the two sets of unit codes are equal
  if(!setequal(dfDenoms$UnitCode, out1$UnitCodes)){
    stop("Unit codes do not match between indicator data and denominator data.")
  }

  # get the data set to denominate
  data_denom <- out1$ind_data
  # make sure UnitCode col exists
  if(is.null(data_denom$UnitCode)){
    stop("UnitCode column not found in indicator data. This is required to correctly match rows with indicator data.")
  }

  # the vector specifying which denominators to use. Replace NAs with "Ones"
  if ((specby == "metadata") & (out1$otype == "COINobj")){
    # only spec by metadata if COIN and specby = metadata
    den_spec <- obj$Input$IndMeta$Denominator %>% replace(is.na(obj$Input$IndMeta$Denominator),"Ones")
  } else {
    # anything else, look for specs in function argument
    den_spec <- denomby %>% replace(is.na(denomby),"Ones")
  }

  if(any(!(den_spec %in% colnames(dfDenoms)))){
    stop("Codes used to specify denominators not found in denominator codes. Check IndMeta or denomby and make sure it corresponds to the column names of the denominator data.")
  }

  # build data frame, same size as indicator data frame, with corresponding denominator columns
  denomtrix <- dfDenoms[c("UnitCode",den_spec)]

  # we need to make sure the two data frames match the unit codes
  # match the order of denomtrix with the data to denominate, using UnitCode col
  # we also remove the UnitCode col again
  denomtrix <- denomtrix[match(data_denom$UnitCode, denomtrix$UnitCode),
                         colnames(denomtrix) != "UnitCode"]

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
