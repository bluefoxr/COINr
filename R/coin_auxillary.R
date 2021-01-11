#' Format check auxiliary function
#'
#' Checks whether function input is COIN object or data frame, and returns input useful to most COINr functions.
#' The purpose is that most COINr functions are able to take either a COIN object or data frame as input.
#' Also includes subsetting of indicator data using "inames", which also works by referring to aggregate groups.
#'
#' @param COINobj Can be either the COIN object, or a data frame.
#' @param dset If input is a COIN object, this specifies which data set in .$Data to use.
#' @param inames An optional character vector of indicator codes to subset the indicator data. Usefully, can also refer to
#' an aggregation group name, and data will be subsetted accordingly. NOTE does not work with multiple aggregate group names.
#'
#' @importFrom magrittr extract
#' @importFrom dplyr select starts_with
#'
#' @examples \dontrun{out <- coin_aux_objcheck(COINobj, dset = "raw", inames = NULL)}
#'
#' @return A list with .$ind_names is a vector with indicator names,
#' .$ind_data is a data frame /tibble with indicator data, plus possibly grouping/denominator columns, etc.
#' .$ind_data_only is a data frame/tibble with only indicator data columns (no names, groups, etc)
#' .$otype is a string, either "COINobj" or "df" describing the input object type
#'
#' @export

coin_aux_objcheck <- function(COINobj, dset = "Raw", inames = NULL){

  # Check to see what kind of input we have.
  if ("COIN object" %in% class(COINobj)){ # COIN obj

    otype <- "COINobj"

    # Select data set to use
    if (dset=="Raw"){
      ind_data <- COINobj$Data$Raw # get raw indicator data
    } else  if (dset=="Denominated"){
      ind_data <- COINobj$Data$Denominated # get denominated indicator data
    } else  if (dset=="Imputed"){
      ind_data <- COINobj$Data$Imputed # get imputed indicator data
    } else  if (dset=="Normalised"){
      ind_data <- COINobj$Data$Normalised # get normalised indicator data
    } else  if (dset=="Treated"){
      ind_data <- COINobj$Data$Treated # get treated indicator data
    } else  if (dset=="Aggregated"){
      ind_data <- COINobj$Data$Aggregated # get treated indicator data
    } else {
      stop("dset name not recognised...")
    }

    # get unit codes. Have to do this because if the units have been screened, then it may
    # not be the same set as when the data was input.
    UnitCodes <- ind_data$UnitCode

    if (is.null(inames)){
      if (dset=="Aggregated"){
        # the aggregated data set includes extra names/columns, i.e. the aggregated groups.
        # Have to extract these as well (e.g. for use in indicator_dash)
        ind_names <- ind_data %>% dplyr::select(!dplyr::starts_with(
          c("UnitCode", "UnitName", "Year", "Group_","Den_")) ) %>% colnames()
      } else {
        # default: use all indicators
        ind_names <- COINobj$Parameters$IndCodes
      }
    } else if (length(inames)==1 &
               # brutish thing here which checks in the list of AggCodes to see if inames string is present
               any(as.logical(lapply(COINobj$Parameters$AggCodes, function(x) any(x == inames))))
    ){
      # if only one, check if this is a reference to an aggregation group
      # then find which level this belongs to.
      # First, get only aggregation columns in IndMeta
      aggcols <- COINobj$Input$IndMeta %>% select(starts_with("Agg"))
      # The col with the named group in it
      aggcol <- aggcols %>% colnames() %>% magrittr::extract(grepl(inames, aggcols))
      # then get the indicators belonging to this group
      ind_names <- COINobj$Input$IndMeta %>%
        dplyr::filter(!!as.symbol(aggcol) == inames) %>%
        dplyr::pull("IndCode")
    } else {
      # otherwise, use the indicator names here
      ind_names <- inames
    }

  } else if (is.data.frame(COINobj)){ # Data frame

    otype <- "df"

    ind_data <- COINobj

    if (is.null(inames)){
      if (exists("UnitCode",ind_data)){
        # If there are unit codes, record them and assume all other cols are indicator names
        ind_names <- COINobj[colnames(COINobj) != "UnitCode"] %>% colnames()
        UnitCodes <- COINobj$UnitCode
      } else {
        # All cols are indicators. No names supplied.
        ind_names <- colnames(COINobj)
        UnitCodes <- NA
      }
    } else {
      ind_names <- inames
      if (exists("UnitCode",ind_data)){
        UnitCodes <- COINobj$UnitCode
      } else {
        UnitCodes <- NA
      }
    }



  } else {
    stop("Input should either be COIN object or data frame.")
  }

  out <- list(ind_names = ind_names,
              ind_data = ind_data,
              ind_data_only = select(ind_data,all_of(ind_names),),
              UnitCodes = UnitCodes,
              otype = otype # the object type
              )

  return(out)
}
