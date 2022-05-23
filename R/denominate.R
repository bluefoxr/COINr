# DENOMINATION TOOLS

#' Denominate a data set within a purse.
#'
#' This works in almost exactly the same way as [Denominate.coin()]. The only point of care is that the
#' `denoms` argument here cannot take time-indexed data, but only a single value for each unit. It is
#' therefore recommended to pass the time-dependent denominator data as part of `iData` when calling
#' [new_coin()]. In this way, denominators can vary with time. See `vignette("denomination")`.
#'
#' @param x A purse class object
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param denoms A data frame of denominator data. If not specified, will extract any potential denominator columns
#' that were attached to `iData` when calling [new_coin()].
#' @param denomby Optional data frame specifying which indicators should be denominated and by what. If not specified,
#' looks for a Denominator column in iMeta.
#' @param denoms_ID An ID column for matching `denoms` with the data to be denominated. This column should contain
#' uMeta codes to match with the data set extracted from the coin.
#' @param f_denom A function which takes two numeric vector arguments and is used to perform the denomination for each
#' column. By default, this is division, i.e. `x[[col]]/denoms[[col]]` for given columns, but any function can be passed
#' that takes two numeric vectors as inputs and returns a single numeric vector. See details.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Denominated"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse
#' @export
#'
#' @examples
#' #
Denominate.purse <- function(x, dset, denoms = NULL, denomby = NULL, denoms_ID = NULL,
                              f_denom = NULL, write_to = NULL, ...){

  # input check
  check_purse(x)

  # apply denomination to each coin
  x$coin <- lapply(x$coin, function(coin){
    Denominate.coin(coin, dset = dset, denoms = denoms, denomby = denomby,
                     denoms_ID = denoms_ID, f_denom = f_denom, write_to = write_to, out2 = "coin")
  })
  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}


#' Denominate data set in a coin
#'
#' @param x A coin class object
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param denoms A data frame of denominator data. If not specified, will extract any potential denominator columns
#' that were attached to `iData` when calling [new_coin()].
#' @param denomby Optional data frame specifying which indicators should be denominated and by what. If not specified,
#' looks for a "Denominator" column in `iMeta`.
#' @param denoms_ID An ID column for matching `denoms` with the data to be denominated. This column should contain
#' `uMeta` codes to match with the data set extracted from the coin.
#' @param f_denom A function which takes two numeric vector arguments and is used to perform the denomination for each
#' column. By default, this is division, i.e. `x[[col]]/denoms[[col]]` for given columns, but any function can be passed
#' that takes two numeric vectors as inputs and returns a single numeric vector. See details.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Denominated"`.
#' @param out2 Either `"coin"` (default) to return updated coin or `"df"` to output the aggregated data set.
#' @param ... arguments passed to or from other methods
#'
#' @return An updated coin if `out2 = "coin"`, else a data frame of denominated data if `out2 = "df"`.
#' @export
#'
#' @examples
#' #
Denominate.coin <- function(x, dset, denoms = NULL, denomby = NULL, denoms_ID = NULL,
                            f_denom = NULL, write_to = NULL, out2 = "coin", ...){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")

  # GET DSET, CHECKS --------------------------------------------------------

  iData <- get_dset(coin, dset)

  # DEFAULTS ----------------------------------------------------------------

  if(is.null(denoms)){
    denoms <- coin$Meta$Unit
  }

  if(is.null(denomby)){
    if("Denominator" %nin% colnames(coin$Meta$Ind)){
      stop("No Denominator column found in iMeta. Please supply denomby argument.")
    }
    denomby <- coin$Meta$Ind[c("iCode", "Denominator")]
    denomby <- denomby[!is.na(denomby$Denominator), ]
  }

  # DENOMINATE --------------------------------------------------------------

  iData_d <- Denominate(iData, denoms = denoms, denomby = denomby,
                         denoms_ID = denoms_ID, f_denom = f_denom)

  # Output ------------------------------------------------------------------

  # output list
  if(out2 == "df"){
    iData_d
  } else {
    if(is.null(write_to)){
      write_to <- "Denominated"
    }
    write_dset(coin, iData_d, dset = write_to)
  }
}


#' Denominate data sets by other variables
#'
#' "Denominates" or "scales" variables by other variables. Typically this is done by dividing extensive variables such as
#' GDP by a scaling variable such as population, to give an intensive variable (GDP per capita).
#'
#' A data frame `x` is denominated by variables found in another data frame `denoms`, according to specifications in
#' `denomby`. `denomby` specifies which columns in `x` are to be denominated, and by which columns in `denoms`, and
#' any scaling factors to apply to each denomination.
#'
#' Both `x` and `denomby` must contain an ID column which matches the rows of `x` to `denomby`. If not specified, this
#' is assumed to be `uCode`,  but can also be specified using the `x_ID` and `denoms_ID` arguments. All entries in
#' `x[[x_ID]]` must be present in `denoms[[denoms_ID]]`, although extra rows are allowed in `denoms`. This is because
#' the rows of `x` are matched to the rows of `denoms` using these ID columns, to ensure that units (rows) are correctly
#' denominated.
#'
#' By default, columns of `x` are divided by columns of `denoms`. This can be generalised by setting `f_denom` to another
#' function which takes two numeric vector arguments. I.e. setting `denoms = ``*`` ` will multiply columns of `x` and
#' denoms together.
#'
#' @param x A data frame of data to be denominated. Columns to be denominated must be numeric, but any columns not
#' specified in `denomby` will be ignored. `x` must also contain an ID column specified by `x_ID` to match rows with
#' `denoms`.
#' @param denoms A data frame of denominator data. Columns should be denominator data, with column names corresponding
#' to entries in `denomby`. This must also include an ID column idenfied by `denoms_ID` to match rows.
#' @param denomby A data frame which specifies which denominators to use for each indicator, and any scaling factors
#' to apply. Should have columns `iCode`, `Denominator`, `ScaleFactor`. `iCode` specifies a column name from `x`,
#' `Denominator` specifies a column name from `denoms` to use to denominate the corresponding column from `x`.
#' `ScaleFactor` allows the possibility to scale
#' denominators if needed, and specifies a factor to multiply the resulting values by. For example, if GDP is a denominator and is measured in
#' dollars, dividing will create very small numbers (order 1e-10 and smaller) which could cause problems with numerical precision.
#' @param x_ID A column name of `x` to use to match rows with `denoms`. Default is `"uCode"`.
#' @param denoms_ID A column name of `denoms` to use to match rows with `x`. Default is `"uCode"`.
#' @param f_denom A function which takes two numeric vector arguments and is used to perform the denomination for each
#' column. By default, this is division, i.e. `x[[col]]/denoms[[col]]` for given columns, but any function can be passed
#' that takes two numeric vectors as inputs and returns a single numeric vector. See details.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # Get a sample of indicator data (note must be indicators plus a "UnitCode" column)
#' iData <- ASEM_iData[c("uCode", "Goods", "Flights", "LPI")]
#' # Also get some denominator data
#' denoms <- ASEM_iData[c("uCode", "GDP", "Population")]
#' # specify how to denominate
#' denomby <- data.frame(iCode = c("Goods", "Flights"),
#' Denominator = c("GDP", "Population"),
#' ScaleFactor = c(1, 1000))
#' # Denominate one by the other
#' iData_den <- Denominate(iData, denoms, denomby)
#'
#' @return A data frame of the same size as `x`, with any specified columns denominated according to specifications.
#'
#' @seealso
#' * [WorldDenoms] A data set of some common national-level denominators.
#'
#' @export
Denominate.data.frame <- function(x, denoms, denomby, x_ID = NULL, denoms_ID = NULL,
                                   f_denom = NULL, ...){


  # CHECKS ------------------------------------------------------------------

  # denoms
  stopifnot(is.data.frame(denoms))

  # denomby
  stopifnot(is.data.frame(denomby))
  required_cols <- c("iCode", "Denominator")
  lapply(required_cols, function(col){
    if(is.null(denomby[[col]])){
      stop("Required column not found in denomby: ", col)
    }
  })
  stopifnot(is.character(denomby$iCode),
            is.character(denomby$Denominator))

  if(!is.null(denomby$ScaleFactor)){
    stopifnot(is.numeric(denomby$ScaleFactor))
  } else {
    denomby$ScaleFactor <- 1
  }

  # denom_ID
  if(is.null(denoms_ID)){
    denoms_ID <- "uCode"
  }
  stopifnot(denoms_ID %in% colnames(denoms))

  # denom_ID
  if(is.null(x_ID)){
    x_ID <- "uCode"
  }
  stopifnot(x_ID %in% colnames(x))

  # cross checks
  if(any(denomby$iCode %nin% names(x))){
    stop("One or more iCode entries in denomby not found in x.")
  }
  if(any(!sapply(x[denomby$iCode], is.numeric))){
    stop("One or more columns in x referred to by denomby$iCode is not numeric.")
  }
  if(any(denomby$Denominator %nin% names(denoms))){
    stop("One or more Denominator entries in denomby not found in denoms.")
  }
  if(any(!sapply(denoms[denomby$Denominator], is.numeric))){
    stop("One or more columns in denoms referred to by denomby$Denominator is not numeric.")
  }

  # check all uCodes in x are found in denoms
  if(any(x[[x_ID]] %nin% denoms[[denoms_ID]])){
    stop("One or more ID codes in x not found in denoms.")
  }

  if(is.null(f_denom)){
    f_denom <- `/`
  }


  # DENOMINATE --------------------------------------------------------------

  # first prep denoms to match the rows of x
  denoms_matched <- denoms[match(x[[x_ID]], denoms[[denoms_ID]]) ,]

  denom_col <- function(iCode){
    # col from x
    xcol <- x[[iCode]]
    # scaled denominator column
    denom_code <- denomby$Denominator[denomby$iCode == iCode]
    dcol <- denoms_matched[[denom_code]] / denomby$ScaleFactor[denomby$iCode == iCode]
    # denominate
    do.call(f_denom, list(xcol, dcol))
  }

  # run function (denominate)
  x_denomcols <- as.data.frame(sapply(denomby$iCode, denom_col))

  # subst back in
  x[colnames(x_denomcols)] <- x_denomcols

  x

}


#' Denominate data
#'
#' "Denominates" or "scales" variables by other variables. Typically this is done by dividing extensive variables such as
#' GDP by a scaling variable such as population, to give an intensive variable (GDP per capita).
#'
#' See documentation for individual methods: [Denominate.data.frame()], [Denominate.coin()] and [Denominate.purse()].
#'
#' @param x Object to be denominated
#' @param ... arguments passed to or from other methods
#'
#' @return message
#'
#' @export
Denominate <- function (x, ...){
  UseMethod("Denominate")
}
