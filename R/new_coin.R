#' Create a new coin
#'
#' Creates a new coin class object.
#'
#' @param iData The indicator data and metadata of each unit
#' @param iMeta Indicator metadata
#'
#' @examples
#' #
#'
#' @seealso
#'
#'
#' @return A "coin" object
#'
#' @export
new_coin <- function(iData, iMeta){

  # record function inputs

  # check inputs, aiming to
  # a. pick out any errors
  # b. detect whether we want to generate multiple coins

  # somewhere we split to separate iDatas. Presumably iMeta should be the same still

  # We should generate uName if not present

  # perhaps any further checks on individual iData, iMeta?

  # Now build coins - either call separate function or recursively?

  # Decide to either extract denoms, groups, other "passed-through" variables or not
  # I'm thinking yes because merge is fairly easy. Can have a get_dset func to automatically merge

  # What happens in the panel case, if we end up with different number/set of units and indicators?
  # Is this allowed?

  # aglevel names (maybe input to new_coin?)

  # build list

  # Add lineage

  #

}


#' Check iData
#'
#' Checks the format of iData input to [new_coin()].
#'
#' @param iData A data frame of indicator data.
#' @param quietly Set `TRUE` to suppress message if input is valid.
#'
#' @examples
#' check_IndData(IndData_2020)
#'
#' @return Message if everything ok, else error messages.
#'
#' @export

check_iData <- function(iData, quietly = FALSE){

  # check is df
  stopifnot(is.data.frame(iData))

  # check is NOT tibble
  if(inherits(iData, "tbl_df")){
    stop("iData is a tibble. Please convert to data frame first ( as.data.frame() ).")
  }


  # REQUIRED COLS -----------------------------------------------------------
  # Required cols are in fact only uCode

  required_cols <- c("uCode")
  # check present
  if(any(required_cols %nin% colnames(iData))){
    stop("One or more expected col names not found (", required_cols, ").")
  }
  # check type
  if(!is.character(iData$uCode)){
    stop("uCode is required to be a character vector.")
  }


  # SPECIAL COLS ------------------------------------------------------------
  # Special cols are those that are not REQUIRED but defined in iMeta

  # Time
  if(!is.null(iData[["Time"]])){
    if(!is.numeric(iData$Time)){
      stop("iData$Time is required to be a numeric vector.")
    }
  }

  # uName
  if(!is.null(iData[["uName"]])){
    if(!is.character(iData$uName)){
      stop("iData$uName is required to be a character vector.")
    }
  }



  # DUPLICATES --------------------------------------------------------------

  # Check unique uCodes
  if(anyDuplicated(iData$uCode) > 1){
    stop("Duplicates detected in iData$uCode.")
  }
  # Check unique colnames
  if(anyDuplicated(colnames(iData)) > 1){
    stop("Duplicates detected in colnames(iData).")
  }

  # check uCode and colnames don't overlap
  if(length(intersect(unique(iData$uCode), colnames(iData) ))>0){
    stop("uCode and colnames(iData) contain overlapping codes.")
  }


  if(!quietly){
    message("IndData checked and OK.")
  }
}
