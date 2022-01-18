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
  # Generate iName if not present

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
#' check_iData(IndData_2020)
#'
#' @return Message if everything ok, else error messages.
#'
#' @export

check_iData <- function(iData, quietly = FALSE){

  # check is df
  stopifnot(is.data.frame(iData))

  # if tibble, convert (no alarms and no surprises)
  if(inherits(iData, "tbl_df")){
    iData <- as.data.frame(iData)
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

  # OUTPUT ------------------------------------------------------------------

  if(!quietly){
    message("iData checked and OK.")
  }
}


#' Check iMeta
#'
#' Checks the format of `iMeta` input to [new_coin()].
#'
#' Required columns for `iMeta` are:
#' * `Level`: Level in aggregation, where 1 is indicator level, 2 is the level resulting from aggregating
#' indicators, 3 is the result of aggregating level 2, and so on. Set to `NA` for entries that are not included
#' in the index (groups, denominators, etc).
#' * `iCode`: Indicator code, alphanumeric. Must not start with a number.
#' * `Parent`: Group (`iCode`) to which indicator/aggregate belongs in level immediately above.
#' Each entry here should also be found in `iCode`. Set to `NA` only
#' for the highest (Index) level (no parent), or for entries that are not included
#' in the index (groups, denominators, etc).
#' * `Direction`: Numeric, either -1 or 1
#' * `Weight`: Numeric weight, will be rescaled to sum to 1 within aggregation group. Set to `NA` for entries that are not included
#' in the index (groups, denominators, etc).
#' * `Type`: The type, corresponding to `iCode`. Can be either `Indicator`, `Aggregate`, `Group`, `Denominator`,
#' or `Other`.
#'
#' Optional columns that are recognised in certain functions are:
#' * `iName`: Name of the indicator: a longer name which is used in some plotting functions.
#' * `Unit`: the unit of the indicator, e.g. USD, thousands, score, etc. Used in some plots if available.
#' * `Target`: a target for the indicator. Used if normalisation type is distance-to-target.
#'
#' The `iMeta` data frame essentially gives details about each of the columns found in `iData`, as well as
#' details about additional data columns eventually created by aggregating indicators. This means that the
#' entries in `iMeta` must include *all* columns in `iData`, *except* the three special column names: `uCode`,
#' `uName`, and `Time`. In other words, all column names of `iData` should appear in `iMeta$iCode`, except
#' the three special cases mentioned. The `iName` column optionally can be used to give longer names to each indicator
#' which can be used for display in plots.
#'
#' `iMeta` also specifies the structure of the index, by specifying the parent of each indicator and aggregate.
#' The `Parent` column must refer to entries that can be found in `iCode`. Try `View(ASEM_iMeta)` for an example
#' of how this works.
#'
#' `Level` is the "vertical" level in the hierarchy, where 1 is the bottom level (indicators), and each successive
#' level is created by aggregating the level below according to its specified groups.
#'
#' `Direction` is set to 1 if higher values of the indicator should result in higher values of the index, and
#' -1 in the opposite case.
#'
#' The `Type` column specifies the type of the entry: `Indicator` should be used for indicators at level 1.
#' `Aggregate` for aggregates created by aggregating indicators or other aggregates. Otherwise set to `Group`
#' if the variable is not used for building the index but instead is for defining groups of units. Set to
#' `Denominator` if the variable is to be used for scaling (denominating) other indicators. Finally, set to
#' `Other` if the variable should be ignored but passed through.
#'
#' Note: this function requires the columns above as specified, but extra columns can also be added without
#' causing errors.
#'
#' @param iMeta A data frame of indicator metadata. See details.
#' @param quietly Set `TRUE` to suppress message if input is valid.
#'
#' @examples
#' check_iMeta(ASEM_iMeta)
#'
#' @return Message if everything ok, else error messages.
#'
#' @export

check_iMeta <- function(iMeta, quietly = FALSE){

  # check is df
  stopifnot(is.data.frame(iMeta))

  # if tibble, convert (no alarms and no surprises)
  if(inherits(iData, "tbl_df")){
    iData <- as.data.frame(iData)
  }

  # REQUIRED COLS -----------------------------------------------------------

  # required cols
  required_cols <- c("Level", "iCode", "Parent", "Direction", "Type", "Weight")
  if(!all(expected_cols %in% colnames(iMeta))){
    stop("One or more expected col names not found (Level, iCode, Parent, Direction, Type, Weight).")
  }

  # check col types
  col_numeric <- c("Level", "Direction", "Weight")
  col_char <- setdiff(expected_cols, col_numeric)

  # numeric
  num_check <- sapply(iMeta[col_numeric], is.numeric)

  if(!all(num_check)){
    stop(paste0("One or more of the following columns is not numeric: ", paste0(col_numeric, collapse = "/")))
  }

  # char
  char_check <- sapply(iMeta[col_char], is.character)

  if(!all(char_check)){
    stop(paste0("One or more of the following columns is not character: ", paste0(col_char, collapse = "/")))
  }

  # specific checks on columns
  # LEVEL should be in 1:100 (not expecting more than 100 levs)
  levs <- iMeta$Level[!is.na(iMeta$Level)] |>
    unique()
  if(levs %nin% 1:100){
    stop("LEVEL column has unexpected entries. Expected as positive integers.")
  }
  # LEVEL should not skip any levels
  maxlev <- max(levs, na.rm = TRUE)
  if(!setequal(levs, 1:maxlev)){
    stop("LEVEL column has missing entries between 1 and max(LEVEL).")
  }


  # PARENT should refer to codes already present in CODE
  if(!all( (iMeta$PARENT %in% iMeta$CODE) |
           is.na(iMeta$PARENT))){
    stop("One or more entries in PARENT not found in CODE.")
  }
  # check that (a) only one entry for level 1 (index), (b) no parent at this level
  top_level <- iMeta[iMeta$LEVEL==1,]
  if(nrow(top_level) > 1){
    stop("More than one entry for LEVEL = 1")
  }
  if(!is.na(top_level$PARENT)){
    stop("Non-NA value for LEVEL = 1. NA expected because this is index level.")
  }
  # DIRECTION should only be -1 or 1
  if(!all(iMeta$DIRECTION %in% c(-1, 1))){
    stop("One or more entries in DIRECTION are not -1 or 1.")
  }
  # CODE should have no duplicates
  if(anyDuplicated(iMeta$CODE) != 0){
    stop("Duplicate entries in CODE.")
  }
  # CODE should not start with a number
  num_start <- substring(iMeta$CODE, 1,1) %in% 0:9
  if(any(num_start)){
    stop("One or more entries in CODE begins with a number - this causes problems and is not allowed.")
  }

  # we should also check NUM column but not 100% sure on format, esp. for aggregates, yet.

  # Check on structure
  # This function checks, for a given CODE/PARENT pair, whether the parent is in the level
  # immediately above. If not, reports error.
  levcheck <- function(x){
    chld <- x[1]
    prnt <- x[2]
    # level of child
    chld_lev <- iMeta$LEVEL[iMeta$CODE == chld]
    # if we reach the top level, break
    if(chld_lev == 1) return(NULL)
    # level of parent
    prnt_lev <- iMeta$LEVEL[iMeta$CODE == prnt]
    # check if parent is immediately above child
    if(prnt_lev != (chld_lev - 1)){
      stop(paste0(
        "LEVEL discrepancy detected. A CODE has a PARENT in a LEVEL other than the one immediately above it: ",
        "CODE = ", chld,
        ", PARENT = ", prnt))
    }
  }
  # run function above on rows of IndData
  # note, return to a variable just to avoid returning NULL (see func above)
  check_struct <- apply(iMeta[c("CODE", "PARENT")],
                        MARGIN = 1,
                        levcheck)

  if(!quietly){
    message("iMeta checked and OK.")
  }
}
