#' Create a new coin
#'
#' Creates a new coin class object.
#'
#' The `exclude` argument can be used to exclude specified indicators. If this is specified, `.$Data$Raw`
#' will be built excluding these indicators, as will all subsequent build operations. However the full data set
#' will still be stored in `.$Log$new_coin`. The codes here should correspond to entries in the `iMeta$iCode`.
#' This option is useful e.g. in generating alternative coins with different indicator sets, and can be included
#' as a variable in a sensitivity analysis.
#'
#' The `split_to` argument allows panel data to be used. Panel data must have a `Time` column in `iData`, which
#' consists of some numerical time variable, such as a year. Panel data has multiple observations for each `uCode`,
#' one for each unique entry in `Time`. The `Time` column is required to be numerical, because it needs to be
#' possible to order it. To split panel data, specify `split_to = "all"` to split to a single coin for each
#' of the unique entries in `Time`. Alternatively, you can pass a vector of entries in `Time` which allows
#' to split to a subset of the entries to `Time`.
#'
#' Splitting panel data results in a so-called "purse" class, which is a data frame of COINs, indexed by `Time`.
#' See online documentation for more details (*TO ADD*).
#'
#' @param iData The indicator data and metadata of each unit
#' @param iMeta Indicator metadata
#' @param exclude Optional character vector of any indicator codes (`iCode`s) to exclude from the coin(s).
#' @param split_to This is used to split panel data into multiple coins, a so-called "purse". Should be either
#' `"all"`, or a subset of entries in `iData$Time`. See Details.
#' @param level_names Optional character vector of names of levels. Must have length equal to the number of
#' levels in the hierarchy (`max(iMeta$Level, na.rm = TRUE)`).
#' @param quietly If `TRUE`, suppresses all messages
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
new_coin <- function(iData, iMeta, exclude = NULL, split_to = NULL,
                     level_names = NULL, quietly = FALSE){

  # WRITE TO LOG ------------------------------------------------------------

  coin <- vector(mode = "list", length = 0)
  coin <- write_log(coin)

  # OVERALL CHECKS ----------------------------------------------------------

  # individual dfs
  check_iData(iData, quietly = quietly)
  check_iMeta(iMeta, quietly = quietly)

  # covert any tibbles to normal dfs.
  if(inherits(iData, "tbl_df")){
    iData <- as.data.frame(iData)
  }
  if(inherits(iMeta, "tbl_df")){
    iMeta <- as.data.frame(iMeta)
  }

  # CROSS CHECKS
  # Make sure iData codes are all in iMeta, excluding special codes

  iData_codes <- colnames(iData)[colnames(iData) %nin% c("uCode", "uName", "Time")]

  if(any(iData_codes %nin% iMeta$iCode)){
    stop("Column names from iData not found in iMeta (excluding special columns).")
  }
  if(any(iMeta$iCode[iMeta$Type != "Aggregate"] %nin% colnames(iData))){
    stop("Entries in iMeta$iCode not found in colnames(iData).")
  }

  # we need indicator codes
  iCodes <- iMeta$iCode[iMeta$Type == "Indicator"]
  non_numeric_inds <- !(sapply(iData[iCodes], is.numeric))
  if(any(non_numeric_inds)){
    stop("Non-numeric indicators detected. The following have been labelled as 'Indicator' but refer to non-numeric columns in iData (not allowed): \n", paste(iCodes[non_numeric_inds], collapse = ", " ),
  "\n This may occur if you have imported data with NAs read as strings.")
  }

  # EXCLUDE INDICATORS ------------------------------------------------------
  # Optionally exclude any specified indicators

  if(!is.null(exclude)){

    stopifnot(is.character(exclude))
    if(any(exclude %nin% iMeta$iCode)){
      stop("One or more entries in exclude not found in iMeta$iCode...")
    }

    iData <- iData[colnames(iData) %nin% exclude]
    iMeta <- iMeta[iMeta$iCode %nin% exclude, ]

    # if removing indicators results in empty aggregation groups
    # (childless parents) we have to remove these. Otherwise when
    # we aggregate, there are aggregation groups with nothing to aggregate.
    childless <- (iMeta$Level > 1) & (iMeta$iCode %nin% iMeta$Parent)
    iMeta <- iMeta[!childless, ]
  }

  # GENERATE DEFAULT NAMES --------------------------------------------------

  # default names are codes
  if(is.null(iData$uName)){
    iData$uName <- iData$uCode
  }
  if(is.null(iMeta$iName)){
    iMeta$iName <- iMeta$iCode
  }

  # SORT DFS ----------------------------------------------------------------

  # also all codes not indicator codes (excluding uCode)
  not_icodes <- names(iData)[names(iData) %nin% c("uCode", iCodes)]

  # This is not strictly necessary but may help later on
  iMeta <- iMeta[order(iMeta$Level, iMeta$Parent),]
  iData <- iData[c("uCode", not_icodes, iCodes)]

  # iData sorting depends on if we have panel data
  is_panel <- length(unique(iData$Time)) > 1
  if(is_panel){
    iData <- iData[order(iData$Time, iData$uCode),]
  } else {
    iData <- iData[order(iData$uCode),]
  }

  # SPLIT PANEL DATA --------------------------------------------------------
  # NOTE: splitting may cause different numbers of units in each coin, but the number of indicators
  # should always be the same, even if some will have all-NAs.

  # NOTE: we need to include the year imputation at some point here.

  if(!is.null(split_to)){

    # make sure we can split first
    if(!is_panel){
      stop("Cannot split to multiple coins because either iData$Time doesn't exist, or you have only
           one unique entry in iData$Time.")
    }

    # now split
    iData_list <- split_iData(iData, split_to = split_to)
    # check
    suppressMessages(lapply(iData_list, check_iData))
  } else {
    if(is_panel){
      stop("Panel data detected, but you have not specifed split_to - please specify this.")
    }
    iData_list <- list(iData)
  }

  # BUILD COINS -------------------------------------------------------------

  # First make some mods to the "base" coin which are same for all coins
  coin$Meta$Ind <- iMeta
  coin$Meta$Lineage <- get_lineage(iMeta, level_names = level_names)
  # we also need to forget about splitting, as if we regenerate one of
  # the coins in the purse, this would cause an error
  coin$Log$new_coin$split_to <- NULL

  coinmaker <- function(iDatai){

    # copy the "global" coin
    coin_i <- coin

    # Store data (only uCode plus indicators)
    coin_i <- write_dset(coin_i, iDatai[c("uCode", iCodes)], dset = "Raw",
                         ignore_class = TRUE)

    # alter Log to only include iData of the COIN (not whole panel)
    coin_i$Log$new_coin$iData <- iDatai

    # Extract denominators, groups and other non-indicator cols
    coin_i$Meta$Unit <- iDatai[c("uCode", not_icodes)]

    # class
    class(coin_i) <- "coin"

    # return
    coin_i
  }

  # now run coinmaker on list of iData
  coins <- lapply(iData_list, coinmaker)

  # TWEAKS AND OUTPUT -------------------------------------------------------

  # squash to single coin if only one in the list
  if(length(coins)==1){

    f_output <- coins[[1]]

  } else {

    # get time value for each coin
    coin_times <- sapply(coins, function(x){
      unique(x$Meta$Unit$Time)
    })

    f_output <- data.frame(Time = coin_times)
    f_output$coin <- coins

    class(f_output) <- c("purse", "data.frame")
  }

  f_output
}


#' Check iData
#'
#' Checks the format of `iData` input to [new_coin()]. This check must be passed to successfully build a new
#' coin.
#'
#' The restrictions on `iData` are not extensive. It should be a data frame with only one required column
#' `uCode` which gives the code assigned to each unit (alphanumeric, not starting with a number). All other
#' columns are defined by corresponding entries in `iMeta`, with the following special exceptions:
#'
#' * `Time` is an optional column which allows panel data to be input, consisting of e.g. multiple rows for
#' each `uCode`: one for each `Time` value. This can be used to split a set of panel data into multiple coins
#' (a so-called "purse") which can be input to COINr functions. See [new_coin()] for more details.
#' * `uName` is an optional column which specifies a longer name for each unit. If this column is not included,
#' unit codes (`uCode`) will be used as unit names where required.
#'
#' @param iData A data frame of indicator data.
#' @param quietly Set `TRUE` to suppress message if input is valid.
#'
#' @examples
#' check_iData(ASEM_iData)
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
    # flag if panel data: more than one unique value in Time
    is_panel <- length(unique(iData$Time)) > 1
  }

  # uName
  if(!is.null(iData[["uName"]])){
    if(!is.character(iData$uName)){
      stop("iData$uName is required to be a character vector.")
    }
  }

  # DUPLICATES --------------------------------------------------------------

  # Check unique uCodes
  # This is different depending on whether iData is panel data or not

  if(is_panel){
    if(anyDuplicated(iData[c("uCode", "Time")]) > 1){
      stop("Duplicate uCode/Time pairs found.")
    }
  } else {
    if(anyDuplicated(iData$uCode) > 1){
      stop("Duplicates detected in iData$uCode.")
    }
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
#' Checks the format of `iMeta` input to [new_coin()]. This performs a series of thorough checks to make sure
#' that `iMeta` agrees with the specifications. This also includes checks to make sure the structure makes
#' sense, there are no duplicates, and other things. `iMeta` must pass this check to build a new coin.
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
#' `Other` if the variable should be ignored but passed through. Any other entries here will cause an error.
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

  # INITIAL CHECKS ----------------------------------------------------------

  # check is df
  stopifnot(is.data.frame(iMeta))

  # if tibble, convert (no alarms and no surprises)
  if(inherits(iMeta, "tbl_df")){
    iMeta <- as.data.frame(iMeta)
  }

  # REQUIRED COLS -----------------------------------------------------------

  # required cols
  required_cols <- c("Level", "iCode", "Parent", "Direction", "Type", "Weight")
  if(!all(required_cols %in% colnames(iMeta))){
    stop("One or more expected col names not found (Level, iCode, Parent, Direction, Type, Weight).")
  }

  # check col types
  col_numeric <- c("Level", "Direction", "Weight")
  col_char <- setdiff(required_cols, col_numeric)

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


  # SPECIFIC COL CHECKS -----------------------------------------------------

  # Level should be in 1:100 (not expecting more than 1000 levs)
  levs <- iMeta$Level[!is.na(iMeta$Level)] |>
    unique()
  if(any(levs %nin% 1:1000)){
    stop("Level column has unexpected entries. Expected as positive integers.")
  }
  # Level should not skip any levels
  maxlev <- max(levs, na.rm = TRUE)
  if(!setequal(levs, 1:maxlev)){
    stop("Level column has missing entries between 1 and max(Level).")
  }

  # iCode should have no duplicates
  if(anyDuplicated(iMeta$iCode) != 0){
    stop("Duplicate entries in iCode.")
  }
  # iCode should not start with a number
  num_start <- substring(iMeta$iCode, 1,1) %in% 0:9
  if(any(num_start)){
    stop("One or more entries in iCode begins with a number - this causes problems and is not allowed.")
  }
  # iCode no NAs are allowed
  if(any(is.na(iMeta$iCode))){
    stop("NAs found in iCode - NAs are not allowed.")
  }

  # Direction should only be -1 or 1
  dirs <- iMeta$Direction[!is.na(iMeta$Direction)]
  if(any(dirs %nin% c(-1, 1))){
    stop("One or more entries in Direction are not -1 or 1.")
  }

  # Type has to be one of the following
  itypes <- c("Indicator", "Aggregate", "Group", "Denominator", "Other")
  if(any(iMeta$Type %nin% itypes)){
    stop("One or more entries in Type is not allowed - should be one of Indicator, Aggregate, Group, Denominator, Other.")
  }

  ## THE FOLLOWING ARE OPTIONAL COLS

  # if iName exists, should be alphanumeric
  if(!is.null(iMeta$iName)){
    if(!is.character(iMeta$iName)){
      stop("iName is not a character vector, which is required. If you don't want to specify iName, this
           column can also be removed.")
    }
    # also no NAs are allowed
    if(any(is.na(iMeta$iName))){
      stop("NAs found in iName - if iName is specified, NAs are not allowed.")
    }
  }
  # if Unit exists, should be alphanumeric
  if(!is.null(iMeta$Unit)){
    if(!is.character(iMeta$Unit)){
      stop("Unit is not a character vector, which is required. If you don't want to specify Unit, this
           column can also be removed.")
    }
  }
  # if Target exists, should be alphanumeric
  if(!is.null(iMeta$Target)){
    if(!is.numeric(iMeta$Target)){
      stop("Target is not a numeric vector, which is required. If you don't want to specify Target, this
           column can also be removed (it is only required for distance to target normalisation).")
    }
  }

  # BETWEEN-COL CHECKS ------------------------------------------------------

  # Level should be non-NA for all indicators and aggregates
  if( any(is.na(iMeta$Level) & (iMeta$Type %in% c("Indicator", "Aggregate"))) ){
    stop("NAs detected in Level for Indicator/Aggregates. All Indicators and Aggregates must have a numeric
         Level defined.")
  }
  # Level should be 1 for indicators
  if(any( (iMeta$Level != 1) & (iMeta$Type == "Indicator") )){
    stop("One or more rows of Type 'Indicator' is assigned Level != 1. Indicators should all be at Level 1.")
  }

  # Direction should be non-NA for all indicators and aggregates
  if( any(is.na(iMeta$Direction) & (iMeta$Type %in% c("Indicator", "Aggregate"))) ){
    stop("NAs detected in Direction for Indicator/Aggregates. All Indicators and Aggregates must have a
         Direction defined (either 1 or -1).")
  }

  # Weight should be non-NA for all indicators and aggregates
  if( any(is.na(iMeta$Weight) & (iMeta$Type %in% c("Indicator", "Aggregate"))) ){
    stop("NAs detected in Weight for Indicator/Aggregates. All Indicators and Aggregates must have a numeric
         Weight defined.")
  }

  # Unit should be non-NA except for Groups
  if( any(is.na(iMeta$Unit) & (iMeta$Type != "Group")) ){
    stop("NAs detected in Unit: NAs are only allowed in Unit for Type = 'Group'.")
  }

  # Target be specified for anything at Level 1
  if( any(is.na(iMeta$Target) & (iMeta$Type == "Indicator")) ){
    stop("NAs detected in Target for Type = 'Indicator'. If targets are specified, they must be non-NA
         for all indicators. You can also remove the Target column if you don't need targets.")
  }

  # Parent should refer to codes already present in iCode
  notin_iCode <- (iMeta$Parent[!is.na(iMeta$Parent)] %nin% iMeta$iCode)
  if(any(notin_iCode)){
    stop("One or more entries in Parent not found in iCode.")
  }
  # check top level has NA for parent (no parent assigned)
  if( any(!is.na(iMeta$Parent) & (iMeta$Parent == maxlev)) ){
    stop("Entries found in Parent at the highest aggregation level. At the highest aggregation level
         there are no parents, so so Parent should be NA.")
  }

  # STRUCTURE CHECKS --------------------------------------------------------

  # This function checks, for a given CODE/PARENT pair, whether the parent is in the level
  # immediately above. If not, reports error.
  levcheck <- function(x){
    chld <- x[1]
    prnt <- x[2]
    # level of child
    chld_lev <- iMeta$Level[iMeta$iCode == chld]
    # if we reach the top level, break
    if(chld_lev == maxlev) return(NULL)
    # level of parent
    prnt_lev <- iMeta$Level[iMeta$iCode == prnt]
    # check if parent is immediately above child
    if(prnt_lev != (chld_lev + 1)){
      stop(paste0(
        "Level discrepancy detected. An iCode has a Parent in a Level other than the one immediately above it: ",
        "iCode = ", chld,
        ", Parent = ", prnt))
    }
  }
  # run function above on rows of iData
  # note, return to a variable just to avoid returning NULL (see func above)
  check_struct <- apply(iMeta[(iMeta$Type %in% c("Indicator", "Aggregate")) ,c("iCode", "Parent")],
                        MARGIN = 1,
                        levcheck)

  if(!quietly){
    message("iMeta checked and OK.")
  }
}


#' Split iData
#'
#' Splits `iData` by the `Time` column into multiple `iData` data frames.
#'
#' @param iData A data frame of indicator panel data
#' @param split_to Either `"all"` (one `iData` for each unique entry in `iData$Time`), or a vector containing a
#' subset of entries in `iData$Time`. In the latter case, `iData`s will only be generated for the entries in this
#' vector.
#'
#' @examples
#' #
#'
#' @return List of `iData` data frames
split_iData <- function(iData, split_to){

  # this function is only called from new_coin(), so if we are here, then the iData should be valid,
  # and there should be more than one unique entry in iData$Year.

  if(split_to != "all"){

    if(any(split_to %nin% iData$Time)){
      stop("One or more entries in split_to is not found in iData$Time.")
    }
    iData <- iData[iData$Time %in% split_to]
  }

  # return list of dfs
  split(iData, iData$Time)

}


#' Make wide structure table
#'
#' Takes an iMeta table and outputs a wide format index structure, i.e. a table with one column per
#' level in the index. This is used in later functions to look up the full "ancestry" of any element
#' in the index.
#'
#' @param iMeta A data frame of indicator metadata. For specs see [check_iMeta()].
#'
#' @examples
#' get_lineage(ASEM_iMeta)
#'
#' @return Lineage table as data frame
get_lineage <- function(iMeta, level_names = NULL){

  # isolate the structural part of iMeta
  longS <- iMeta[c("iCode", "Parent")]
  # prep wide version: filter to the indicator level and parent level
  wideS <- iMeta[iMeta$Type == "Indicator", c("iCode", "Parent")]

  # find max level
  maxlev <- max(iMeta$Level, na.rm = TRUE)

  # successively add columns by looking up parent codes of last col
  for(ii in 2:(maxlev-1)){
    wideS <- cbind(wideS,
                   longS$Parent[match(wideS[[ii]], longS$iCode)])
  }

  # rename columns
  if(is.null(level_names)){
    level_names <- paste0("Level_", 1:maxlev)
  } else {
    if(length(level_names) != ncol(wideS)){
      stop("level_names is not the same length as the number of levels in the index.")
    }
  }
  colnames(wideS) <- level_names

  wideS
}
