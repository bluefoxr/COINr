#' Compare two coins
#'
#' Compares two coin class objects using a specified `iCode` (column of data) from specified data sets.
#'
#' This function replaces the now-defunct `compTable()` from COINr < v1.0.
#'
#' @param coin1 A coin class object
#' @param coin2 A coin class object
#' @param dset A data set that is found in `.$Data`.
#' @param iCode The name of a column that is found in `.$Data[[dset]]`.
#' @param also_get Optional metadata columns to attach to the table: see [get_data()].
#' @param compare_by Either `"ranks"` which produces a comparison using ranks, or else `"scores"`, which instead
#' uses scores. Note that scores may be very different if the methodology is different from one coin to another,
#' e.g. for different normalisation methods.
#' @param sort_by Optionally, a column name of the output data frame to sort rows by. Can be either
#' `"coin.1"`, `"coin.2"`, `"Diff"`, `"Abs.diff"` or possibly a column name imported using `also_get`.
#' @param decreasing Argument to pass to [order()]: how to sort.
#'
#' @examples
#' # build full example coin
#' coin <- build_example_coin(quietly = TRUE)
#'
#' # copy coin
#' coin2 <- coin
#'
#' # change to prank function (percentile ranks)
#' # we don't need to specify any additional parameters (f_n_para) here
#' coin2$Log$Normalise$global_specs <- list(f_n = "n_prank")
#'
#' # regenerate
#' coin2 <- Regen(coin2)
#'
#' # compare index, sort by absolute rank difference
#' compare_coins(coin, coin2, dset = "Aggregated", iCode = "Index",
#'               sort_by = "Abs.diff", decreasing = TRUE)
#'
#' @return A data frame of comparison information.
#'
#' @export
compare_coins <- function(coin1, coin2, dset, iCode, also_get = NULL, compare_by = "ranks",
                          sort_by = NULL, decreasing = FALSE){


  # CHECKS ------------------------------------------------------------------

  check_coin_input(coin1)
  check_coin_input(coin2)

  stopifnot(length(iCode) == 1,
            compare_by %in% c("ranks", "scores"))

  if(!is.null(also_get)){
    if(also_get == "none"){
      stop("also_get cannot be set to 'none': at a minimum uCodes are needed for matching.")
    }
  }


  # GET DATA AND MERGE ------------------------------------------------------

  # get selected CODE plus ISO3 for matching
  df1 <- get_data(coin1, dset = dset, iCodes = iCode, also_get = also_get)
  df2 <- get_data(coin2, dset = dset, iCodes = iCode, also_get = also_get)

  # merge the two tables and convert to ranks if needed
  df12 <- merge(df1, df2, by = "uCode", all.x = TRUE, all.y = TRUE)
  if(compare_by == "ranks"){
    df12 <- rank_df(df12)
  }

  # get meta columns for each coin - this needs to be done after merge because there could be some NAs

  m1_codes <- extract_iData(coin1, iData = df1, GET = "mCodes")
  m1_codes <- setdiff(m1_codes, "uCode")
  m2_codes <- extract_iData(coin2, iData = df2, GET = "mCodes")
  m2_codes <- setdiff(m2_codes, "uCode")
  if(!setequal(m1_codes, m2_codes)){
    stop("Different metadata columns returned by coin1 and coin2.")
  }

  # sort out meta cols, if there are any
  if(length(m1_codes) > 0){
    m1 <- df12[paste0(m1_codes, ".x")]
    m2 <- df12[paste0(m2_codes, ".y")]
    # replace any NAs from one df with the other
    # from here m1 is what we will put in the output table
    m1[is.na(m1)] <- m2[is.na(m1)]
    colnames(m1) <- gsub('.x', '', colnames(m1))
  }

  # get iCode cols
  idat1 <- df12[[paste0(iCode,".x")]]
  idat2 <- df12[[paste0(iCode,".y")]]

  # OUTPUT ------------------------------------------------------------------

  # assemble the table
  if(length(m1_codes) > 0){
    dfout <- data.frame(uCode = df12$uCode,
               m1, # meta data excluding uCode
               coin.1 = idat1,
               coin.2 = idat2,
               Diff = idat1 - idat2,
               Abs.diff = abs(idat1 - idat2))
  } else {
    dfout <- data.frame(uCode = df12$uCode,
               coin.1 = idat1,
               coin.2 = idat2,
               Diff = idat1 - idat2,
               Abs.diff = abs(idat1 - idat2))
  }

  # SORT
  if(is.null(sort_by)){
    dfout
  } else {
    if(sort_by %nin% names(dfout)){
      stop("sort_by not recognised...")
    }
    dfout[order(dfout[[sort_by]], decreasing = decreasing), ]

  }
}


#' Compare multiple coins
#'
#' Given multiple coins as a list, generates a rank comparison of a single indicator or aggregate which is specified
#' by the `dset` and `iCode` arguments (passed to [get_data()]). The indicator or aggregate targeted must be available
#' in all the coins in `coins`.
#'
#' By default, the ranks of the target indicator/aggregate of each coin will be merged using the `uCode`s within each coin.
#' Optionally, specifying `also_get` (passed to [get_data()]) will additionally merge using the metadata columns.
#' This means that coins must share the same metadata columns that are returned as a result of `also_get`.
#'
#' This function replaces the now-defunct `compTableMulti()` from COINr < v1.0.
#'
#' @param coins A list of coins. If names are provided, these will be used in the tables returned by this function.
#' @param tabtype The type of table to generate. One of:
#' * `"Values"`: returns a data frame of rank values for each coin provided, plus ISO3 column
#' * `"Diffs"`: returns a data frame of rank differences between the base coin and each other coin (see `ibase`)
#' * `"AbsDiffs"`: as `"Diffs"` but absolute rank differences are returned
#' * `"All"`: returns all of the three previous rank tables, as a list of data frames
#' @param ibase The index of the coin to use as a base comparison (default first coin in list)
#' @param sort_table If TRUE, sorts by the base COIN (`ibase`) (default).
#' @param dset The name of a data set found in `.$Data`. See [get_data()].
#' @param iCode A column name of the data set targeted by `dset`. See [get_data()].
#' @param also_get Optional metadata columns to attach to the table: see [get_data()]. If this is not specified, the
#' results of each coin will be merged using the `uCode`s within each coin. If this is specified, results will be
#' merged additionally using the metadata columns. This means that coins must share the same metadata columns that
#' are returned as a result of `also_get`.
#' @param compare_by Either `"ranks"` which produces a comparison using ranks, or else `"scores"`, which instead
#' uses scores. Note that scores may be very different if the methodology is different from one coin to another,
#' e.g. for different normalisation methods.
#'
#' @examples
#' # see vignette("adjustments")
#'
#' @return Data frame unless `tabtype = "All"`, in which case a list of three data frames is returned.
#'
#' @export
#'
compare_coins_multi <- function(coins, dset, iCode, also_get = NULL, tabtype = "Values",
                                ibase = 1, sort_table = TRUE, compare_by = "ranks"){

  tabtypes <- c("Values", "Diffs", "AbsDiffs", "All")

  # if all tabtypes are requested, recursively call the function
  if(tabtype == "All"){
    tablist <- lapply(setdiff(tabtypes, "All"), function(x){
      compare_coins_multi(coins, dset = dset, iCode = iCode, also_get = also_get, tabtype = x,
                          ibase = ibase, sort_table = sort_table, compare_by = compare_by)})
    names(tablist) <- setdiff(tabtypes, "All")
    return(tablist)
  }

  # Prep --------------------------------------------------------------------

  # checks
  if(any(!sapply(coins, is.coin))){
    stop("One or more coins are not coin class objects.")
  }
  if(tabtype %nin% tabtypes){
    stop("tabtype must be one of c('Values', 'Diffs', 'AbsDiffs', 'All') ")
  }
  stopifnot(ibase %in% 1:length(coins),
            is.logical(sort_table),
            compare_by %in% c("ranks", "scores"))
  if(!is.null(also_get)){
    if(also_get == "none"){
      stop("also_get cannot be set to 'none': at a minimum uCodes are needed for matching.")
    }
  }

  # names
  if (is.null(names(coins))){
    names(coins) <- paste0("coin.", 1:length(coins))
  }

  # change order of list: put ibase first
  coins <- coins[c(ibase, setdiff(1:length(coins), ibase))]

  # Assemble df of scores ---------------------------------------------------

  # get base
  df_CODE <- get_data(coins[[1]], dset = dset, iCodes = iCode, also_get = also_get)

  # this func checks uCode is present
  df_check <- function(dfi){
    if("uCode" %nin% names(dfi)){
      stop("Expected column 'uCode' not found in extracted data frame.")
    }
  }
  # check
  df_check(df_CODE)
  # get metadata column names
  m1_codes <- extract_iData(coins[[1]], iData = df_CODE, GET = "mCodes")
  # rename to avoid name duplication warnings
  names(df_CODE)[names(df_CODE) %nin% m1_codes] <- "v1"

  # func to merge other coins onto this table
  merge_dfs <- function(coin){

    # get new data
    df1 <- get_data(coin, dset = dset, iCodes = iCode, also_get = also_get)
    # check
    df_check(df1)

    # get meta column names
    m2_codes <- extract_iData(coin, iData = df1, GET = "mCodes")
    if(!setequal(m1_codes, m2_codes)){
      stop("Different metadata columns returned by coins: cannot merge.")
    }

    # rename to avoid name duplication warnings
    names(df1)[names(df1) %nin% m2_codes] <- paste0("v", ncol(df_CODE))
    # merge
    df_CODE <<- merge(df_CODE, df1, by = m2_codes, all.x = TRUE)
  }

  # apply merge function to remaining coins
  lapply(coins[-1], merge_dfs)
  # rename to list names
  names(df_CODE)[which(names(df_CODE) == "v1"):ncol(df_CODE)] <- names(coins)


  # Convert to output format ------------------------------------------------

  # first convert to ranks if needed (exclude metadata cols)
  if(compare_by == "ranks"){
    df_CODE[names(df_CODE) %nin% m1_codes] <- rank_df(df_CODE[names(df_CODE) %nin% m1_codes])
  }

  if (tabtype != "Values"){
    # DIFFS: subtract each col from the first (numeric) col
    df_CODE[names(coins)] <- df_CODE[names(coins)] - data.frame(rep(df_CODE[names(coins)[1]], length(names(coins))) )
  }
  if (tabtype == "AbsDiffs"){
    df_CODE[names(coins)] <- abs(df_CODE[names(coins)])
  }

  # sort
  if(sort_table){
    df_CODE <- df_CODE[order(df_CODE[[names(coins)[1]]]),]
  }

  df_CODE

}
