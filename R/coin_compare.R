#' Rank comparison table between two COINs
#'
#' Takes two COINs, and generates a rank comparison between specified indicator/aggregates. COINs must share at least some common
#' unit codes, and the indicator selected by `isel`.
#'
#' @param COIN1 First COIN
#' @param COIN2 Second COIN
#' @param dset The data set of interest
#' @param isel The indicator/column of interest
#' @param COINnames An optional character vector of the names of `COIN1` and `COIN2`, to be used in the table headers.
#' @param sort_by If `"RankCOIN1"`, sorts by the indicator values of COIN1, if `"RankCOIN2"`, sorts by `COIN2`,
#' if `"RankChange"`, sorts by rank change, and if `"AbsRankChange"` sorts by absolute rank change.
#'
#' @examples
#' ASEM <- build_ASEM()
#' # Make a copy
#' ASEMAltNorm <- ASEM
#' # Edit .$Method
#' ASEMAltNorm$Method$normalise$ntype <- "borda"
#' # Regenerate
#' ASEMAltNorm <- regen(ASEMAltNorm, quietly = TRUE)
#' # compare
#' CT <- compTable(ASEM, ASEMAltNorm, dset = "Aggregated", isel = "Index")
#'
#' @return A data frame with ranks and rank changes between two COINs.
#'
#' @seealso
#' * [compTableMulti()] Comparison table between multiple COINs
#'
#' @export
#'
compTable <- function(COIN1, COIN2, dset = "Raw", isel, COINnames = NULL, sort_by = "AbsRankChange"){

  tab1 <- COIN1$Data[[dset]][c("UnitCode", "UnitName", isel)]
  tab2 <- COIN2$Data[[dset]][c("UnitCode", "UnitName", isel)]

  # join the two tables
  df1 <- merge(tab1, tab2, by = c("UnitCode", "UnitName"))
  # convert scores to ranks
  df1 <- rankDF(df1)
  # add diff and abs diff
  df1 <- cbind(df1,
               df1[3] - df1[4],
               abs(df1[3] - df1[4]))
  # tidy up
  colnames(df1) <- c("UnitCode", "UnitName", "RankCOIN1", "RankCOIN2", "RankChange", "AbsRankChange")

  # sort
  if(sort_by == "RankCOIN1" | sort_by == "RankCOIN2"){
    df1 <- df1[order(df1[[sort_by]]),]
  } else {
    df1 <- df1[order(-df1[[sort_by]]),]
  }


  if (!is.null(COINnames)){
    colnames(df1)[3:4] <- paste0("Rank: ",COINnames)
  }

  df1
}

#' Rank tables between multiple COINs
#'
#' Takes multiple COINs (two or more), and generates a rank comparison for a single indicator or aggregate.
#'
#' @param COINs A list of COINs
#' @param dset The data set to extract the indicator from (must be present in each COIN). Default `"Aggregated"`.
#' @param isel Code of the indicator or aggregate to extract from each COIN (must be present in the specified
#' data set of each COIN). Default `"Index"`.
#' @param tabtype The type of table to generate - `"Ranks"`, `"Diffs"`, or `"AbsDiffs"`.
#' @param ibase The index of the COIN to use as a base comparison
#' @param sort_table If TRUE, sorts by the base COIN (`ibase`) (default).
#' @param extra_cols A character vector of any extra columns to include from the COIN referenced by `ibase`. For example,
#' this could include group columns.
#'
#' @importFrom purrr modify_if
#'
#' @examples
#' ASEM <- build_ASEM()
#' # Make a copy
#' ASEMAltNorm <- ASEM
#' # Edit .$Method
#' ASEMAltNorm$Method$normalise$ntype <- "borda"
#' # Regenerate
#' ASEMAltNorm <- COINr::regen(ASEMAltNorm, quietly = TRUE)
#' # compare
#' ctable <- compTableMulti(list(ASEM, ASEMAltNorm), dset = "Aggregated", isel = "Index")
#'
#' # add more COINs to the list to see more cols in the table...
#'
#' @return Rank comparison table as a data frame
#'
#' @seealso
#' * [compTable()] Comparison table between two COINs
#'
#' @export

compTableMulti <- function(COINs, dset = "Aggregated", isel = "Index", tabtype = "Ranks", ibase = 1,
                           sort_table = TRUE, extra_cols = NULL){

  if(any(!(sapply(COINs, is.COIN)))){
    stop("One or more elements of COINs argument is not a valid COIN. Please check.")
  }

  if(length(COINs)<2){
    stop("List of COINs has less than two entries. You need at least 2 COINs to use this function.")
  }

  # change order of list: put ibase first
  COINs <- COINs[c(ibase, setdiff(1:length(COINs), ibase))]

  # names
  if(is.null(names(COINs))){
    names(COINs) <- paste0("COIN_", 1:length(COINs))
  }

  # get scores of baseline COIN
  tab1 <- COINs[[1]]$Data[[dset]][c("UnitCode", "UnitName", extra_cols, isel)]
  colnames(tab1)[ncol(tab1)] <- names(COINs)[1]

  # now loop over COINs to get the other columns
  for (ii in 2:length(COINs)){

    # get indicator data for iith COIN
    tabi <- COINs[[ii]]$Data[[dset]][c("UnitCode", isel)]

    # join the two tables
    tab1 <- merge(tab1, tabi, by = "UnitCode")

    # rename col
    colnames(tab1)[ncol(tab1)] <- names(COINs)[ii]
  }

  # convert to ranks
  tab1 <- rankDF(tab1)

  # if tabtype is not "Ranks", have to do a further step
  if  (tabtype == "Diffs"){

    # calculate rank differences
    tab1 <- purrr::modify_if(tab1,
                             .p = is.numeric,
                             .f = ~{tab1[names(COINs)[1]] - .x}
    )

  } else if (tabtype == "AbsDiffs"){

    # calculate abs rank differences
    tab1 <- purrr::modify_if(tab1,
                             .p = is.numeric,
                             .f = ~{abs(tab1[names(COINs)[1]] - .x)}
                             )
  }

  # sort
  if(sort_table){
    tab1 <- tab1[order(tab1[[names(COINs)[1]]]),]
  }

  return(tab1)
}
