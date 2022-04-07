#' Convert a data frame to ranks
#'
#' Replaces all numerical columns of a data frame with their ranks. Uses sport ranking, i.e. ties
#' share the highest rank place. Ignores non-numerical columns. See [rank()]. Optionally, returns in-group ranks
#' using a specified grouping column.
#'
#' @param df A data frame
#' @param use_group An optional column of df (specified as a string) to use as a grouping variable. If specified, returns ranks
#' inside each group present in this column.
#'
#' @examples
#' # some random data, with a column of characters
#' df <- data.frame(RName = c("A", "B", "C"),
#' Score1 = runif(3), Score2 = runif(3))
#' # convert to ranks
#' rank_df(df)
#' # grouped ranking - use some example data
#' df1 <- ASEM_iData[c("uCode", "GDP_group", "Goods", "LPI")]
#' rank_df(df1, use_group = "GDP_group")
#'
#' @return A data frame equal to the data frame that was input, but with any numerical columns replaced with ranks.
#'
#' @seealso
#' * [round_df()] Round a data frame to a specified number of decimals.
#'
#' @export

rank_df <- function(df, use_group = NULL){

  if(is.null(use_group)){
    df <- data.frame(
      lapply(df, function(y) if(is.numeric(y)) rank(-1*y, na.last = "keep", ties.method = "min") else y)
    )
  } else {
    stopifnot(use_group %in% colnames(df))
    # get groups
    grps <- unique(unlist(df[[use_group]]))
    # I have to work over groups. To me the clearest way of doing this is with a for loop (sorry)
    dfold <- df
    for(grp in grps){
      # get current group rows
      grprows <- df[[use_group]] == grp
      # exclude any NAs
      grprows[is.na(grprows)] <- FALSE
      # now work over all columns, but just for the current group rows
      df[grprows,] <- data.frame(
        lapply(dfold[grprows,], function(y) if(is.numeric(y)) rank(-1*y, na.last = "keep", ties.method = "min") else y)
      )
    }

    # now I have to fill in rows that have NA group values, with NAs
    if(any(is.na(df[[use_group]]))){
      df[is.na(df[[use_group]]),] <- data.frame(lapply(df[is.na(df[[use_group]]),], function(y) if(is.numeric(y)) NA else y)
      )
    }
  }

  rownames(df) <- NULL
  df

}


#' Compare two data frames
#'
#' A custom function for comparing two data frames of indicator data, to see whether they match up, at a specified number of
#' significant figures.
#'
#' This function compares numerical and non-numerical columns to see if they match. Rows and columns can be in any order. The function
#' performs the following checks:
#'
#'   * Checks that the two data frames are the same size
#'   * Checks that column names are the same, and that the matching column has the same entries
#'   * Checks column by column that the elements are the same, after sorting according to the matching column
#'
#' It then summarises for each column whether there are any differences, and also what the differences are, if any.
#'
#' This is intended to cross-check results. For example, if you run something in COINr and want to check indicator results against
#' external calculations.
#'
#' @param df1 A data frame
#' @param df2 Another data frame
#' @param matchcol A common column name that is used to match row order. E.g. this might be `uCode`.
#' @param sigfigs The number of significant figures to use for matching numerical columns
#'
#' @examples
#' # take a sample of indicator data (including the uCode column)
#' data1 <- ASEM_iData[c(2,12:15)]
#' # copy the data
#' data2 <- data1
#' # make a change: replace one value in data2 by NA
#' data2[1,2] <- NA
#' # compare data frames
#' compare_df(data1, data2, matchcol = "uCode")
#'
#' @return A list with comparison results. List contains:
#' * `.$Same`: overall summary: if `TRUE` the data frames are the same according to the rules specified, otherwise `FALSE`.
#' * `.$Details`: details of each column as a data frame. Each row summarises a column of the data frame, saying whether
#' the column is the same as its equivalent, and the number of differences, if any. In case the two data frames have differing
#' numbers of columns and rows, or have differing column names or entries in `matchcol`, `.$Details` will simply contain a
#' message to this effect.
#' * `.$Differences`: a list with one entry for every column which contains different entries. Differences are summarised as
#' a data frame with one row for each difference, reporting the value from `df1` and its equivalent from `df2`.
#'
#' @export

compare_df <- function(df1, df2, matchcol, sigfigs = 5){

  # general checks
  stopifnot(is.data.frame(df1),
            is.data.frame(df2),
            matchcol %in% colnames(df1),
            matchcol %in% colnames(df2))

  # check for duplicates in matchcol
  if( (anyDuplicated(df1[[matchcol]]) > 0) | (anyDuplicated(df2[[matchcol]]) > 0) ){
    stop("Duplicates found in matchcol. This function requires unique entries in matchcol to make a comparison.")
  }

  # this is default but will change if anything is found to be different
  sameanswer <- TRUE

  # check sizes
  if(nrow(df1)!=nrow(df2)){
    sameanswer <- FALSE
    details <- "Different number of rows."
  }
  if(ncol(df1)!=ncol(df2)){
    sameanswer <- FALSE
    details <- "Different number of columns."
  }

  # check column names
  if(!setequal(colnames(df1), colnames(df2))){
    sameanswer <- FALSE
    details <- "Column names not the same."
  }
  # check row names same in matchcol
  if(!setequal(df1[[matchcol]], df2[[matchcol]])){
    sameanswer <- FALSE
    details <- "Elements in matchcol are not the same."
  }

  if(!sameanswer){
    # exiting because dfs have different sizes or column/row names
    return(list(Same = sameanswer,
                Details = details))
  } else {

    # From this point we should be fairly sure that the two dfs are the same size and contain the same cols and rows

    # match col order
    df2 <- df2[colnames(df1)]

    # match row order
    df2 <- df2[match(df1[[matchcol]], df2[[matchcol]]),]

    # Now the dfs should be also in the same order of rows and cols. Remains to check the values.
    details <- data.frame(Column = colnames(df1),
                          TheSame = NA,
                          Comment = NA,
                          NDifferent = NA)

    diffs <- vector(mode = "list", length = 0)

    # now loop over columns
    for(ii in 1:length(colnames(df1))){

      # get cols
      x <- df1[[ii]]
      y <- df2[[ii]]

      # class check
      if(class(x)!=class(y)){
        details$TheSame[[ii]] <- FALSE
        details$Comment[[ii]] <- "Class difference"
        next
      }

      # now check depending on type
      if(is.numeric(x)){

        if(identical(signif(x, sigfigs), signif(y, sigfigs))){
          details$TheSame[[ii]] <- TRUE
          details$Comment[[ii]] <- paste0("Numerical and identical to ", sigfigs, " sf.")
          details$NDifferent[[ii]] <- 0
        } else {
          details$TheSame[[ii]] <- FALSE
          details$Comment[[ii]] <- paste0("Numerical and different at ", sigfigs, " sf.")
          dfdiffs <- data.frame(MatchCol = df1[[matchcol]], df1 = x, df2 = y)
          colnames(dfdiffs)[1] <- matchcol
          diffrows <- signif(x, sigfigs) != signif(y, sigfigs)
          diffrows[is.na(diffrows)] <- TRUE
          dfdiffs <- dfdiffs[diffrows, ]
          diffs[[colnames(df1)[ii]]] <- dfdiffs
          details$NDifferent[[ii]] <- nrow(dfdiffs)
        }

      } else {

        if(identical(x, y)){
          details$TheSame[[ii]] <- TRUE
          details$Comment[[ii]] <- paste0("Non-numerical and identical")
          details$NDifferent[[ii]] <- 0
        } else {
          details$TheSame[[ii]] <- FALSE
          details$Comment[[ii]] <- paste0("Non-numerical and different")
          dfdiffs <- data.frame(MatchCol = df1[[matchcol]], df1 = x, df2 = y)
          colnames(dfdiffs)[1] <- matchcol
          dfdiffs <- dfdiffs[x != y, ]
          diffs[[colnames(df1)[ii]]] <- dfdiffs
          details$NDifferent[[ii]] <- nrow(dfdiffs)
        }
      }

    }

    list(Same = all(details$TheSame),
                Details = details,
                Differences = diffs)

  }

}


#' Replace multiple values in a data frame
#'
#' Given a data frame (or vector), this function replaces values according to a look up table or dictionary. In COINr this may
#' be useful for exchanging categorical data with numeric scores, prior to assembly. Or for changing codes.
#'
#' The lookup data frame must not have any duplicated values in the `old` column. This function looks for exact matches of
#' elements of the `old` column and replaces them with the corresponding value in the `new` column. For each row of `lookup`,
#' the class of the old value must match the class of the new value. This is to keep classes of data frames columns consistent.
#' If you wish to replace with a different class, you should convert classes in your data frame before using this function.
#'
#' @param df A data frame or a vector
#' @param lookup A data frame with columns `old` (the values to be replaced) and `new` the values to replace with. See details.
#'
#' @examples
#' # replace sub-pillar codes in ASEM indicator metadata
#' codeswap <- data.frame(old = c("Conn", "Sust"), new = c("SI1", "SI2"))
#' # swap codes in both iMeta
#' replace_df(ASEM_iMeta, codeswap)
#'
#' @return A data frame with replaced values
#'
#' @seealso
#' * [rank_df()] Replace numeric columns of a data frame with ranks.
#' * [round_df()] Replace numeric columns of a data frame with rounded values.
#' * [compare_df()] Detailed comparison of two similar data frames.
#'
#' @export

replace_df <- function(df, lookup){

  # if a vector is input, convert to data frame
  vecflag <- FALSE
  if(is.vector(df)){
    vecflag <- TRUE
    df <- data.frame(v1 = df)
  }

  # checks
  stopifnot(is.data.frame(df),
            is.data.frame(lookup),
            !(is.null(lookup$old)),
            !(is.null(lookup$new)),
            anyDuplicated(lookup$old) == 0)

  # replace each item one at a time
  for(ii in 1:nrow(lookup)){

    # check that the class of the old/new pair is the same
    if(class(lookup$old[ii]) != class(lookup$new[ii]) ){
      stop(paste0("Class difference detected in row ", ii, " of lookup. Old class is ", class(lookup$old[ii]), " but new class is ", class(lookup$new[ii]), "."))
    }

    # replace value
    df[df == lookup$old[ii]] <- lookup$new[ii]
  }

  # if it was a vector, convert back
  if(vecflag){
    df <- unlist(df, use.names = FALSE)
  }

  df

}


#' Round down a data frame
#'
#' Tiny function just to round down a data frame for display in a table, ignoring non-numeric columns.
#'
#' @param df A data frame to input
#' @param decimals The number of decimal places to round to (default 2)
#'
#' @examples
#' round_df( as.data.frame(matrix(runif(20),10,2)), decimals = 3)
#'
#' @seealso
#' * [rank_df()] Replace data frame numbers with ranks.
#'
#' @return A data frame, with any numeric columns rounded to the specified amount.
#'
#' @export

round_df <- function(df, decimals = 2){
  df <- data.frame(
    lapply(df, function(y) if(is.numeric(y)) round(y, decimals) else y)
  )
  rownames(df) <- NULL
  df
}


#' Round a data frame to specified significant figures
#'
#' Tiny function just to round down a data frame by significant figures for display in a table, ignoring non-numeric columns.
#'
#' @param df A data frame to input
#' @param digits The number of decimal places to round to (default 3)
#'
#' @examples
#' signif_df( as.data.frame(matrix(runif(20),10,2)), digits = 3)
#'
#' @seealso
#' * [rank_df()] Replace data frame numbers with ranks.
#'
#' @return A data frame, with any numeric columns rounded to the specified amount.
#'
#' @export

signif_df <- function(df, digits = 3){
  df <- data.frame(
    lapply(df, function(y) if(is.numeric(y)) signif(y, digits) else y)
  )
  rownames(df) <- NULL
  df
}
