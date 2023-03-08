# GENERAL UTILITY FUNCTIONS
# NONE OF THESE FUNCTIONS ARE EXPORTED

# Not in operator
#
# For convenience, rather than always `!(x, %in% y)`
#
# @param x A scalar or vector
# @param y A scalar or vector
#
# @return TRUE if x is not in y, FALSE otherwise
'%nin%' <- function(x,y){
  !('%in%'(x,y))
}


# rbind two lists with different names into a data frame
#
# Performs an `rbind()` operation on two named lists or vectors that do not need to share the same names, but
# will match the names and fill any missing cols with `NA`s.
#
# @param x1 A named list or named vector
# @param x2 Another named list or named vector
#
# @examples
# #
#
# @return Data frame
rbind_fill <- function(x1, x2){

  if(is.null(names(x1)) || is.null(names(x2))){
    stop("names of x1 or x2 is NULL")
  }

  # make to dfs
  x1 <- as.data.frame(as.list(x1))
  x2 <- as.data.frame(as.list(x2))

  # fill with NAs
  x1[setdiff(names(x2), names(x1))] <- NA
  x2[setdiff(names(x1), names(x2))] <- NA

  rbind(x1, x2)

}


# Remove empty components from list
#
# Short cut for removing any empty components of a list
#
# @param l A list
#
# @examples
# #
#
# @return List with empty bits removed
tidy_list <- function(l){
  l[lengths(l) > 0]
}


# Check availability of function
#
# Checks if a function is available, and returns an error if not.
#
# @param f_name A string to use to check whether a function exists with that name.
#
# @return Nothing or error
check_fname <- function(f_name){
  if(!(exists(f_name, mode = "function"))){
    stop("function '", f_name, "' not found. must be an accessible function.")
  }
}

# Set default arg
#
# A shortcut
#
# @param x The argument
# @param x_default The default to set
#
# @return the parameter
set_default <- function(x, x_default){
  if(is.null(x)){
    x_default
  } else {
    x
  }
}


# Data frame or matrix to long form
#
# This is a substitute function for tidyr's 'pivot_longer' to avoid dependencies, and behaves in more or
# less the same way.
#
# If `cols` is not specified, assumes a square correlation matrix to convert to long form. If `cols` is
# specified, this behaves like pivot_longer's "cols" argument.
#
# @param X A data frame or square correlation matrix
# @param cols Columns to pivot into longer format.
#
# @importFrom utils stack
#
# @return A long format data frame
lengthen <- function(X, cols = NULL){

  # make df
  X <- as.data.frame(X)

  if(!is.null(cols)){

    stopifnot(all(cols %in% names(X)))
    X_ <- X[cols]
    X <- X[names(X) %nin% cols]
    X$V_to_pivot <- rownames(X)

  } else {
    X_ <- X
  }

  # stack and add names
  X1 <- cbind(utils::stack(X_), rownames(X_))
  names(X1) <- c("Value", "V2", "V1")
  X1$V2 <- as.character(X1$V2)
  X1 <- rev(X1)

  if(!is.null(cols)){
    X1 <- merge(X, X1, by.x = "V_to_pivot", by.y = "V1", all = TRUE)
    X1 <- X1[names(X1) != "V_to_pivot"]
    names(X1)[names(X1) == "V2"] <- "name"
  }

  X1

}


# Make long df wide
#
# This is a quick function for making a long-format data frame wide. It is limited in scope, assumes
# that the input is a data frame with three columns: one of which is numeric, and the other two are
# character vectors. The numeric column will be widened, and the other two columns will be used
# for row and column names.
#
# @param X a long format data frame
#
# @importFrom utils unstack
#
# @return A wide format data frame
widen <- function(X){

  stopifnot(ncol(X) == 3)

  # make df
  X <- as.data.frame(X)

  # find numeric col
  num_cols <- sapply(X, is.numeric)
  if(sum(num_cols) > 1){
    stop("More than one numeric column found")
  }
  if(sum(num_cols) == 0){
    stop("No numeric columns found.")
  }

  # rearrange to get numeric col first
  X <- X[c(which(num_cols), which(!num_cols))]

  # order
  X <- X[order(X[[3]], X[[2]]),]

  # unstack and add row names
  Xw <- utils::unstack(X[1:2])
  row.names(Xw) <- unique(X[[3]])

  Xw
}


#' Convert iCodes to iNames
#'
#' @param coin A coin
#' @param iCodes A vector of iCodes
#'
#' @return Vector of iNames
#' @export
icodes_to_inames <- function(coin, iCodes){

  stopifnot(is.coin(coin))

  iMeta <- coin$Meta$Ind

  stopifnot(all(iCodes %in% iMeta$iCode))

  iMeta$iName[match(iCodes, iMeta$iCode)]

}


# Splits data frame into numeric and non-numeric columns
#
# @param x A data frame with numeric and non-numeric columns.
#
# @return A list with `.$not_numeric` containing a data frame with non-numeric columns, and `.$numeric` being
# a data frame containing only numeric columns.
#
# @examples
# #
split_by_numeric <- function(x){

  stopifnot(is.data.frame(x))

  # numeric cols
  numeric_cols <- sapply(x, is.numeric)

  if(sum(numeric_cols) == 0){
    stop("No numeric cols found in the data frame.")
  }

  list(not_numeric = x[!numeric_cols],
       numeric = x[numeric_cols])

}

# this function adjusts an iData dataset by directions, this is for use e.g.
# in correlation plotting.
# Just works with in-coin directions at the moment.
# iData can have non-numeric columns like uCode, uName etc, but any numeric
# cols will be required to have a corresponding direction entry in iMeta.
directionalise <- function(iData, coin){

  imeta <- coin$Meta$Ind[coin$Meta$Ind$Type == "Indicator", ]

  df_out <- lapply(names(iData), function(iCode){

    x <- iData[[iCode]]

    if(is.numeric(x)){
      if(iCode %nin% imeta$iCode){
        stop("Name of numeric column in iData does not have an entry in iMeta found in coin. Column: ", iCode)
      }
      iData[iCode]*imeta$Direction[imeta$iCode == iCode]
    } else {
      x
    }

  })
  df_out <- as.data.frame(df_out)

  stopifnot(identical(names(df_out), names(iData)))

  df_out

}

# X is a df
# cols specifies the names of TWO columns in X
# from which to remove duplicate pairs
remove_duplicate_corrs <- function(X, cols){

  X1 = X[,cols]
  duplicated_rows <- duplicated(t(apply(X1, 1, sort)))
  X[!duplicated_rows, ]

}
