# IMPUTATION


#' Impute a data set in a coin
#'
#' @param x A coin class object
#' @param dset A data set within the coin
#' @param f_i An imputation function
#' @param f_i_para Further arguments to pass to `f_i`
#' @param impute_by s
#' @param group_level xx
#' @param norm_specs xx
#' @param ... further arguments to be passed to [normalise()]
#' @param out2 Either `"coin"` to return normalised data set back to the coin, or `df` to simply return a data
#' frame.
#' @param write_to Optional character string for naming the data set in the coin. Data will be written to
#' `.$Data[[write_to]]`. Default is `write_to == "Imputed"`.
#'
#' @return
#' @export
#'
#' @examples
#' #
impute2.coin <- function(x, dset = NULL, f_i = NULL, f_i_para = NULL, impute_by = "column",
                         group_level = NULL, norm_specs = NULL, ..., out2 = "coin",
                         write_to = NULL){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")

  # GET DSET, DEFAULTS ------------------------------------------------------

  iData <- get_dset(coin, dset)
  iData_ <- iData[colnames(iData) != "uCode"]


  # NORMALISE ---------------------------------------------------------------

  # to finish


  # IMPUTE DATA ----------------------------------------------------------

  # first, we may need to split
  if(is.null(group_level)){

    # no splitting here = easy
    iData_i <- impute2.data.frame(iData_, f_i = f_i, f_i_para = f_i_para,
                                  impute_by = impute_by)

  } else {

    if(group_level %nin% 1:x$Meta$maxlev){
      stop("group_level is out of range: must be between 1 and max level.")
    }

    # we split the data set based on its grouping at a given level
    # get lineage
    lin <- coin$Meta$Lineage
    # make sure lineage is in the same order as colnames of data
    lin <- lin[match(colnames(iData_), lin$Indicator), ]
    # this is our factor variable for splitting cols
    f <- lin[[group_level]]
    # now split
    iData_split <- split.default(iData_, f)

    # now do the imputation
    iData_split_i <- lapply(iData_split, function(dfi){
      impute2.data.frame(dfi, f_i = f_i, f_i_para = f_i_para,
                         impute_by = impute_by)
    })

    # reassemble
    iData_i <- Reduce(cbind, iData_split_i)
    iData_i <- iData_i[colnames(iData_)]

    # check non-NAs have not changed
    x_check <- iData_i
    iData_i[is.na(iData_)] <- NA

    if(!identical(iData_i, iData_)){
      warning("Differences detected in non-NA values of imputed data frame.")
    }

  }

  # reunite with uCode col
  iData_i <- cbind(uCode = iData$uCode, iData_i)

  # output list
  if(out2 == "df"){
    iData_i
  } else {
    if(is.null(write_to)){
      write_to <- "Imputed"
    }
    write_dset(coin, iData_i, dset = write_to)
  }

}


#' Impute a data frame
#'
#' Impute a data frame using any function, either columnwise, rowwise or by the whole data frame in one
#' shot.
#'
#' This function only accepts data frames with all numeric columns. It imputes any NAs in the data frame
#' by invoking the function `f_i` and any optional arguments `f_i_para` on each column at a time (if
#' `impute_by = "column"`), or on each row at a time (if `impute_by = "row"`), or by passing the entire
#' data frame to `f_i` if `impute_by = "df"`.
#'
#' Clearly, the function `f_i` needs to be able to accept with the data class passed to it - if
#' `impute_by` is `"row"` or `"column"` this will be a numeric vector, or if `"df"` it will be a data
#' frame. Checks are made on the format of the data returned by imputation functions, to ensure the
#' type and that non-`NA` values have not been inadvertently altered.
#'
#' @param x A data frame with only numeric columns.
#' @param f_i A function to use for imputation. By default, imputation is performed by simply substituting
#' the mean of non-`NA` values for each column at a time.
#' @param f_i_para Any additional parameters to pass to `f_i`, apart from `x`
#' @param impute_by Specifies how to impute: if `"column"`, passes each column separately as a numerical
#' vector to `f_i`; if `"row"`, passes each *row* separately; and if `"df"` passes the entire data frame to
#' `f_i`. The function called by `f_i` should be compatible with the type of data passed to it.
#'
#' @return
#' @export
#'
#' @examples
#' #
impute2.data.frame <- function(x, f_i = NULL, f_i_para = NULL, impute_by = "column"){

  # CHECKS ------------------------------------------------------------------

  stopifnot(impute_by %in% c("column", "row", "df"))

  # check for non-numeric cols
  non_numeric <- !sapply(x, is.numeric)

  if(any(non_numeric)){
    stop("Non-numeric columns detected in x. Please remove these columns to use this function.")
  }

  # check for NAs
  x_NAs <- is.na(x)

  # if there are no NAs, just return df as it was
  if(sum(x_NAs) == 0){
    return(x)
  }

  # IMPUTE ------------------------------------------------------------------

  if(impute_by == "df"){

    # function args
    f_args <- list(x = x)
    if(!is.null(f_i_para)){
      if(!is.list(f_i_para)){
        stop("f_i_para must be a list")
      }
      f_args <- c(f_args, f_i_para)
    }

    x_imp <- do.call(what = f_i, args = f_args)

    # Checks
    if(!is.data.frame(x_imp)){
      stop("Object returned by f_i is not a data frame.")
    }
    if(dim(x_imp) != dim(x)){
      stop("Object returned by f_i has different dimensions from x.")
    }

  } else if (impute_by == "column") {

    if(is.null(f_i_para)){
      x_imp <- lapply(x, impute2.numeric, f_i)
    } else {
      x_imp <- lapply(x, impute2.numeric, f_i, f_i_para)
    }

  } else if (impute_by == "row"){

    # work row-wise with apply
    if(is.null(f_i_para)){
      x_imp <- apply(x, 1, impute2.numeric, f_i, simplify = FALSE)
    } else {
      x_imp <- apply(x, 1, impute2.numeric, f_i, f_i_para, simplify = FALSE)
    }

    # I have to reassemble carefully into a df
    x_imp <- Reduce(rbind, x_imp)

  }

  x_imp <- as.data.frame(x_imp)


  # OUTPUT ------------------------------------------------------------------

  # manually reset row names
  row.names(x_imp) <- attr(x, "row.names")

  # check non-NAs have not changed
  x_check <- x_imp
  x_check[x_NAs] <- NA

  if(!identical(x_check, x)){
    warning("Differences detected in non-NA values of imputed data frame.")
  }

  x_imp

}


#' Impute a numeric vector
#'
#' This calls the function `f_i()`, with optionally further arguments `f_i_para`, to impute any missing
#' values found in `x`. By default, `f_i = "i_mean()"`, which simply imputes `NA`s with the mean of the
#' non-`NA` values in `x`.
#'
#' You could also use one of the imputation functions directly (such as [i_mean()]). However, this
#' function offers a few extra advantages, such as checking the input and output formats, and making
#' sure the resulting imputed vector agrees with the input. It will also skip imputation entirely if
#' there are no `NA`s at all.
#'
#' @param x A numeric vector, possibly with `NA` values to be imputed.
#' @param f_i A function that imputes missing values in a numeric vector
#' @param f_i_para Optional further arguments to be passed to `f_i()`
#'
#' @return An imputed numeric vector of the same length of `x`.
#' @export
impute2.numeric <- function(x, f_i = NULL, f_i_para = NULL){


  # DEFAULTS ----------------------------------------------------------------

  f_i <- set_default(f_i, "i_mean")

  # function args
  f_args <- list(x = x)
  if(!is.null(f_i_para)){
    if(!is.list(f_i_para)){
      stop("f_n_para must be a list")
    }
    f_args <- c(f_args, f_i_para)
  }

  # IMPUTE ---------------------------------------------------------------

  nas <- is.na(x)

  if(sum(nas) == length(x)){
    stop("Input is all NAs - cannot impute.")
  }

  # call imputation function
  # if "none" or there are no NAs we skip entirely
  if((f_i == "none") | (length(nas) == 0)){
    return(x)
  } else {
    xi <- do.call(what = f_i, args = f_args)
  }

  # CHECK and OUTPUT --------------------------------------------------------

  if(length(xi) != length(x)){
    stop("length of imputed vector not equal to length of x")
  }
  if(!is.numeric(xi)){
    stop("imputed vector is not numeric")
  }
  if(any(xi[!nas] != xi[!nas])){
    stop("One or more non-NA values of x has changed as a result of imputation. Check the behaviour of the imputation function.")
  }

  xi

}


#' Impute indicator data sets
#'
#' @param x Object to be imputed
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' #
#'
#' @return An object of the same class as `x`, but imputed.
#'
#' @export
impute2 <- function(x, ...){
  UseMethod("impute2")
}

#' Impute by mean
#'
#' Replaces `NA`s in a numeric vector with the mean of the non-`NA` values.
#'
#' @param x A numeric vector
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x <- c(1,2,3,4, NA)
#' i_mean(x)
#'
i_mean <- function(x){

  stopifnot(is.numeric(x))
  # get mean
  mx <- mean(x, na.rm = TRUE)
  # replace by mean
  x[is.na(x)] <- mx
  x
}

#' Impute by median
#'
#' Replaces `NA`s in a numeric vector with the median of the non-`NA` values.
#'
#' @param x A numeric vector
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x <- c(1,2,3,4, NA)
#' i_median(x)
#'
i_median <- function(x){

  stopifnot(is.numeric(x))
  # get mean
  mx <- stats::median(x, na.rm = TRUE)
  # replace by mean
  x[is.na(x)] <- mx
  x
}

#' Impute by group mean
#'
#' Replaces `NA`s in a numeric vector with the grouped arithmetic means of the non-`NA` values.
#' Groups are defined by the `f` argument.
#'
#' @param x A numeric vector
#' @param f A grouping variable, of the same length of `x`, that specifies the group that each value
#' of `x` belongs to. This will be coerced to a factor.
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x <- c(NA, runif(10), NA)
#' f <- c(rep("a", 6), rep("b", 6))
#' i_mean_grp(x)
#'
i_mean_grp <- function(x, f){

  stopifnot(is.numeric(x),
            length(x)==length(f))

  # split by factors, apply func then unsplit
  x_split <- split(x, f)
  x_split <- lapply(x_split, i_mean)
  unsplit(x_split, f)
}

#' Impute by group median
#'
#' Replaces `NA`s in a numeric vector with the grouped medians of the non-`NA` values.
#' Groups are defined by the `f` argument.
#'
#' @param x A numeric vector
#' @param f A grouping variable, of the same length of `x`, that specifies the group that each value
#' of `x` belongs to. This will be coerced to a factor.
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x <- c(NA, runif(10), NA)
#' f <- c(rep("a", 6), rep("b", 6))
#' i_median_grp(x)
#'
i_median_grp <- function(x, f){

  stopifnot(is.numeric(x),
            length(x)==length(f))

  # split by factors, apply func then unsplit
  x_split <- split(x, f)
  x_split <- lapply(x_split, i_median)
  unsplit(x_split, f)
}
