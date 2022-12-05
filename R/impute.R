# IMPUTATION

#' Impute data sets in a purse
#'
#' This function imputes the target data set `dset` in each coin using the imputation function `f_i`. This is performed
#' in the same way as the coin method [Impute.coin()], but with one "special case" for panel data. If `f_i = "impute_panel`,
#' the data sets inside the purse are imputed using the last available data point, using the [impute_panel()]
#' function. In this case, coins are not imputed individually, but treated as a single data set. In this
#' case, optionally set `f_i_para = list(max_time = .)` where `.` should be substituted with the maximum
#' number of time points to search backwards for a non-`NA` value. See [impute_panel()] for more details.
#' No further arguments need to be passed to [impute_panel()]. See `vignette("imputation")` for more
#' details. See also [Impute.coin()] documentation.
#'
#' @param x A purse object
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param f_i An imputation function. For the "purse" class, if `f_i = "impute_panel` this is a special
#' case: see details.
#' @param f_i_para Further arguments to pass to `f_i`, other than `x`. See details.
#' @param impute_by Specifies how to impute: if `"column"`, passes each column (indicator) separately as a numerical
#' vector to `f_i`; if `"row"`, passes each *row* separately; and if `"df"` passes the entire data set (data frame) to
#' `f_i`. The function called by `f_i` should be compatible with the type of data passed to it.
#' @param group_level A level of the framework to use for grouping indicators. This is only
#' relevant if `impute_by = "row"` or `"df"`. In that case, indicators will be split into their groups at the
#' level specified by `group_level`, and imputation will be performed across rows of the group, rather
#' than the whole data set. This can make more sense because indicators within a group are likely to be
#' more similar.
#' @param use_group Optional grouping variable name to pass to imputation function if this supports group
#' imputation.
#' @param normalise_first Logical: if `TRUE`, each column is normalised using a min-max operation before
#' imputation. By default this is `FALSE` unless `impute_by = "row"`. See details.
#' @param write_to Optional character string for naming the resulting data set in each coin. Data will be written to
#' `.$Data[[write_to]]`. Default is `write_to == "Imputed"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse with imputed data sets added to each coin.
#' @export
#'
#' @examples
#' # see vignette("imputation")
Impute.purse <- function(x, dset, f_i = NULL, f_i_para = NULL, impute_by = "column",
                         group_level = NULL, use_group = NULL, normalise_first = NULL,
                         write_to = NULL, ...){

  # input check
  check_purse(x)

  if(!is.null(f_i)){

    if(f_i == "impute_panel"){

      # this is a special case
      iDatas <- get_dset(x, dset)

      # impute
      l_imp <- impute_panel(iDatas, max_time = f_i_para$max_time)

      # extract imputed data
      iDatas_i <- l_imp$iData_imp

      # split by Time
      iDatas_i_l <- split(iDatas_i, iDatas$Time)

      # now write dsets to coins
      x$coin <- lapply(x$coin, function(coin){

        # get Time
        tt <- coin$Meta$Unit$Time[[1]]
        if(is.null(tt)){
          stop("Time index is NULL or not found in writing imputed data set to coin.")
        }

        if(is.null(write_to)){
          write_to <- "Imputed"
        }

        # isolate data from the time point
        iData_write <- iDatas_i_l[[which(names(iDatas_i_l) == tt)]]
        # remove Time column
        iData_write <- iData_write[names(iData_write) != "Time"]

        # write dset first
        coin <- write_dset(coin, iData_write, dset = write_to)

        # also write to log - we signal that coin can't be regenerated any more
        coin$Log$can_regen <- FALSE
        coin$Log$message <- "Coin was imputed inside a purse with using panel imputation. Cannot be regenerated."

        coin
      })

    } else {

      # apply unit screening to each coin
      x$coin <- lapply(x$coin, function(coin){
        Impute.coin(coin, dset = dset, f_i = f_i, f_i_para = f_i_para, impute_by = impute_by,
                    group_level = group_level, use_group = use_group,
                    normalise_first = normalise_first, out2 = "coin",
                    write_to = write_to)})

    }



  } else {

    # apply unit screening to each coin
    x$coin <- lapply(x$coin, function(coin){
      Impute.coin(coin, dset = dset, f_i = f_i, f_i_para = f_i_para, impute_by = impute_by,
                  group_level = group_level, use_group = use_group,
                  normalise_first = normalise_first, out2 = "coin",
                  write_to = write_to)
    })

  }


  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}


#' Impute a data set in a coin
#'
#' This imputes any `NA`s in the data set specified by `dset`
#' by invoking the function `f_i` and any optional arguments `f_i_para` on each column at a time (if
#' `impute_by = "column"`), or on each row at a time (if `impute_by = "row"`), or by passing the entire
#' data frame to `f_i` if `impute_by = "df"`.
#'
#' Clearly, the function `f_i` needs to be able to accept with the data class passed to it - if
#' `impute_by` is `"row"` or `"column"` this will be a numeric vector, or if `"df"` it will be a data
#' frame. Moreover, this function should return a vector or data frame identical to the vector/data frame passed to
#' it except for `NA` values, which can be replaced. The function `f_i` is not required to replace *all* `NA`
#' values.
#'
#' When imputing row-wise, prior normalisation of the data is recommended. This is because imputation
#' will use e.g. the mean of the unit values over all indicators (columns). If the indicators are on
#' very different scales, the result will likely make no sense. If the indicators are normalised first,
#' more sensible results can be obtained. There are two options to pre-normalise: first is by setting
#' `normalise_first = TRUE` - this is anyway the default if `impute_by = "row"`. In this case, you also
#' need to supply a vector of directions. The data will then be normalised using a min-max approach
#' before imputation, followed by the inverse operation to return the data to the original scales.
#'
#' Another approach which gives more control is to simply run [Normalise()] first, and work with the
#' normalised data from that point onwards. In that case it is better to set `normalise_first = FALSE`,
#' since by default if `impute_by = "row"` it will be set to `TRUE`.
#'
#' Checks are made on the format of the data returned by imputation functions, to ensure the
#' type and that non-`NA` values have not been inadvertently altered. This latter check is allowed
#' a degree of tolerance for numerical precision, controlled by the `sfigs` argument. This is because
#' if the data frame is normalised, and/or depending on the imputation function, there may be a very
#' small differences. By default `sfigs = 9`, meaning that the non-`NA` values pre and post-imputation
#' are compared to 9 significant figures.
#'
#' See also documentation for [Impute.data.frame()] and [Impute.numeric()] which are called by this function.
#'
#' @param x A coin class object
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param f_i An imputation function. See details.
#' @param f_i_para Further arguments to pass to `f_i`, other than `x`. See details.
#' @param impute_by Specifies how to impute: if `"column"`, passes each column (indicator) separately as a numerical
#' vector to `f_i`; if `"row"`, passes each *row* separately; and if `"df"` passes the entire data set (data frame) to
#' `f_i`. The function called by `f_i` should be compatible with the type of data passed to it.
#' @param use_group Optional grouping variable name to pass to imputation function if this supports group
#' imputation.
#' @param group_level A level of the framework to use for grouping indicators. This is only
#' relevant if `impute_by = "row"` or `"df"`. In that case, indicators will be split into their groups at the
#' level specified by `group_level`, and imputation will be performed across rows of the group, rather
#' than the whole data set. This can make more sense because indicators within a group are likely to be
#' more similar.
#' @param normalise_first Logical: if `TRUE`, each column is normalised using a min-max operation before
#' imputation. By default this is `FALSE` unless `impute_by = "row"`. See details.
#' @param out2 Either `"coin"` to return normalised data set back to the coin, or `df` to simply return a data
#' frame.
#' @param write_to Optional character string for naming the data set in the coin. Data will be written to
#' `.$Data[[write_to]]`. Default is `write_to == "Imputed"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated coin with imputed data set at `.$Data[[write_to]]`
#' @export
#'
#' @examples
#' #' # build coin
#' coin <- build_example_coin(up_to = "new_coin")
#'
#' # impute raw data set using population groups
#' # output to data frame directly
#' Impute(coin, dset = "Raw", f_i = "i_mean_grp",
#'                use_group = "Pop_group", out2 = "df")
#'
Impute.coin <- function(x, dset, f_i = NULL, f_i_para = NULL, impute_by = "column",
                        use_group = NULL, group_level = NULL, normalise_first = NULL, out2 = "coin",
                        write_to = NULL, ...){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")

  # GET DSET, DEFAULTS ------------------------------------------------------

  iData <- get_dset(coin, dset, use_group)
  iData_ <- iData[colnames(iData) %nin% c("uCode", use_group)]

  # if normalise_first not specified set TRUE if rowwise or FALSE for anything else
  if(is.null(normalise_first)){
    if(impute_by == "row"){
      normalise_first <- TRUE
    } else {
      normalise_first <- FALSE
    }
  }

  # get directions if needed: these are the directions in iMeta
  if(!is.null(normalise_first)){
    directions <- coin$Meta$Ind[c("iCode", "Direction")]
  }

  # add grouping as parameter if required
  if(!is.null(use_group)){
    if(is.null(f_i_para)){
      f_i_para <- list(f = iData[[use_group]])
    } else {
      f_i_para[["f"]] <- iData[[use_group]]
    }
  }

  # IMPUTE DATA ----------------------------------------------------------

  # first, we may need to split
  if(is.null(group_level)){

    directions <- directions$Direction[match(colnames(iData_), directions$iCode)]

    # no splitting here = easy
    iData_i <- Impute.data.frame(iData_, f_i = f_i, f_i_para = f_i_para,
                                  impute_by = impute_by,
                                  normalise_first = normalise_first,
                                  directions = directions)

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
      directions <- directions$Direction[match(colnames(dfi), directions$iCode)]
      Impute.data.frame(dfi, f_i = f_i, f_i_para = f_i_para,
                         impute_by = impute_by, normalise_first = normalise_first,
                         directions = directions)
    })

    # reassemble
    iData_i <- Reduce(cbind, iData_split_i)
    iData_i <- iData_i[colnames(iData_)]

    # check non-NAs have not changed
    x_check <- iData_i
    x_check[is.na(iData_)] <- NA

    if(!identical(x_check, iData_)){
      warning("Differences detected in non-NA values of imputed data frame.")
    }

  }


  # OUTPUT ------------------------------------------------------------------

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
#' Impute a data frame using any function, either column-wise, row-wise or by the whole data frame in one
#' shot.
#'
#' This function only accepts data frames with all numeric columns. It imputes any `NA`s in the data frame
#' by invoking the function `f_i` and any optional arguments `f_i_para` on each column at a time (if
#' `impute_by = "column"`), or on each row at a time (if `impute_by = "row"`), or by passing the entire
#' data frame to `f_i` if `impute_by = "df"`.
#'
#' Clearly, the function `f_i` needs to be able to accept with the data class passed to it - if
#' `impute_by` is `"row"` or `"column"` this will be a numeric vector, or if `"df"` it will be a data
#' frame. Moreover, this function should return a vector or data frame identical to the vector/data frame passed to
#' it except for `NA` values, which can be replaced. The function `f_i` is not required to replace *all* `NA`
#' values.
#'
#' When imputing row-wise, prior normalisation of the data is recommended. This is because imputation
#' will use e.g. the mean of the unit values over all indicators (columns). If the indicators are on
#' very different scales, the result will likely make no sense. If the indicators are normalised first,
#' more sensible results can be obtained. There are two options to pre-normalise: first is by setting
#' `normalise_first = TRUE` - this is anyway the default if `impute_by = "row"`. In this case, you also
#' need to supply a vector of directions. The data will then be normalised using a min-max approach
#' before imputation, followed by the inverse operation to return the data to the original scales.
#'
#' Another approach which gives more control is to simply run [Normalise()] first, and work with the
#' normalised data from that point onwards. In that case it is better to set `normalise_first = FALSE`,
#' since by default if `impute_by = "row"` it will be set to `TRUE`.
#'
#' Checks are made on the format of the data returned by imputation functions, to ensure the
#' type and that non-`NA` values have not been inadvertently altered. This latter check is allowed
#' a degree of tolerance for numerical precision, controlled by the `sfigs` argument. This is because
#' if the data frame is normalised, and/or depending on the imputation function, there may be a very
#' small differences. By default `sfigs = 9`, meaning that the non-`NA` values pre and post-imputation
#' are compared to 9 significant figures.
#'
#' @param x A data frame with only numeric columns.
#' @param f_i A function to use for imputation. By default, imputation is performed by simply substituting
#' the mean of non-`NA` values for each column at a time.
#' @param f_i_para Any additional parameters to pass to `f_i`, apart from `x`
#' @param impute_by Specifies how to impute: if `"column"`, passes each column separately as a numerical
#' vector to `f_i`; if `"row"`, passes each *row* separately; and if `"df"` passes the entire data frame to
#' `f_i`. The function called by `f_i` should be compatible with the type of data passed to it.
#' @param normalise_first Logical: if `TRUE`, each column is normalised using a min-max operation before
#' imputation. By default this is `FALSE` unless `impute_by = "row"`. See details.
#' @param directions A vector of directions: either -1 or 1 to indicate the direction of each column
#' of `x` - this is only used if `normalise_first = TRUE`. See details.
#' @param ... arguments passed to or from other methods.
#'
#' @return An imputed data frame
#' @export
#'
#' @examples
#' # a df of random numbers
#' X <- as.data.frame(matrix(runif(50), 10, 5))
#'
#' # introduce NAs (2 in 3 of 5 cols)
#' X[sample(1:10, 2), 1] <- NA
#' X[sample(1:10, 2), 3] <- NA
#' X[sample(1:10, 2), 5] <- NA
#'
#' # impute using column mean
#' Impute(X, f_i = "i_mean")
#'
#' # impute using row median (no normalisation)
#' Impute(X, f_i = "i_median", impute_by = "row",
#'        normalise_first = FALSE)
#'
#'
Impute.data.frame <- function(x, f_i = NULL, f_i_para = NULL, impute_by = "column",
                               normalise_first = NULL, directions = NULL, ...){

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

  # NORMALISE ---------------------------------------------------------------

  # if normalise_first not specified set TRUE if rowwise or FALSE for anything else
  if(is.null(normalise_first)){
    if(impute_by == "row"){
      normalise_first <- TRUE
    } else {
      normalise_first <- FALSE
    }
  }

  if(normalise_first){

    if(is.null(directions)){
      stop("To normalise the data you need to specify the 'directions' argument. If you are normalising
           by row, normalisation is recommended and is switched on by default.")
    }

    # checks
    stopifnot(length(directions) == ncol(x),
              all(directions %in% c(-1, 1)))

    # adjust for directions
    x_n <- as.data.frame(mapply(`*`, x, directions))

    # we will need the original min and max to reconstruct
    xmins <- sapply(x_n, min, na.rm = T)
    xmaxs <- sapply(x_n, max, na.rm = T)

    x_n <- as.data.frame(lapply(x_n, function(x){
      (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }))
  } else {
    x_n <- x
  }

  # IMPUTE ------------------------------------------------------------------

  if(impute_by == "df"){

    # require that first arg of f_i is "x"
    f_args <- formals(f_i)
    if(is.null(f_args)){
      stop("The function specified by f_i seems to have no input arguments!")
    } else {
      if(names(f_args)[1] != "x"){
        stop("The first argument of f_i must be called 'x'.")
      }
    }

    # function args
    f_args <- list(x = x_n)
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
    if(!identical(dim(x_imp), dim(x))){
      stop("Object returned by f_i has different dimensions from x.")
    }

  } else if (impute_by == "column") {

    if(is.null(f_i_para)){
      x_imp <- lapply(x_n, Impute.numeric, f_i)
    } else {
      x_imp <- lapply(x_n, Impute.numeric, f_i, f_i_para)
    }

  } else if (impute_by == "row"){

    # work row-wise with apply
    if(is.null(f_i_para)){
      x_imp <- apply(x_n, 1, Impute.numeric, f_i, simplify = FALSE)
    } else {
      x_imp <- apply(x_n, 1, Impute.numeric, f_i, f_i_para, simplify = FALSE)
    }

    # I have to reassemble carefully into a df
    x_imp <- Reduce(rbind, x_imp)

  }

  x_imp <- as.data.frame(x_imp)


  # UN-NORMALISE ------------------------------------------------------------

  if(normalise_first){

    # do normalisation backwards
    x_imp <- mapply(function(x, mn, mx, direc){
      xo <- x*(mx - mn) + mn
      xo*direc
    }, x_imp, xmins, xmaxs, directions)

    x_imp <- as.data.frame(x_imp)

  }


  # CHECKS AND OUTPUT -------------------------------------------------------------

  # manually reset row names
  row.names(x_imp) <- attr(x, "row.names")

  # check non-NAs have not changed
  x_check <- x_imp
  x_check[x_NAs] <- NA

  # to cross check we also have to make all cols from x numeric: some may be integer
  # columns that are changed to numeric during imputation. In this case, identical()
  # will return false even if the values are the same
  x <- as.data.frame(lapply(x, as.numeric))
  row.names(x) <- attr(x_imp, "row.names")

  if(!isTRUE(all.equal(x_check, x))){
    stop("Differences detected in non-NA values of imputed data frame.")
  }

  # replace non-NA values with original values to avoid any numerical precision issues
  x_imp[!x_NAs] <- x[!x_NAs]

  x_imp

}


#' Impute a numeric vector
#'
#' Imputes missing values in a numeric vector using a function `f_i`. This function should return a vector identical
#' to `x` except for `NA` values, which can be replaced. The function `f_i` is not required to replace *all* `NA`
#' values.
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
#' @param f_i A function that imputes missing values in a numeric vector. See descriotion and details.
#' @param f_i_para Optional further arguments to be passed to `f_i()`
#' @param ... arguments passed to or from other methods.
#'
#' @return An imputed numeric vector of the same length of `x`.
#' @export
#'
#' @examples
#' # a vector with a missing value
#' x <- 1:10
#' x[3] <- NA
#' x
#'
#' # impute using median
#' # this calls COINr's i_median() function
#' Impute(x, f_i = "i_median")
#'
Impute.numeric <- function(x, f_i = NULL, f_i_para = NULL, ...){

  # DEFAULTS ----------------------------------------------------------------

  f_i <- set_default(f_i, "i_mean")

  # require that first arg of f_i is "x"
  f_args <- formals(f_i)
  if(is.null(f_args)){
    stop("The function specified by f_i seems to have no input arguments!")
  } else {
    if(names(f_args)[1] != "x"){
      stop("The first argument of f_i must be called 'x'.")
    }
  }

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
  if(!identical(xi[!nas], x[!nas])){
    stop("One or more non-NA values of x has changed as a result of imputation. Check the behaviour of the imputation function.")
  }

  xi

}


#' Imputation of missing data
#'
#' This is a generic function with the following methods:
#'
#' * [Impute.numeric()]
#' * [Impute.data.frame()]
#' * [Impute.coin()]
#' * [Impute.purse()]
#'
#' See those methods for individual documentation.
#'
#' This function replaces the now-defunct `impute()` from COINr < v1.0.
#'
#' @param x Object to be imputed
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # See individual method documentation
#'
#' @return An object of the same class as `x`, but imputed.
#'
#' @export
Impute <- function(x, ...){
  UseMethod("Impute")
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
#' @param skip_f_na If `TRUE`, will work around any `NA`s in `f` (the corresponding values of `x` will be excluded from the imputation
#' and returned unaltered). Else if `FALSE`, will cause an error.
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' x <- c(NA, runif(10), NA)
#' f <- c(rep("a", 6), rep("b", 6))
#' i_mean_grp(x, f)
#'
i_mean_grp <- function(x, f, skip_f_na = TRUE){

  stopifnot(is.numeric(x),
            length(x)==length(f))

  # get any NAs in f
  fna <- is.na(f)
  if(sum(fna) == length(x)){
    stop("f must have at least one non-NA value")
  }

  # extract x values with non-NA f values
  if(skip_f_na){
    x_use <- x[!fna]
    f_use <- f[!fna]
  } else {
    if(any(fna)){
      stop("NAs found in f. If skip_f_na = TRUE f cannot contain any NAs.")
    }
    x_use <- x
    f_use <- f[!fna]
  }

  # split by factors, apply func then unsplit
  x_split <- split(x_use, f_use)
  x_split <- lapply(x_split, i_mean)
  x_imp <- unsplit(x_split, f_use)

  # reassemble and output
  x[!fna] <- x_imp
  x
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
#' i_median_grp(x, f)
#'
i_median_grp <- function(x, f){

  stopifnot(is.numeric(x),
            length(x)==length(f))

  # split by factors, apply func then unsplit
  x_split <- split(x, f)
  x_split <- lapply(x_split, i_median)
  unsplit(x_split, f)
}

#' Impute panel data
#'
#' Given a data frame of panel data, with a time-index column `time_col` and a unit ID column `unit_col`, imputes other
#' columns using the entry from the latest available time point.
#'
#' This presumes that there are multiple observations for each unit code, i.e. one per time point. It then searches for any missing values in the target year, and replaces them with the equivalent points
#' from previous time points. It will replace using the most recently available point.
#'
#' @param iData A data frame of indicator data, containing a time index column `time_col`, a unit code column `unit_col`,
#' and other numerical columns to be imputed.
#' @param time_col The name of a column found in `iData` to be used as the time index column. Must point to a numeric column.
#' @param unit_col The name of a column found in `iData` to be used as the unit code/ID column. Must point to a character column.
#' @param cols Optionally, a character vector of names of columns to impute. If `NULL` (default), all columns apart from `time_col` and
#' `unit_col` will be imputed where possible.
#' @param max_time The maximum number of time points to look backwards to impute from. E.g. if `max_time = 1`, if an
#' `NA` is found at time \eqn{t}, it will only look for a replacement value at \eqn{t-1} but not in any time points before that.
#' By default, searches all time points available.
#'
#' @examples
#' # Copy example panel data
#' iData_p <- ASEM_iData_p
#'
#' # we introduce two NAs: one for NZ in 2022 in LPI indicator
#' iData_p$LPI[iData_p$uCode == "NZ" & iData_p$Time == 2022] <-  NA
#' # one for AT, also in 2022, but for Flights indicator
#' iData_p$Flights[iData_p$uCode == "AT" & iData_p$Time == 2022] <- NA
#'
#' # impute: target only the two columns where NAs introduced
#' l_imp <- impute_panel(iData_p, cols = c("LPI", "Flights"))
#' # get imputed df
#' iData_imp <- l_imp$iData_imp
#'
#' # check the output is what we expect: both NAs introduced should now have 2021 values
#' iData_imp$LPI[iData_imp$uCode == "NZ" & iData_imp$Time == 2022] ==
#'   ASEM_iData_p$LPI[ASEM_iData_p$uCode == "NZ" & ASEM_iData_p$Time == 2021]
#'
#' iData_imp$Flights[iData_imp$uCode == "AT" & iData_imp$Time == 2022] ==
#'   ASEM_iData_p$Flights[ASEM_iData_p$uCode == "AT" & ASEM_iData_p$Time == 2021]
#'
#' @return A list containing:
#' * `.$iData_imp`: An `iData` format data frame with missing data imputed using previous time points (where possible).
#' * `.$DataT`: A data frame in the same format as `iData`, where each entry shows which time point each data point
#' came from.
#'
#' @export
impute_panel <- function(iData, time_col = NULL, unit_col = NULL, cols = NULL, max_time = NULL){


  # DEFAULTS ----------------------------------------------------------------

  if(is.null(time_col)){
    time_col <- "Time"
  }
  if(is.null(unit_col)){
    unit_col <- "uCode"
  }

  # CHECKS ------------------------------------------------------------------

  stopifnot(is.character(time_col),
            length(time_col) == 1,
            is.character(unit_col),
            length(unit_col) == 1)

  if(is.null(iData[[time_col]])){
    stop("No Time column found - use 'time_col' argument.")
  }
  if(is.null(iData[[unit_col]])){
    stop("No unit column found - use 'unit_col' argument.")
  }
  if(!is.numeric(iData[[time_col]])){
    stop("time_col refers to a non-numeric column")
  }
  if(!is.character(iData[[unit_col]])){
    stop("unit_col refers to a non-character column")
  }

  if(is.null(cols)){
    cols <- setdiff(names(iData), c(time_col, unit_col))
  }
  if(any(cols %nin% names(iData))){
    stop("One or more entries in 'cols' not found in names(iData).")
  }

  # not_numeric <- !sapply(iData[colnames(iData) %nin% c(time_col, unit_col)], is.numeric)
  # if(any(not_numeric)){
  #   stop("Non-numeric columns found other than time_col and unit_col - cannot impute.")
  # }

  # See what times are in iData
  yrs <- sort(unique(iData[[time_col]]), decreasing = TRUE)

  if(length(yrs)==1){
    stop("Cannot impute by latest time point because only one time point of data is available.")
  }

  # From here I will reduce iData to only the cols to be imputed. Will be put back later
  iData_orig <- iData
  iData <- iData[c(time_col, unit_col, cols)]

  # FUNC TO IMPUTE ----------------------------------------------------------
  # Function to impute a data set from a single time point, using previous years of data
  # I have to do this unit by unit... this is the safest way to deal with the possibility of
  # (a) different ordering of units
  # (b) subsets of units being available for different years
  # Since the each year of the data comes from the same table, column ordering is consistent so
  # I don't have to worry about that.

  impute_year <- function(use_year){

    # data from year
    iData_yr <- iData[iData[[time_col]] == use_year, ]

    # here I prep a data frame which will record the year used for each data point
    # we only make changes to this when a point is imputed
    DataYears <- iData_yr
    DataYears[colnames(DataYears) %nin% c(time_col, unit_col)] <- use_year
    DataYears[is.na(iData_yr)] <- NA

    # previous years
    olderyrs <- yrs[yrs < use_year]
    if(length(olderyrs) == 0){
      return(
        list(iData = iData_yr, DataT = DataYears)
      )
    }

    # only look up max number of points backwards
    if(!is.null(max_time)){
      olderyrs <- olderyrs[1:min(c(max_time, length(olderyrs)))]
    }

    # find ucodes of rows with nas
    nacodes <- iData_yr[[unit_col]][rowSums(is.na(iData_yr)) > 0]

    for(ucode in nacodes){

      irow <- iData_yr[iData_yr[[unit_col]] == ucode, ]

      # otherwise, we have to go year by year
      for(oldyr in olderyrs){

        # get row of same unit, for a previous year
        irowold <- iData[(iData[[time_col]] == oldyr) & (iData[[unit_col]] == ucode), ]
        # substitute in any missing values
        # first, get the equivalent entries of the old row (corresponding to NAs in new row)
        irowold_replace <- irowold[, as.logical(is.na(irow))]
        # and the names
        names_irowold <- names(irowold)[as.logical(is.na(irow))]
        # replace them into the new row
        irow[, names_irowold] <- irowold_replace
        # find which indicators were imputed here
        ind_imp <- names_irowold[!as.logical(is.na(irowold_replace))]
        # record what happened in datayears
        DataYears[DataYears[[unit_col]] == ucode, colnames(DataYears) %in% ind_imp] <- oldyr
        # check if we need to carry on
        if(all(!is.na(irow))){break}

      }

      # replace with imputed row
      iData_yr[iData_yr[[unit_col]] == ucode, ] <- irow

    }

    # output list
    list(iData = iData_yr, DataT = DataYears)

  }

  # apply function to all years of data
  l_imp <- lapply(yrs, impute_year)

  # reassemble data frame
  iData_imp <- lapply(rev(l_imp),  `[[`, "iData")
  iData_imp <- Reduce(rbind, iData_imp)

  stopifnot(nrow(iData_imp) == nrow(iData),
            ncol(iData_imp) == ncol(iData))

  # get data times
  DataT <- lapply(l_imp,  `[[`, "DataT")
  DataT <- Reduce(rbind, DataT)

  # return iData to its full form if cols was specified
  iData_imp_full <- iData_orig
  iData_imp_full[c(time_col, unit_col, cols)] <- iData_imp[c(time_col, unit_col, cols)]

  # return imputed data
  list(iData_imp = iData_imp_full,
       DataT = DataT)

}
