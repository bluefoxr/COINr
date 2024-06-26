#' Aggregate indicators
#'
#' Aggregates indicators following the structure specified in `iMeta`, for each coin inside the purse.
#' See [Aggregate.coin()], which is applied to each coin, for more information
#'
#' @param x A purse-class object
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param f_ag The name of an aggregation function, a string. This can either be a single string naming
#' a function to use for all aggregation levels, or else a character vector of function names of length `n-1`, where `n` is
#' the number of levels in the index structure. In this latter case, a different aggregation function may be used for each level
#' in the index: the first in the vector will be used to aggregate from Level 1 to Level 2, the second from Level 2 to Level 3, and
#' so on.
#' @param w An optional data frame of weights. If `f_ag` does not require accept weights, set to `"none"`. Alternatively, can be the
#' name of a weight set found in `.$Meta$Weights`. This can also be specified as a list specifying the aggregation weights for each
#' level, in the same way as the previous parameters.
#' @param f_ag_para Optional parameters to pass to `f_ag`, other than `x` and `w`. As with `f_ag`, this can specified to have different
#' parameters for each aggregation level by specifying as a nested list of length `n-1`. See details.
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' within an aggregation group has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values. Can also be specified as a vector of length `n-1`, where `n` is
#' the number of levels in the index structure, to specify different data availability thresholds by level.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details. Can also be specified as a logical vector of length `n-1`, where `n` is
#' the number of levels in the index structure.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Aggregated"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse with new treated data sets added at `.$Data[[write_to]]` in each coin.
#' @export
#'
#' @examples
#' # build example purse up to normalised data set
#' purse <- build_example_purse(up_to = "Normalise", quietly = TRUE)
#'
#' # aggregate using defaults
#' purse <- Aggregate(purse, dset = "Normalised")
#'
Aggregate.purse <- function(x, dset, f_ag = NULL, w = NULL, f_ag_para = NULL, dat_thresh = NULL,
                             write_to = NULL, by_df = FALSE, ...){

  # input check
  check_purse(x)

  # apply unit screening to each coin
  x$coin <- lapply(x$coin, function(coin){
    Aggregate.coin(coin, dset, f_ag = f_ag, w = w, f_ag_para = f_ag_para, dat_thresh = dat_thresh,
                    out2 = "coin", write_to = write_to, by_df = by_df)
  })
  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}


#' Aggregate indicators in a coin
#'
#' Aggregates a named data set specified by `dset` using aggregation function(s) `f_ag`, weights `w`, and optional
#' function parameters `f_ag_para`. Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends.
#'
#' When `by_df = FALSE`, aggregation is performed row-wise using the function `f_ag`, such that for each row `x_row`, the output is
#' `f_ag(x_row, f_ag_para)`, and for the whole data frame, it outputs a numeric vector. Otherwise if `by_df = TRUE`,
#' the entire data frame of each indicator group is passed to `f_ag`.
#'
#' The function `f_ag` must be supplied as a string, e.g. `"a_amean"`, and it must take as a minimum an input
#' `x` which is either a numeric vector (if `by_df = FALSE`), or a data frame (if `by_df = TRUE`). In the former
#' case `f_ag` should return a single numeric value (i.e. the result of aggregating `x`), or in the latter case
#' a numeric vector (the result of aggregating the whole data frame in one go).
#'
#' Weights are passed to the function `f_ag` as an argument named `w`. This means that the function should have
#' arguments that look like `f_ag(x, w, ...)`, where `...` are possibly other input arguments to the function. If the
#' aggregation function doesn't use weights, you can set `w = "none"`, and no weights will be passed to it.
#'
#' `f_ag` can optionally have other parameters, apart from `x` and `w`, specified as a list in `f_ag_para`.
#'
#' The aggregation specifications can be set to be different for each level of aggregation: the arguments `f_ag`,
#' `f_ag_para`, `dat_thresh`, `w` and `by_df` can all be optionally specified as vectors or lists of length n-1, where
#' n is the number of levels in the index. In this case, the first value in each vector/list will be used for the first
#' round of aggregation, i.e. from indicators to the aggregates at level 2. The next will be used to aggregate from
#' level 2 to level 3, and so on.
#'
#' When different functions are used for different levels, it is important to get the list syntax correct. For example, in a case with
#' three aggregations using different functions, say we want to use `a_amean()` for the first two levels, then a custom
#' function `f_cust()` for the last. `f_cust()` has some additional parameters `a` and `b`. In this case, we would specify e.g.
#' `f_ag_para = list(NULL, NULL, list(a = 2, b = 3))` - this is becauase `a_amean()` requires no additional parameters, so
#' we pass `NULL`.
#'
#' Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends. To see a list browse COINr functions alphabetically or
#' type `a_` in the R Studio console and press the tab key (after loading COINr), or see the [online documentation](https://bluefoxr.github.io/COINr/articles/aggregate.html#coinr-aggregation-functions).
#'
#' Optionally, a data availability threshold can be assigned below which the aggregated value will return
#' `NA` (see `dat_thresh` argument). If `by_df = TRUE`, this will however be ignored because aggregation is not
#' done on individual rows. Note that more complex constraints could be built into `f_ag` if needed.
#'
#' @param x A coin class object.
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param f_ag The name of an aggregation function, a string. This can either be a single string naming
#' a function to use for all aggregation levels, or else a character vector of function names of length `n-1`, where `n` is
#' the number of levels in the index structure. In this latter case, a different aggregation function may be used for each level
#' in the index: the first in the vector will be used to aggregate from Level 1 to Level 2, the second from Level 2 to Level 3, and
#' so on.
#' @param w An optional data frame of weights. If `f_ag` does not require accept weights, set to `"none"`. Alternatively, can be the
#' name of a weight set found in `.$Meta$Weights`. This can also be specified as a list specifying the aggregation weights for each
#' level, in the same way as the previous parameters.
#' @param f_ag_para Optional parameters to pass to `f_ag`, other than `x` and `w`. As with `f_ag`, this can specified to have different
#' parameters for each aggregation level by specifying as a nested list of length `n-1`. See details.
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' within an aggregation group has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values. Can also be specified as a vector of length `n-1`, where `n` is
#' the number of levels in the index structure, to specify different data availability thresholds by level.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details. Can also be specified as a logical vector of length `n-1`, where `n` is
#' the number of levels in the index structure.
#' @param out2 Either `"coin"` (default) to return updated coin or `"df"` to output the aggregated data set.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Aggregated"`.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # build example up to normalised data set
#' coin <- build_example_coin(up_to = "Normalise")
#'
#' # aggregate normalised data set
#' coin <- Aggregate(coin, dset = "Normalised")
#'
#' @return An updated coin with aggregated data set added at `.$Data[[write_to]]` if `out2 = "coin"`,
#' else if `out2 = "df"` outputs the aggregated data set as a data frame.
#'
#' @export
Aggregate.coin <- function(x, dset, f_ag = NULL, w = NULL, f_ag_para = NULL, dat_thresh = NULL,
                            by_df = FALSE, out2 = "coin", write_to = NULL, ...){

  # Write to Log ------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")


  # Check and set by_df -----------------------------------------------------

  nlev <- max(coin$Meta$Ind$Level, na.rm = TRUE)

  stopifnot(is.logical(by_df))

  if(length(by_df) == 1){
    by_dfs <- rep(by_df, nlev-1)
  } else if (length(by_df) != (nlev -1)) {
    stop("by_df must have either length 1 (same for all levels) or length equal to (number of levels - 1), in your case: ", nlev-1)
  } else {
    by_dfs <- by_df
  }

  # CHECK AND SET f_ag ------------------------------------------------------

  # default and check
  if(is.null(f_ag)){
    f_ag <- "a_amean"
    f_ag_para <- NULL
  } else {
    if(!is.character(f_ag)){
      stop("f_ag must be specified as a character string or vector (function name(s) in inverted commas).")
    }
  }
  stopifnot(length(f_ag) > 0)

  # if same for all levels, repeat
  if(length(f_ag) == 1){
    f_ags <- rep(f_ag, nlev - 1)
  } else {
    if(length(f_ag) != (nlev - 1)){
      stop("f_ag must have either length 1 (same function for all levels) or length equal to (number of levels - 1), in your case: ", nlev-1)
    }
    f_ags <- f_ag
  }

  # CHECK AND SET w ---------------------------------------------------------

  # If weights are supplied we have to see what kind of thing it is. This gets
  # a bit messy due to the number of different input types that w can take, including
  # varying by level.

  # NULL indicates that we should use metadata weights
  if(!is.null(w)){

    # case where weights is invariant at all levels
    if(length(w) == 1 || is.data.frame(w)){

      if(is.data.frame(w)){

        stopifnot(exists("iCode", w),
                  exists("Weight", w))
        w1 <- rep(list(w), nlev - 1)

      } else if(is.character(w)){

        if(w != "none"){

          # we look for a named weight set
          w1 <- coin$Meta$Weights[[w]]
          if(is.null(w1)){
            stop("Weight set with name '", w, "' not found in .$Meta$Weights.")
          }
          stopifnot(is.data.frame(w1),
                    exists("iCode", w1),
                    exists("Weight", w1))
          # copy for all levels
          w1 <- rep(list(w1), nlev - 1)

        } else {
          # convert w1 to NULL - means no weights will be passed to function
          w1 <- rep(list(NULL), nlev - 1)
          #w1 <- NULL
        }
      } else {
        stop("w must be either a string indicating a name of a weight set, or a data frame of weights, or 'none', or NULL (to use weights from metadata).")
      }

    } else {

      # w has been specified as a list of length > 1
      if(!is.list(w)){
        stop("If length(w) > 1, it must be specified as a list.")
      }
      if(length(w) != nlev - 1){
        stop("If w is specified as a list with length > 1, it must have length equal to the number of aggregation levels minus one.")
      }

      # Process each entry in the list separately
      w1 <- lapply(w, function(wi){

        if(is.data.frame(wi)){

          stopifnot(exists("iCode", wi),
                    exists("Weight", wi))

        } else if (is.character(wi)){

          if(wi != "none"){

            # we look for a named weight set
            wi <- coin$Meta$Weights[[wi]]
            if(is.null(wi)){
              stop("Weight set with name '", wi, "' not found in .$Meta$Weights.")
            }
            stopifnot(is.data.frame(wi),
                      exists("iCode", wi),
                      exists("Weight", wi))

          } else {
            # convert w1 to NULL - means no weights will be passed to function
            wi <- NULL
          }
        } else if (is.null(wi)){

          wi <- coin$Meta$Ind[c("iCode", "Weight")]

        } else {
          stop("Entries in w must be either a data frame of weights, name of weight set, 'none' or NULL.")
        }
        wi
      })

    }

  } else{
    # if w was NULL, get from metadata
    w1 <- rep(list(coin$Meta$Ind[c("iCode", "Weight")]), nlev - 1)
  }

  # from this point, w1 should be a list of either data frames or NULLs
  stopifnot(
    all(sapply(w1, is.data.frame) | sapply(w1, is.null))
  )

  # CHECK AND SET f_ag_para -------------------------------------------------

  # if f_ag_para is NULL, repeat for all levs
  if(!is.null(f_ag_para)){
    if(!is.list(f_ag_para)){
      stop("f_ag_para must be specified as a list or list of lists")
    }
    stopifnot(length(f_ag_para) > 0)

    # if same for all levels, repeat
    if(length(f_ag_para) == 1){
      f_ag_paras <- rep(list(f_ag_para), nlev - 1)
    } else {
      if(length(f_ag_para) != (nlev - 1)){
        stop("f_ag_para must have either length 1 (same parameters for all levels) or length equal to number of levels - in your case: ", nlev)
      }
      f_ag_paras <- f_ag_para
    }
  } else {
    f_ag_paras <- rep(list(NULL), nlev - 1)
  }

  # Other Prep --------------------------------------------------------------------

  if(is.null(dat_thresh)){
    dat_threshs <- rep(list(NULL), nlev - 1)
  } else {
    if(!is.numeric(dat_thresh)){
      stop("dat_thresh must be a numeric value or vector of length (number of levels - 1) - in your case: ", nlev)
    }
    if(any((dat_thresh < 0) | (dat_thresh > 1))){
      stop("dat_thresh must only contain numeric values between 0 and 1.")
    }
    if(length(dat_thresh) == 1){
      dat_threshs <- rep(dat_thresh, nlev - 1)
    } else {
      if(length(dat_thresh) != (nlev - 1)){
        stop("dat_thresh must have either length 1 (same for all levels) or length equal to number of levels - in your case: ", nlev)
      }
      dat_threshs <- dat_thresh
    }
  }

  # Aggregate ---------------------------------------------------------------
  # Here we apply the aggregation by level

  # get data (also performing checks)
  indat <- get_dset(coin, dset)
  # get metadata
  imeta <- coin$Meta$Ind[!is.na(coin$Meta$Ind$Level), ]

  # Function that aggregates from Level = lev to the next level up
  # calls the function specified by f_ag.
  aggregate_level <- function(lev){

    # filter metadata to level
    imeta_l <- imeta[imeta$Level == (lev-1), ]

    if(is.null(w1[[lev-1]])){
      aggs <- tapply(imeta_l$iCode, imeta_l$Parent, function(codes){
        # call func
        do.call("Aggregate",
                list(x = indat_ag[codes],
                     f_ag = f_ags[lev-1],
                     f_ag_para = f_ag_paras[[lev-1]],
                     dat_thresh = dat_threshs[[lev-1]],
                     by_df = by_dfs[lev-1]))
      })
    } else {
      aggs <- tapply(imeta_l$iCode, imeta_l$Parent, function(codes){
        # get weights
        wts <- w1[[lev-1]]$Weight[match(codes, w1[[lev-1]]$iCode)]
        # call func
        do.call("Aggregate",
                list(x = indat_ag[codes],
                     f_ag = f_ags[lev-1],
                     f_ag_para = c(list(w = wts), f_ag_paras[[lev-1]]),
                     dat_thresh = dat_threshs[[lev-1]],
                     by_df = by_dfs[lev-1]))
      })
    }

    if(is.list(aggs)){
      # aggs comes out as a list of vectors, have to make to df
      as.data.frame(do.call(cbind, aggs))
    } else if (is.numeric(aggs)){
      # in this case there is only one row, and it comes out as an array which needs to be converted
      as.data.frame(t(aggs))
    }

  }

  indat_ag <- indat

  # run the above function for each level
  for(lev in 2:nlev){
    indat_ag <- cbind(indat_ag, aggregate_level(lev))
  }

  # Output ------------------------------------------------------------------

  # output list
  if(out2 == "df"){
    indat_ag
  } else {
    if(is.null(write_to)){
      write_to <- "Aggregated"
    }
    write_dset(coin, indat_ag, dset = write_to)
  }


}


#' Aggregate data frame
#'
#' Aggregates a data frame into a single column using a specified function. Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends.
#'
#' Aggregation is performed row-wise using the function `f_ag`, such that for each row `x_row`, the output is
#' `f_ag(x_row, f_ag_para)`, and for the whole data frame, it outputs a numeric vector. The data frame `x` must
#' only contain numeric columns.
#'
#' The function `f_ag` must be supplied as a string, e.g. `"a_amean"`, and it must take as a minimum an input
#' `x` which is either a numeric vector (if `by_df = FALSE`), or a data frame (if `by_df = TRUE`). In the former
#' case `f_ag` should return a single numeric value (i.e. the result of aggregating `x`), or in the latter case
#' a numeric vector (the result of aggregating the whole data frame in one go).
#'
#' `f_ag` can optionally have other parameters, e.g. weights, specified as a list in `f_ag_para`.
#'
#' Note that COINr has a number of aggregation functions built in,
#' all of which are of the form `a_*()`, e.g. [a_amean()], [a_gmean()] and friends. To see a list browse COINr functions alphabetically or
#' type `a_` in the R Studio console and press the tab key (after loading COINr), or see the [online documentation](https://bluefoxr.github.io/COINr/articles/aggregate.html#coinr-aggregation-functions).
#'
#' Optionally, a data availability threshold can be assigned below which the aggregated value will return
#' `NA` (see `dat_thresh` argument). If `by_df = TRUE`, this will however be ignored because aggregation is not
#' done on individual rows. Note that more complex constraints could be built into `f_ag` if needed.
#'
#' @param x Data frame to be aggregated
#' @param f_ag The name of an aggregation function, as a string.
#' @param f_ag_para Any additional parameters to pass to `f_ag`, as a named list.
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' of `x` has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # get some indicator data - take a few columns from built in data set
#' X <- ASEM_iData[12:15]
#'
#' # normalise to avoid zeros - min max between 1 and 100
#' X <- Normalise(X,
#'                global_specs = list(f_n = "n_minmax",
#'                                     f_n_para = list(l_u = c(1,100))))
#'
#' # aggregate using harmonic mean, with some weights
#' y <- Aggregate(X, f_ag = "a_hmean", f_ag_para = list(w = c(1, 1, 2, 1)))
#'
#' @return A numeric vector
#'
#' @export
Aggregate.data.frame <- function(x, f_ag = NULL, f_ag_para = NULL, dat_thresh = NULL,
                                  by_df = FALSE, ...){

  # CHECKS ------------------------------------------------------------------
  # x must be a df but check all numeric
  not_numeric <- !sapply(x, is.numeric)
  if(any(not_numeric)){
    stop("Non-numeric column(s) in x.")
  }

  if(!is.null(f_ag_para)){
    if(!is.list(f_ag_para)){
      stop("f_ag_para must be a list")
    }
  }

  # DEFAULTS ----------------------------------------------------------------

  # default mean of cols
  if(is.null(f_ag)){
    f_ag <- "a_amean"
    f_ag_para = list(w = rep(1, ncol(x)))
  }

  if(is.null(dat_thresh)){
    dat_thresh <- -1 # effectively no limit
  }

  # AGGREGATE ---------------------------------------------------------------

  lx <- ncol(x)

  # call aggregation function

  if(by_df){

    # DATA FRAME AGGRGATION
    if(is.null(f_ag_para)){
      y <- do.call(f_ag, list(x = x))
    } else {
      y <- do.call(f_ag, c(list(x = x), f_ag_para))
    }

  } else {

    # BY-ROW AGGREGATION
    if(is.null(f_ag_para)){
      y <- apply(x, 1, function(x){
        if(sum(!is.na(x))/lx < dat_thresh){
          NA
        } else {
          do.call(f_ag, list(x = x))
        }
      })
    } else {
      y <- apply(x, 1, function(x){
        if(sum(!is.na(x))/lx < dat_thresh){
          NA
        } else {
          do.call(f_ag, c(list(x = x), f_ag_para))
        }
      })
    }

  }


  if(!is.numeric(y)){
    if(all(is.na(y))){
      # if we get all NAs, this comes back as a logical vector, so convert
      y <- as.numeric(y)
    } else {
      stop("The output of f_ag has not successfully created a numeric vector.")
    }
  }
  if(length(y) != nrow(x)){
    stop("The ouput of f_ag is not the same length as nrow(x).")
  }

  y
}

#' Aggregate data
#'
#' Methods for aggregating numeric vectors, data frames, coins and purses. See individual method documentation
#' for more details:
#'
#' * [Aggregate.data.frame()]
#' * [Aggregate.coin()]
#' * [Aggregate.purse()]
#'
#' @param x Object to be aggregated
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' # see individual method documentation
#'
#' @return An object similar to the input
#'
#' @export
Aggregate <- function(x, ...){
  UseMethod("Aggregate")
}


#' Weighted arithmetic mean
#'
#' The vector of weights `w` is relative since the formula is:
#'
#' \deqn{ y = \frac{1}{\sum w_i} \sum w_i x_i }
#'
#' If `x` contains `NA`s, these `x` values and the corresponding `w` values are removed before applying the
#' formula above.
#'
#' @param x A numeric vector.
#' @param w A vector of numeric weights of the same length as `x`.
#'
#' @examples
#' x <- c(1:10)
#' w <- c(10:1)
#' a_amean(x,w)
#'
#' @return The weighted mean as a scalar value
#'
#' @export
a_amean <- function(x, w){

  # Checks
  stopifnot(is.numeric(x),
            is.numeric(w),
            length(w) == length(x))

  if(any(is.na(w))){
    stop("w cannot contain NAs")
  }

  # remove w entries corresponding to NAs in x
  w <- w[!is.na(x)]
  # also x
  x <- x[!is.na(x)]

  if(length(x)==0){
    return(NA)
  }

  # w to sum to 1
  w <- w/sum(w)

  sum(w*x)

}

#' Weighted geometric mean
#'
#' Weighted geometric mean of a vector. `NA` are skipped by default.
#'
#' This function replaces the now-defunct `geoMean()` from COINr < v1.0.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # weighted geometric mean
#' a_gmean(x,w)
#'
#' @return The geometric mean, as a numeric value.
#'
#' @export

a_gmean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified for geometric mean, using equal weights.")
  }

  if(any(!is.na(x))){

    if(any((x <= 0), na.rm = TRUE)){
      stop("Negative or zero values found when applying geometric mean. This doesn't work because geometric
         mean uses log. Normalise to remove negative/zero values first or use another aggregation method.")}

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA
    # calculate geom mean
    gm <- exp( sum(w * log(x), na.rm = TRUE)/sum(w, na.rm = TRUE) )

  } else {
    gm <- NA
  }

  gm

}


#' Weighted harmonic mean
#'
#' Weighted harmonic mean of a vector. `NA` are skipped by default.
#'
#' This function replaces the now-defunct `harMean()` from COINr < v1.0.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # weighted harmonic mean
#' a_hmean(x,w)
#'
#' @return Weighted harmonic mean, as a numeric value.
#'
#' @export
a_hmean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified harmonic mean, using equal weights.")
  }

  if(any(!is.na(x))){

    if(any(x == 0, na.rm = TRUE)){
      stop("Zero values found when applying harmonic mean. This doesn't work because harmonic
         mean uses 1/x. Normalise to remove zero values first or use another aggregation method.")}

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA

    hm <- sum(w, na.rm = TRUE)/sum(w/x, na.rm = TRUE)

  } else {
    hm <- NA
  }

  hm

}


#' Weighted generalised mean
#'
#' Weighted generalised mean of a vector. `NA` are skipped by default.
#'
#' The generalised mean is as follows:
#'
#' \deqn{ y = \left( \frac{1}{\sum w_i} \sum w_i x_i^p  \right)^{1/p} }
#'
#' where `p` is a coefficient specified in the function argument here. Note that:
#'
#' - For negative `p`, all `x` values must be positive
#' - Setting `p = 0` will result in an error due to the negative exponent. This case
#' is equivalent to the geometric mean in the limit, so use [a_gmean()] instead.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#' @param p Coefficient - see details.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # cubic mean
#' a_genmean(x,w, p = 2)
#'
#' @return Weighted harmonic mean, as a numeric value.
#'
#' @export
a_genmean <- function(x, w = NULL, p){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified, using equal weights.")
  }

  if(p==0){
    stop("Setting p = 0 results in an infinite exponent. In the limit, this case is equal to the geometric mean: use a_gmean() instead.")
  }

  if(any(!is.na(x))){

    if(any(x <= 0, na.rm = TRUE) && p < 0){
      stop("Zero or negative values found when applying generalised mean with negative p: cannot be calculated.")
    }

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA

    gm <- (sum(w*x^p)/sum(w))^(1/p)

  } else {
    gm <- NA
  }

  gm

}



#' Outranking matrix
#'
#' Constructs an outranking matrix based on a data frame of indicator data and corresponding weights.
#'
#' @param X A data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A vector of weights, which should have length equal to `ncol(X)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # get a sample of a few indicators
#' ind_data <- COINr::ASEM_iData[12:16]
#' # calculate outranking matrix
#' outlist <- outrankMatrix(ind_data)
#' # see fraction of dominant pairs (robustness)
#' outlist$fracDominant
#'
#' @return A list with:
#' * `.$OutRankMatrix` the outranking matrix with `nrow(X)` rows and columns (matrix class).
#' * `.$nDominant` the number of dominance/robust pairs
#' * `.$fracDominant` the percentage of dominance/robust pairs
#'
#' @export

outrankMatrix <- function(X, w = NULL){

  stopifnot(is.data.frame(X) | is.matrix(X))

  if (!all(apply(X, 2, is.numeric))){
    stop("Non-numeric columns in input data frame or matrix not allowed.")
  }

  nInd <- ncol(X)
  nUnit <- nrow(X)

  if(is.null(w)){
    # default equal weights
    w <- rep(1,nInd)
    message("No weights specified for outranking matrix, using equal weights.")
  }

  # make w sum to 1
  w = w/sum(w, na.rm = TRUE)

  # prep outranking matrix
  orm <- matrix(NA, nrow = nUnit, ncol = nUnit)

  for (ii in 1:nUnit){

    # get iith row, i.e. the indicator values of unit ii
    rowii <- X[ii,]

    for (jj in 1:nUnit){

      if (ii==jj){
        # diag vals are zero
        orm[ii, jj] <- 0
      } else if (ii>jj){
        # to save time, only calc upper triangle of matrix. If lower triangle, do 1-upper
        orm[ii, jj] <- 1 - orm[jj, ii]
      } else {

        # get jjth row, i.e. the indicator values of unit jj
        rowjj <- X[jj,]

        # get score. Sum of weights where ii scores higher than jj, and half sum of weights where they are equal
        orm[ii, jj] <- sum(
          sum(w[rowii > rowjj], na.rm = TRUE),
          sum(w[rowii == rowjj], na.rm = TRUE)/2,
          na.rm = TRUE)

      }
    }
  }

  # find number of dominance pairs
  ndom <- sum(orm==1, na.rm = TRUE)
  npairs <- (nUnit^2 - nUnit)/2
  prcdom <- ndom/npairs

  list(
    OutRankMatrix = orm,
    nDominant = ndom,
    fracDominant = prcdom)

}

#' Copeland scores
#'
#' Aggregates a data frame of indicator values into a single column using the Copeland method.
#' This function calls `outrankMatrix()`.
#'
#' The outranking matrix is transformed as follows:
#'
#' * values > 0.5 are replaced by 1
#' * values < 0.5 are replaced by -1
#' * values == 0.5 are replaced by 0
#' * the diagonal of the matrix is all zeros
#'
#' The Copeland scores are calculated as the row sums of this transformed matrix.
#'
#' This function replaces the now-defunct `copeland()` from COINr < v1.0.
#'
#' @param X A numeric data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A numeric vector of weights, which should have length equal to `ncol(X)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # some example data
#' ind_data <- COINr::ASEM_iData[12:16]
#'
#' # aggregate with vector of weights
#' outlist <- outrankMatrix(ind_data)
#'
#' @return Numeric vector of Copeland scores.
#'
#' @export
a_copeland <- function(X, w = NULL){

  # get outranking matrix
  orm <- outrankMatrix(X, w)$OutRankMatrix

  orm[orm > 0.5] <- 1
  orm[orm < 0.5] <- -1
  orm[orm == 0.5] <- 0
  diag(orm) <- 0

  # get scores by summing across rows
  rowSums(orm, na.rm = TRUE)

  # outlist <- list(Scores = scores, OutRankMat = orm)
  # outlist
}
