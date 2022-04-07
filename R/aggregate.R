#' Aggregate indicators
#'
#' @param x A purse object
#' @param dset The data set to treat in each coin
#' @param f_ag The name of an aggregation function, specified as a string
#' @param w An optional data frame of weights. If `f_ag` does not require or accept weights, set to `"none"`.
#' @param f_ag_para Optional parameters to pass to `f_ag`, other than `x` and `w`
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' within an aggregation group has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Aggregated"`.
#'
#' @return An updated purse with new treated data sets added at `.$Data$Aggregated` in each coin.
#' @export
#'
#' @examples
#' #
Aggregate.purse <- function(x, dset, f_ag = NULL, w = NULL, f_ag_para = NULL, dat_thresh = NULL,
                             write_to = NULL){

  # input check
  check_purse(x)

  # apply unit screening to each coin
  x$coin <- lapply(x$coin, function(coin){
    Aggregate.coin(coin, dset, f_ag = f_ag, w = w, f_ag_para = f_ag_para, dat_thresh = dat_thresh,
                    out2 = "coin", write_to = write_to)
  })
  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}


#' Aggregate indicators
#'
#' Aggregates a named data set specified by `dset` using aggregation function `f_ag`, weights `w`, and optional
#' function parameters `f_ag_para`.
#'
#' @param x The coin object
#' @param dset The data set to aggregate
#' @param f_ag The name of an aggregation function, a string
#' @param w An optional data frame of weights. If `f_ag` does not require accept weights, set to `"none"`.
#' @param f_ag_para Optional parameters to pass to `f_ag`, other than `x` and `w`
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' within an aggregation group has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details.
#' @param out2 Either `"coin"` (default) to return updated coin or `"df"` to output the aggregated data set.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Aggregated"`.
#'
#' @examples
#' #
#'
#' @return An updated coin with aggregated data set added
#'
#' @export
Aggregate.coin <- function(x, dset, f_ag = NULL, w = NULL, f_ag_para = NULL, dat_thresh = NULL,
                            by_df = FALSE, out2 = "coin", write_to = NULL){

  # Write to Log ------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")

  # CHECK AND SET f_ag ------------------------------------------------------

  nlev <- max(coin$Meta$Ind$Level, na.rm = TRUE)

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
      stop("f_ag must have either length 1 (same function for all levels) or length equal to number of levels - in your case: ", nlev)
    }
    f_ags <- f_ag
  }

  # CHECK AND SET w ---------------------------------------------------------

  # if weights is supplied we have to see what kind of thing it is
  # NULL indicates that we should use metadata weights
  if(!is.null(w)){

    if(is.data.frame(w)){

      stopifnot(exists("iCode", w),
                exists("Weight", w))
      w1 <- w

    } else if(is.character(w)){

      if(length(w) != 1){
        stop("w must be either a string indicating a name of a weight set, or a data frame of weights, or 'none', or NULL (to use weights from metadata).")
      }

      if(w != "none"){

        # we look for a named weight set
        w1 <- coin$Meta$Weights[[w]]
        if(is.null(w1)){
          stop("Weight set with name '", w, "' not found in .$Meta$Weights.")
        }
        stopifnot(is.data.frame(w1),
                  exists("iCode", w1),
                  exists("Weight", w1))

      } else {
        # convert w1 to NULL
        w1 <- NULL
      }
    } else {
      stop("w must be either a string indicating a name of a weight set, or a data frame of weights, or 'none', or NULL (to use weights from metadata).")
    }

  } else{
    # if w was NULL, get from metadata
    w1 <- coin$Meta$Ind[c("iCode", "Weight")]
  }

  # from this point, w1 is either a data frame of weights, or NULL (don't pass weights to f_ag)

  # CHECK AND SET f_ag_para -------------------------------------------------

  # if f_ag_para is NULL, repeat for all levs
  if(!is.null(f_ag_para)){
    if(!is.list(f_ag_para)){
      stop("f_ag_para must be specified as a list or list of lists")
    }
    stopifnot(length(f_ag_para) > 0)

    # if same for all levels, repeat
    if(length(f_ag_para) == 1){
      f_ag_paras <- rep(f_ag_para, nlev - 1)
    } else {
      if(length(f_ag_para) != (nlev - 1)){
        stop("f_ag_para must have either length 1 (same parameters for all levels) or length equal to number of levels - in your case: ", nlev)
      }
      f_ag_paras <- f_ag_para
    }
  } else {
    f_ag_paras <- rep(list(NULL), 4)
  }

  # Other Prep --------------------------------------------------------------------

  if(is.null(dat_thresh)){
    dat_threshs <- rep(list(NULL), 4)
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

    if(is.null(w1)){
      aggs <- tapply(imeta_l$iCode, imeta_l$Parent, function(codes){
        # call func
        do.call("Aggregate",
                list(x = indat_ag[codes],
                     f_ag = f_ags[lev-1],
                     f_ag_para = f_ag_paras[[lev-1]],
                     dat_thresh = dat_threshs[lev-1],
                     by_df = by_df))
      })
    } else {
      aggs <- tapply(imeta_l$iCode, imeta_l$Parent, function(codes){
        # get weights
        wts <- w1$Weight[match(codes, w1$iCode)]
        # call func
        do.call("Aggregate",
                list(x = indat_ag[codes],
                     f_ag = f_ags[lev-1],
                     f_ag_para = c(list(w = wts), f_ag_paras[[lev-1]]),
                     dat_thresh = dat_threshs[[lev-1]],
                     by_df = by_df))
      })
    }

    # aggs comes out as a list of vectors, have to make to df
    as.data.frame(do.call(cbind, aggs))
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
#' Aggregates a data frame into a single column using a specified function.
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
#' Optionally, a data availability threshold can be assigned below which the aggregated value will return
#' `NA` (see `dat_thresh` argument). If `by_df = TRUE`, this will however be ignored because aggregation is not
#' done on individual rows. Note that more complex constraints could be built into `f_ag` if needed.
#'
#' @param x Object to be aggregated
#' @param f_ag The name of an aggregation function, as a string.
#' @param f_ag_para Any additional parameters to pass to `f_ag`, as a named list.
#' @param dat_thresh An optional data availability threshold, specified as a number between 0 and 1. If a row
#' of `x` has data availability lower than this threshold, the aggregated value for that row will be
#' `NA`. Data availability, for a row `x_row` is defined as `sum(!is.na(x_row))/length(x_row)`, i.e. the
#' fraction of non-`NA` values.
#' @param by_df Controls whether to send a numeric vector to `f_ag` (if `FALSE`, default) or a data frame (if `TRUE`) - see
#' details.
#'
#' @examples
#' #
#'
#' @return A numeric vector
#'
#' @export
Aggregate.data.frame <- function(x, f_ag = NULL, f_ag_para = NULL, dat_thresh = NULL,
                                  by_df = FALSE){

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
    stop("The output of f_ag has not successfully created a numeric vector.")
  }
  if(length(y) != nrow(x)){
    stop("The ouput of f_ag is not the same length as nrow(x).")
  }

  y
}

#' Aggregate data
#'
#' Methods for aggregating numeric vectors, data frames, coins and purses.
#'
#' @param x Object to be aggregated
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' #
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
#' $ y = 1(\sum w) \sum wx $
#'
#' If `x` contains `NA`s, these `x` values and the corresponding `w` values are removed before applying the
#' formula above.
#'
#' @param x A numeric vector
#' @param w A vector of numeric weights of the same length as `x`.
#'
#' @examples
#' x <- c(1:10)
#' w <- c(10:1)
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
#' geoMean(x,w)
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
#' ind_data <- COINr::ASEMIndData[12:16]
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
    w <- rep(1,nUnit)
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
#' Aggregates a data frame of indicator values into a single column using the Copeland method. This function is used inside
#' [aggregate()], and calls `outrankMatrix()`.
#'
#' @param X A numeric data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A numeric vector of weights, which should have length equal to `ncol(X)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # get a sample of a few indicators
#' ind_data <- ASEMIndData[12:16]
#' # calculate outranking matrix
#' cop_results <- copeland(ind_data)
#' # check output
#' stopifnot(length(cop_results$Scores) == nrow(ind_data))
#'
#' @return Numeric vector of Copeland scores.
#'
#' @export
a_copeland <- function(X, w = NULL){

  # get outranking matrix
  orm <- outrankMatrix(X, w)$OutRankMatrix

  # get scores by summing across rows
  rowSums(orm, na.rm = TRUE)

  # outlist <- list(Scores = scores, OutRankMat = orm)
  # outlist
}
