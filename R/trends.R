
#' Get time trends
#'
#' Get time trends from a purse object. This function extracts a panel data set from a purse, and calculates trends
#' for each indicator/unit pair using a specified function `f_trend`. For example, if `f_trend = "CAGR"`, this extracts
#' the time series for each indicator/unit pair and passes it to [CAGR()].
#'
#' This function requires a purse object as an input. The data set is selected using [get_data()], such that a subset
#' of the data set can be analysed using the `uCodes`, `iCodes` and `Time` arguments. The latter is useful especially
#' if only a subset of the time series should be analysed.
#'
#' The function `f_trend` is a function that, given a time series, returns a trend metric. This must follow a
#' specific format. It must of course be available to call, and *must* have arguments `y` and `x`, which are
#' respectively a vector of values and a vector indexing the values in time. See [prc_change()] and [CAGR()]
#' for examples. The function *must* return a single value (not a vector with multiple entries, or a list).
#' The function can return either numeric or character values.
#'
#' @param purse A purse object
#' @param dset Name of the data set to extract, passed to [get_data.purse()]
#' @param uCodes Optional subset of unit codes to extract, passed to [get_data.purse()]
#' @param iCodes Optional subset of indicator/aggregate codes to extract, passed to [get_data.purse()]
#' @param Time Optional vector of time points to extract, passed to [get_data.purse()]
#' @param f_trend Function that returns a metric describing the trend of the time series. See details.
#' @param use_latest A positive integer which specifies to use only the latest "n" data points. If this is specified, it
#' overrides `Time`. If e.g. `use_latest = 5`, will use the latest five observations, working backwards from the latest
#' non-`NA` point.
#' @param interp_at Option to linearly interpolate missing data points in each time series. Must be specified as a vector
#' of time values where to apply interpolation. If `interp_at = "all"`, will attempt to interpolate at every
#' time point. Uses linear interpolation - note that any `NA`s outside of the range of observed values will not
#' be estimated, i.e. this does not extrapolate beyond the range of data. See [approx_df()].
#' @param adjust_directions Logical: if `TRUE`, trend metrics are adjusted according to indicator/aggregate
#' directions input in `iMeta` (i.e. if the corresponding direction is -1, the metric will be multiplied by -1).
#'
#' @importFrom stats lm
#'
#' @return A data frame in long format, with trend metrics for each indicator/unit pair, plus
#' data availability statistics.
#' @export
#'
#' @examples
#' #
#'
get_trends <- function(purse, dset, uCodes = NULL, iCodes = NULL,
                       Time = NULL, use_latest = NULL, f_trend = "CAGR", interp_at = NULL,
                       adjust_directions = FALSE){


  # Input checks ------------------------------------------------------------

  check_purse_input(purse)
  if(!is.null(use_latest)){
    stopifnot(is.numeric(use_latest),
              length(use_latest) == 1)
  }


  # Get data and check ------------------------------------------------------

  # retrieve data
  # NOTE if we want to interpolate need to pull all data (all Time)
  iData <- get_data(purse, dset = dset, uCodes = uCodes, iCodes = iCodes)
  # get iMeta (for directions)
  iMeta <- purse$coin[[1]]$Meta$Ind

  # checks
  stopifnot("uCode" %in% names(iData),
            "Time" %in% names(iData))

  if(!is.null(interp_at)){

    # interp at all points
    if(length(interp_at) == 1){
      if(interp_at == "all"){
        interp_at <- unique(iData$Time)
      }
    }

    stopifnot(is.numeric(interp_at),
              all(!is.na(interp_at)))
    if( any(interp_at < min(iData$Time)) || any(interp_at > max(iData$Time))){
      stop("One or more entries in interp_at are outside the time range of the data.", call. = FALSE)
    }
  }

  if(!is.null(Time)){
    if(any(Time %nin% unique(iData$Time))){
      stop("One or more entries in Time are not found in the selected data set.")
    }
  }


  # Prep for data avail records ---------------------------------------------

  # this is done kind of separately because was added afterwards, would otherwise have to re-write
  # Prep df for all indicator/unit pairs, with data avail cols. These will be populated within the following loops
  dat_avail <- data.frame(
    expand.grid(unique(iData$uCode), names(iData)[names(iData) %nin% c("uCode", "Time")], stringsAsFactors = FALSE),
    Avail = 0,
    t_first = NA,
    t_latest = NA,
    Avail_use_latest = NA,
    t_first_use_latest = NA
  )
  names(dat_avail)[1:2] <- c("uCode", "iCode")

  # df for time series (currently only for use_latest)
  if(!is.null(use_latest)){
    # df for y values
    df_y <- dat_avail[c("uCode", "iCode")]
    df_y <- cbind(df_y, as.data.frame(matrix(nrow = nrow(df_y), ncol = use_latest)))
    names(df_y)[3:ncol(df_y)] <- paste0("y", 1:use_latest)
    # df for x values
    df_x <- df_y
    names(df_x)[3:ncol(df_x)] <- paste0("x", 1:use_latest)
  }

  # Get trends --------------------------------------------------------------

  # first split data by uCode
  l_data <- split(iData, f = iData$uCode)

  # this function gets trends for each uCode
  l_trends <- lapply(l_data, function(dfi){

    # dfi is a data frame with one single unit code
    uCode <- unique(dfi$uCode)
    stopifnot(length(uCode) == 1)

    # I have to make sure that the Time col has the same entries as interp_at
    # otherwise this causes trouble later on
    if(!is.null(interp_at)){
      dfi <- merge(data.frame(Time = interp_at), dfi, by = "Time", all = TRUE)
      dfi$uCode <- uCode
    }

    # now sort it (low to high)
    dfi <- dfi[order(dfi$Time), ]

    # get time vector
    tt <- dfi$Time

    # get indicator cols
    icols <- dfi[names(dfi) %nin% c("Time", "uCode")]

    # get time-filtered copy for later on
    if(!is.null(Time)){
      dfi_t <- dfi[dfi$Time %in% Time, ]
      tt_t <- dfi_t$Time
      dfi_t <- dfi_t[names(dfi_t) %nin% c("Time", "uCode")]

    } else {
      dfi_t <- icols
      tt_t <- tt
    }

    # interpolate if requested
    if(!is.null(interp_at)){

      l_out <- approx_df(Y = icols, tt = tt, tt_est = interp_at)
      Y_use <- l_out$Y
      tt_use <- l_out$tt

    } else {

      Y_use <- icols
      tt_use <- tt

    }

    # subset to Time
    if(!is.null(Time) && is.null(use_latest)){
      # use_latest overrides Time, if it is specified, so this only happens if use_latest is NULL
      # Time may request some points not present so we take the intersect
      tt_use <- intersect(tt_use, Time)
      Y_use <- Y_use[match(tt_use, Time), ]
    }

    # things are getting fiddly now so I go into a for loop
    trends <- as.numeric(rep(NA, ncol(Y_use)))

    for(ii in 1:ncol(Y_use)){

      # indicator code of this col
      icode <- names(Y_use)[ii]
      # direction of this indicator
      if(adjust_directions){
        direction <- iMeta$Direction[iMeta$iCode == icode]
      } else {
        direction <- 1
      }

      # yraw is the col of indicator data (with no adjustments, but possibly filtered by Time)
      yraw <- dfi_t[[ii]]

      # y is the column of indicator data (possibly interpolated)
      y <- Y_use[[ii]]

      # row index in data availability table
      i_dat_avail <- which((dat_avail$uCode == uCode) & (dat_avail$iCode == icode))
      # overall data availability of y
      dat_avail$Avail[i_dat_avail] <<- mean(!is.na(yraw))

      if(sum(!is.na(yraw)) > 1){
        dat_avail$t_first[i_dat_avail] <<- tt_t[min(which(!is.na(yraw)))]
        dat_avail$t_latest[i_dat_avail] <<- tt_t[max(which(!is.na(yraw)))]
      }

      # if y has less than 2 non-NA points, we simply give NA and go to the next col
      if(sum(!is.na(y)) < 2){
        trends[ii] <- NA
        next
      }

      if(!is.null(use_latest)){

        # this is the index of the latest data point
        i_latest <- max(which(!is.na(y)))
        # index of first point in series (to use)
        i_first <- i_latest - use_latest + 1

        if(i_first > 1){

          # take the latest points
          y <- y[i_first : i_latest]
          tt_ii <- tt_use[i_first : i_latest]
          yraw_ii <- yraw[i_first : i_latest]

          # record these in data frames TEST
          df_x[i_dat_avail, 3:ncol(df_x)] <<- tt_ii
          df_y[i_dat_avail, 3:ncol(df_y)] <<- yraw_ii

          # overall data availability of y (we use yraw, not interpolated)
          dat_avail$Avail_use_latest[i_dat_avail] <<- mean(!is.na(yraw_ii))

          if(sum(!is.na(y)) > 1){
            dat_avail$t_first_use_latest[i_dat_avail] <<- tt_ii[min(which(!is.na(y)))]
          }

          # check again in case we have all NAs here
          if(sum(!is.na(y)) < 2){
            trends[ii] <- NA
            next
          }

          # send to function
          trend_metric <- do.call(f_trend, list(y = y, x = tt_ii))

          # check
          if(length(trend_metric) == 1){
            if(is.numeric(trend_metric) || is.character(trend_metric)){
              trends[ii] <- trend_metric*direction
            } else {
              stop("The trend metric returned by 'f_trend' is not either numeric or character", call. = FALSE)
            }
          } else {
            stop("The trend metric returned by 'f_trend' is not a vector of length 1", call. = FALSE)
          }


        } else {
          # here we go back father than we have data points, so NA
          trends[ii] <- NA
          next
        }

      } else {

        # no further subsetting of the data by time
        # send to function
        trend_metric <- do.call(f_trend, list(y = y, x = tt_use))

        # check
        if(length(trend_metric) == 1){
          if(is.numeric(trend_metric) || is.character(trend_metric)){
            trends[ii] <- trend_metric*direction
          } else {
            stop("The trend metric returned by 'f_trend' is not either numeric or character", call. = FALSE)
          }
        } else {
          stop("The trend metric returned by 'f_trend' is not a vector of length 1", call. = FALSE)
        }

      } # end

    } # end for loop

    # the output of the lapply call
    names(trends) <- names(Y_use)
    trends

  })

  # reshape this into a data frame
  df_trends <- as.data.frame(l_trends)
  df_trends <- cbind(iCode = row.names(df_trends), df_trends)
  row.names(df_trends) <- NULL

  # convert to long format
  df_long <- lengthen(df_trends, cols = names(df_trends)[names(df_trends) != "iCode"])
  names(df_long)[names(df_long) == "Value"] <- f_trend
  names(df_long)[names(df_long) == "name"] <- "uCode"

  # merge with dat_avail
  df_long <- merge(df_long, dat_avail, by = c("uCode", "iCode"), all = TRUE)

  # order rows of df_long
  df_long <- df_long[order(df_long$iCode, df_long$uCode), ]

  # output
  list(Trends = df_long,
       x = df_x[order(df_x$iCode, df_x$uCode), ],
       y = df_y[order(df_y$iCode, df_y$uCode), ])

}


#' Interpolate time-indexed data frame
#'
#' Given a numeric data frame `Y` with rows indexed by a time vector `tt`, interpolates at time values
#' specified by the vector `tt_est`. If `tt_est` is not in `tt`, will create new rows in the data frame
#' corresponding to these interpolated points.
#'
#' This is a wrapper for [stats::approx()], with some differences. In the first place, [stats::approx()] is
#' applied to each column of `Y`, using `tt` each time as the corresponding time vector indexing `Y`. Interpolated
#' values are generated at points specified in `tt_est` but these are appended to the existing data (whereas
#' [stats::approx()] will only return the interpolated points and nothing else). Further arguments to
#' [stats::approx()] can be passed using the `...` argument.
#'
#' @param Y A data frame with all numeric columns
#' @param tt A time vector with length equal to `nrow(Y)`, indexing the rows in `Y`.
#' @param tt_est A time vector of points to interpolate in `Y`. If `NULL`, will attempt to interpolate all
#' points in `Y` (you may need to adjust the `rule` argument of [stats::approx()] here). Note that points not
#' specified in `tt_est` will not be interpolated. `tt_est` does not need to be a subset of `tt`.
#' @param ... Further arguments to pass to [stats::approx()] other than `x`, `y` and `xout`.
#'
#' @importFrom stats approx
#'
#' @return A list with:
#' * `.$tt` the vector of time points, including time values of interpolated points
#' * `.$Y`  the corresponding interpolated data frame
#'
#' Both outputs are sorted by `tt`.
#' @export
#'
#' @examples
#' # a time vector
#' tt <- 2011:2020
#'
#' # two random vectors with some missing values
#' y1 <- runif(10)
#' y2 <- runif(10)
#' y1[2] <- y1[5] <- NA
#' y2[3] <- y2[5] <- NA
#' # make into df
#' Y <- data.frame(y1, y2)
#'
#' # interpolate for time = 2012
#' Y_int <- approx_df(Y, tt, 2012)
#' Y_int$Y
#'
#' # notice Y_int$y2 is unchanged since at 2012 it did not have NA value
#' stopifnot(identical(Y_int$Y$y2, y2))
#'
#' # interpolate at value not in tt
#' approx_df(Y, tt, 2015.5)
#'
approx_df <- function(Y, tt, tt_est = NULL, ...){

  # some basic checks
  stopifnot(is.data.frame(Y),
            all(sapply(Y, is.numeric)),
            nrow(Y) == length(tt))

  # defaults
  if(is.null(tt_est)){
    tt_est <- tt
  }

  # get tt that are NOT to be sent to approx()
  tt_not_est <- setdiff(tt, tt_est)

  # get tt_out
  tt_out <- c(tt_est, tt_not_est)

  Y_out <- lapply(Y, function(y){

    if(sum(!is.na(y)) > 1){
      l_out <- stats::approx(x = tt, y = y, xout = tt_est, ...)
      y_out <- c(l_out$y, y[match(tt_not_est, tt)])
    } else {
      # if vector is all NAs, just return a vector of NAs
      as.numeric(rep(NA, length(tt_out)))
    }

  })

  # reassamble and order
  Y_out <- as.data.frame(Y_out)
  Y_out <- Y_out[order(tt_out), ]
  row.names(Y_out) <- NULL
  tt_out <- tt_out[order(tt_out)]

  list(tt = tt_out,
       Y = Y_out)

}

#' Compound annual growth rate
#'
#' Given a variable `y` indexed by a time vector `x`, calculates the compound annual growth rate. Note that CAGR assumes
#' that the `x` refer to years. Also it is only calculated using the first and latest observed values.
#'
#' @param y A numeric vector
#' @param x A numeric vector of the same length as `y`, indexing `y` in time. No `NA` values are allowed
#' in `x`. This vector is assumed to be years, otherwise the result must be interpreted differently.
#'
#' @return A scalar value (CAGR)
#' @export
#'
#' @examples
#' # random points over 10 years
#' x <- 2011:2020
#' y <- runif(10)
#'
#' CAGR(y, x)
#'
CAGR <- function(y, x){

  # checks
  stopifnot(is.numeric(y),
            is.numeric(x),
            length(x) == length(y))

  if(any(is.na(x))){
    stop("x contains NAs - this is not allowed (each y value should be indexed by a time point in x)", call. = FALSE)
  }

  if(sum(!is.na(y)) < 2){
    return(NA)
  }

  # deal with NAs
  xy <- data.frame(x, y)
  xy <- na.omit(xy)

  # calc CAGR

  # order first
  xy <- xy[order(xy$x), ]
  # index of latest obs
  ilat <- nrow(xy)

  if(xy$y[ilat] == xy$y[1]){
    # this covers when start and end value are both zero, which would otherwise return NaN
    out1 <- 0
  } else {
    out1 <- (xy$y[ilat] / xy$y[1])^(1 / (xy$x[ilat] - xy$x[1])) - 1
  }

  out1

}


#' Percentage change of time series
#'
#' Calculates the percentage change in a time series from the initial value. The time series is defined by
#' `y` the response variable, indexed by `x`, the time variable. The `per` argument can optionally be used
#' to scale the result according to a period of time. E.g. if the units of `x` are years, setting `x = 10`
#' will measure the percentage change per decade.
#'
#' This function operates in two ways, depending on the number of data points. If `x` and `y` have two non-`NA`
#' observations, percentage change is calculated using the first and last values. If three or more points are
#' available, a linear regression is used to estimate the average percentage change. If fewer than two points
#' are available, the percentage change cannot be estimated and `NA` is returned.
#'
#' If all `y` values are equal, it will return a change of zero.
#'
#' @param y A numeric vector
#' @param x A numeric vector of the same length as `y`, indexing `y` in time. No `NA` values are allowed
#' in `x`.
#' @param per Numeric value to scale the change according to a period of time. See description.
#'
#' @return Percentage change as a scalar value.
#'
#' @export
#'
#' @examples
#' # a time vector
#' x <- 2011:2020
#'
#' # some random points
#' y <- runif(10)
#'
#' # find percentage change per decade
#' prc_change(y, x, 10)
prc_change <- function(y, x, per = 1){

  # checks
  stopifnot(is.numeric(y),
            is.numeric(x),
            length(x) == length(y))

  if(any(is.na(x))){
    stop("x contains NAs - this is not allowed (each y value should be indexed by a time point in x)", call. = FALSE)
  }

  if(sum(!is.na(y)) < 2){
    return(NA)
  }

  # deal with NAs
  xy <- data.frame(x, y)
  xy <- na.omit(xy)


  # Calc prc change ---------------------------------------------------------

  # first check if all values in y are the same. If this is sent to lm() we get
  # NaN but it is more sensible to return 0.
  if(length(unique(xy$y)) == 1){
    return(0)
  }

  # order first
  xy <- xy[order(xy$x), ]

  if(nrow(xy) < 3){

    # index of latest obs
    ilat <- nrow(xy)

    # prc change based on first and last vals
    prc <- ((xy$y[ilat] - xy$y[1])*100/xy$y[1])*(per/(xy$x[ilat] - xy$x[1]))

  } else {

    # if we have 3 or more points we perform a regression
    lm1 <- stats::lm(xy$y ~ xy$x)

    # we get the first and last values
    # prc change based on first and last vals
    #((xy$y[ilat] - xy$y[1])*100/xy$y[1])*(10/(xy$x[ilat] - xy$x[1]))

    # get coeffs
    icpt <- as.numeric(lm1$coefficients[1])
    slop <- as.numeric(lm1$coefficients[2])

    # calculate prc change per decade
    prc <- (slop * per)/(icpt + slop * xy$x[1])*100

  }

  prc

}
