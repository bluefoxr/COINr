#' Statistics of indicators
#'
#' Given a coin and a specified data set (`dset`), returns a table of statistics with entries for each column.
#' The statistics (columns in the output table) are as follows (entries correspond to each column):
#'
#' *`Min`: the minimum
#' *`Max`: the maximum
#' *`Mean`: the (arirthmetic) mean
#' *`Median`: the median
#' *`Std`: the standard deviation
#' *`Skew`: the skew
#' *`Kurt`: the kurtosis
#' *`N.Avail`: the number of non-`NA` values
#' *`N.NonZero`: the number of non-zero values
#' *`N.Unique`: the number of unique values
#' *`Frc.Avail`: the fraction of non-`NA` values
#' *`Frc.NonZero`: the fraction of non-zero values
#' *`Frc.Unique`: the fraction of unique values
#' *`Flag.Avail`: a data availability flag - columns with `Frc.Avail < t_avail` will be flagged as `"LOW"`, else `"ok"`.
#' *`Flag.NonZero`: a flag for columns with a high proportion of zeros. Any columns with `Frc.NonZero < t_zero` are
#' flagged as `"LOW"`, otherwise `"ok"`.
#' *`Flag.Unique`: a unique value flag - any columns with `Frc.Unique < t_unq` are flagged as `"LOW"`, otherwise `"ok"`.
#' *`Flag.SkewKurt`: a skew and kurtosis flag which is an indication of possible outliers. Any columns with
#' `abs(Skew) > t_skew` AND `Kurt > t_kurt` are flagged as `"OUT"`, otherwise `"ok"`.
#'
#' The aim of this table, among other things, is to check the basic statistics of each column/indicator, and identify
#' any possible issues for each indicator. For example, low data availability, having a high proportion of zeros and/or
#' a low proportion of unique values. Further, the combination of skew and kurtosis (i.e. the `Flag.SkewKurt` column)
#' is a simple test for possible outliers, which may require treatment using [Treat()].
#'
#' The table can be returned either to the coin or as a standalone data frame - see `out2`.
#'
#' See also `vignette("analysis")`.
#'
#' @param t_skew Absolute skewness threshold. See details.
#' @param t_kurt Kurtosis threshold. See details.
#' @param t_avail Data availability threshold. See details.
#' @param x A coin
#' @param dset A data set present in `.$Data`
#' @param nsignif Number of significant figures to round the output table to.
#' @param out2 Either `"df"` (default) to output a data frame of indicator statistics, or "`coin`" to output an
#' updated coin with the data frame attached under `.$Analysis`.
#' @param ... arguments passed to or from other methods.
#' @param t_zero A threshold between 0 and 1 for flagging indicators with high proportion of zeroes. See details.
#' @param t_unq A threshold between 0 and 1 for flagging indicators with low proportion of unique values. See details.plot
#'
#' @examples
#' # build example coin
#' coin <-  build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get table of indicator statistics for raw data set
#' get_stats(coin, dset = "Raw", out2 = "df")
#'
#' @return Either a data frame or updated coin - see `out2`.
#'
#' @export
get_stats.coin <- function(x, dset, t_skew = 2, t_kurt = 3.5, t_avail = 0.65, t_zero = 0.5,
                                 t_unq = 0.5, nsignif = 3, out2 = "df", ...){

  stopifnot(out2 %in% c("df", "coin"))

  # get data
  iData <- get_data(x, dset = dset, ...)

  # get iData_ (only numeric indicator cols)
  iData_ <- extract_iData(x, iData, GET = "iData_")

  # get stats table
  stat_tab <- get_stats(iData_, t_skew = t_skew, t_kurt = t_kurt, t_avail = t_avail, t_zero = t_zero,
                        t_unq = t_unq, nsignif = nsignif)

  # write to coin or output as df
  if(out2 == "df"){
    stat_tab
  } else {
    x$Analysis[[dset]][["Stats"]] <- stat_tab
    x
  }

}



#' Statistics of columns
#'
#' Takes a data frame and returns a table of statistics with entries for each column. The statistics (columns in the
#' output table) are as follows (entries correspond to each column):
#'
#' *`Min`: the minimum
#' *`Max`: the maximum
#' *`Mean`: the (arithmetic) mean
#' *`Median`: the median
#' *`Std`: the standard deviation
#' *`Skew`: the skew
#' *`Kurt`: the kurtosis
#' *`N.Avail`: the number of non-`NA` values
#' *`N.NonZero`: the number of non-zero values
#' *`N.Unique`: the number of unique values
#' *`Frc.Avail`: the fraction of non-`NA` values
#' *`Frc.NonZero`: the fraction of non-zero values
#' *`Frc.Unique`: the fraction of unique values
#' *`Flag.Avail`: a data availability flag - columns with `Frc.Avail < t_avail` will be flagged as `"LOW"`, else `"ok"`.
#' *`Flag.NonZero`: a flag for columns with a high proportion of zeros. Any columns with `Frc.NonZero < t_zero` are
#' flagged as `"LOW"`, otherwise `"ok"`.
#' *`Flag.Unique`: a unique value flag - any columns with `Frc.Unique < t_unq` are flagged as `"LOW"`, otherwise `"ok"`.
#' *`Flag.SkewKurt`: a skew and kurtosis flag which is an indication of possible outliers. Any columns with
#' `abs(Skew) > t_skew` AND `Kurt > t_kurt` are flagged as `"OUT"`, otherwise `"ok"`.
#'
#' The aim of this table, among other things, is to check the basic statistics of each column/indicator, and identify
#' any possible issues for each indicator. For example, low data availability, having a high proportion of zeros and/or
#' a low proportion of unique values. Further, the combination of skew and kurtosis (i.e. the `Flag.SkewKurt` column)
#' is a simple test for possible outliers, which may require treatment using [Treat()].
#'
#' See also `vignette("analysis")`.
#'
#' @param t_skew Absolute skewness threshold. See details.
#' @param t_kurt Kurtosis threshold. See details.
#' @param t_avail Data availability threshold. See details.
#' @param x A data frame with only numeric columns.
#' @param nsignif Number of significant figures to round the output table to.
#' @param ... arguments passed to or from other methods.
#' @param t_zero A threshold between 0 and 1 for flagging indicators with high proportion of zeroes. See details.
#' @param t_unq A threshold between 0 and 1 for flagging indicators with low proportion of unique values. See details.
#'
#' @importFrom stats median sd
#'
#' @examples
#' # stats of mtcars
#' get_stats(mtcars)
#'
#' @return A data frame of statistics for each column
#'
#' @export
get_stats.data.frame <- function(x, t_skew = 2, t_kurt = 3.5, t_avail = 0.65, t_zero = 0.5,
                                 t_unq = 0.5, nsignif = 3, ...){


  # CHECKS ------------------------------------------------------------------

  not_numeric <- !(sapply(x, is.numeric))
  if(any(not_numeric)){
    stop("Non-numeric cols detected in data frame. Input must be a data frame with only numeric columns.")
  }

  # STATS -------------------------------------------------------------------

  n <- nrow(x)

  # this function gets all stats for one column of data
  stats_i <- function(xi){

    n_avail <- sum(!is.na(xi))
    prc_avail <- n_avail/n
    sk <- skew(xi, na.rm = TRUE)
    kt <- kurt(xi, na.rm = TRUE)
    nzero <- sum(xi != 0, na.rm = TRUE)
    nunq <- length(unique(xi[!is.na(xi)]))

    data.frame(
      Min = min(xi, na.rm = TRUE),
      Max = max(xi, na.rm = TRUE),
      Mean = mean(xi, na.rm = TRUE),
      Median = stats::median(xi, na.rm = TRUE),
      Std = stats::sd(xi, na.rm = TRUE),
      Skew = sk,
      Kurt = kt,
      N.Avail = n_avail,
      N.NonZero = nzero,
      N.Unique = nunq,
      Frc.Avail = prc_avail,
      Frc.NonZero = nzero/n,
      Frc.Unique = nunq/n,
      Flag.Avail = ifelse(prc_avail >= t_avail, "ok", "LOW"),
      Flag.NonZero = ifelse(nzero/n >= t_zero, "ok", "LOW"),
      Flag.Unique = ifelse(nunq/n >= t_unq, "ok", "LOW"),
      Flag.SkewKurt = ifelse((abs(sk) > t_skew) & (kt > t_kurt), "OUT", "ok")
    )
  }

  # now apply function to all cols and add iCode column
  stats_tab <- lapply(x, stats_i)
  stats_tab <- Reduce(rbind, stats_tab)
  stats_tab <- cbind(iCode = names(x), stats_tab)


  # OUTPUT ------------------------------------------------------------------

  # sfs
  if(!is.null(nsignif)){
    stats_tab <- signif_df(stats_tab, nsignif)
  }

  stats_tab

}


#' Statistics of columns/indicators
#'
#' Generic function for reports various statistics from a data frame or coin. See method documentation:
#'
#' * [get_stats.data.frame()]
#' * [get_stats.coin()]
#'
#' See also `vignette("analysis")`.
#'
#' This function replaces the now-defunct `getStats()` from COINr < v1.0.
#'
#' @param x Object (data frame or coin)
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' #
#'
#' @return A data frame of statistics for each column
#'
#' @export
get_stats <- function(x, ...){
  UseMethod("get_stats")
}
