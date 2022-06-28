# QUICK FUNCTIONS

#' Quick normalisation of a purse
#'
#' This is a wrapper function for [Normalise()], which offers a simpler syntax but less flexibility. It
#' normalises data sets within a purse using a specified function `f_n` which is used to normalise each indicator, with
#' additional function arguments passed by `f_n_para`. By default, `f_n = "n_minmax"` and `f_n_para` is
#' set so that the indicators are normalised using the min-max method, between 0 and 100.
#'
#' Essentially, this function is similar to [Normalise()] but brings parameters into the function arguments
#' rather than being wrapped in a list. It also does not allow individual normalisation.
#'
#' Normalisation can either be performed independently on each coin, or over the entire panel data set
#' simultaneously. See the discussion in [Normalise.purse()] and `vignette("normalise")`.
#'
#' @param x A purse
#' @param dset Name of data set to normalise
#' @param f_n Name of a normalisation function (as a string) to apply to each indicator. Default `"n_minmax"`.
#' @param f_n_para Any further arguments to pass to `f_n`, as a named list.
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data frame
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will be taken from the `iMeta` table in the coin, if available.
#' @param global Logical: if `TRUE`, normalisation is performed "globally" across all coins, by using e.g. the
#' max and min of each indicator in any coin. This effectively makes normalised scores comparable between coins
#' because they are all scaled using the same parameters. Otherwise if `FALSE`, coins are normalised individually.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse with normalised data sets
#' @export
#'
#' @examples
#' # build example purse
#' purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)
#'
#' # normalise using min-max, globally
#' purse <- qNormalise(purse, dset = "Raw", global = TRUE)
#'
qNormalise.purse <- function(x, dset, f_n = "n_minmax", f_n_para = list(l_u = c(0,100)),
                             directions = NULL, global = TRUE, ...){

  # assemble default specs
  specs_def <- list(f_n = f_n,
                    f_n_para = f_n_para)

  # normalise
  Normalise.purse(x, dset = dset, global_specs = specs_def, directions = directions,
                  global = global, write_to = NULL)

}

#' Quick normalisation of a coin
#'
#' This is a wrapper function for [Normalise()], which offers a simpler syntax but less flexibility. It
#' normalises a data set within a coin using a specified function `f_n` which is used to normalise each indicator, with
#' additional function arguments passed by `f_n_para`. By default, `f_n = "n_minmax"` and `f_n_para` is
#' set so that the indicators are normalised using the min-max method, between 0 and 100.
#'
#' Essentially, this function is similar to [Normalise()] but brings parameters into the function arguments
#' rather than being wrapped in a list. It also does not allow individual normalisation.
#'
#' See [Normalise()] documentation for more details, and `vignette("normalise")`.
#'
#' @param x A coin
#' @param dset Name of data set to normalise
#' @param f_n Name of a normalisation function (as a string) to apply to each indicator. Default `"n_minmax"`.
#' @param f_n_para Any further arguments to pass to `f_n`, as a named list.
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data frame
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will be taken from the `iMeta` table in the coin, if available.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated coin with normalised data set.
#' @export
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # normalise raw data set using min max, but change to scale 1-10
#' coin <- qNormalise(coin, dset = "Raw", f_n = "n_minmax",
#'                    f_n_para = list(l_u = c(1,10)))
#'
qNormalise.coin <- function(x, dset, f_n = "n_minmax", f_n_para = list(l_u = c(0,100)),
                            directions = NULL, ...){

  # write log
  coin <- write_log(x, dont_write = "x", write2log = TRUE)

  # assemble default specs
  specs_def <- list(f_n = f_n,
                    f_n_para = f_n_para)

  # normalise
  Normalise.coin(coin, dset = dset, global_specs = specs_def,
                 directions = directions, out2 = "coin", write2log = FALSE)

}


#' Quick normalisation of a data frame
#'
#' This is a wrapper function for [Normalise()], which offers a simpler syntax but less flexibility. It
#' normalises a data frame using a specified function `f_n` which is used to normalise each column, with
#' additional function arguments passed by `f_n_para`. By default, `f_n = "n_minmax"` and `f_n_para` is
#' set so that the columns of `x` are normalised using the min-max method, between 0 and 100.
#'
#' Essentially, this function is similar to [Normalise()] but brings parameters into the function arguments
#' rather than being wrapped in a list. It also does not allow individual normalisation.
#'
#' See [Normalise()] documentation for more details, and `vignette("normalise")`.
#'
#'
#' @param x A numeric data frame
#' @param f_n Name of a normalisation function (as a string) to apply to each column of `x`. Default `"n_minmax"`.
#' @param f_n_para Any further arguments to pass to `f_n`, as a named list. If `f_n = "n_minmax"`, this defaults
#' to `list(l_u = c(0,100))` (scale between 0 and 100).
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data frame
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will all be assigned as `1`. Non-numeric columns do not need
#' to have directions assigned.
#' @param ... arguments passed to or from other methods.
#'
#' @return A normalised data frame
#' @export
#'
#' @examples
#' # some made up data
#' X <- data.frame(uCode = letters[1:10],
#'                 a = runif(10),
#'                 b = runif(10)*100)
#' # normalise (defaults to min-max)
#' qNormalise(X)
#'
qNormalise.data.frame <- function(x, f_n = "n_minmax", f_n_para = NULL,
                                  directions = NULL, ...){

  # default para
  if(f_n == "n_minmax"){
    if(is.null(f_n_para)){
      f_n_para <- list(l_u = c(0,100))
    }
  }

  # assemble default specs
  specs_def <- list(f_n = f_n,
                    f_n_para = f_n_para)

  # normalise
  Normalise.data.frame(x, global_specs = specs_def, directions = directions)

}


#' Quick normalisation
#'
#' This is a generic wrapper function for [Normalise()], which offers a simpler syntax but less flexibility.
#'
#' See individual method documentation:
#'
#' * [qNormalise.data.frame()]
#' * [qNormalise.coin()]
#' * [qNormalise.purse()]
#'
#' @param x Object to be normalised
#' @param ... arguments passed to or from other methods.
#'
#' @return A normalised object
#'
#' @export
qNormalise <- function (x, ...){
  UseMethod("qNormalise")
}

#' Quick outlier treatment of a purse
#'
#' A simplified version of [Treat()] which allows direct access to the default parameters. This has less flexibility,
#' but is an easier interface and probably more convenient if the objective is to use the default treatment process
#' but with some minor adjustments.
#'
#' This function simply applies the same data treatment to each coin. See documentation for [Treat.coin()],
#' [qTreat.coin()] and `vignette("treat")`.
#'
#' @param x A purse
#' @param dset Name of data set to treat for outliers in each coin
#' @param winmax Maximum number of points to Winsorise for each indicator. Default 5.
#' @param skew_thresh Absolute skew threshold - default 2.
#' @param kurt_thresh Kurtosis threshold - default 3.5.
#' @param f2 Function to call if Winsorisation does not bring skew and kurtosis within limits. Default `"log_CT"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse
#' @export
#'
#' @examples
#' #
qTreat.purse <- function(x, dset, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5, f2 = "log_CT",
                         ...){

  # pass args to specs list
  global_specs <- list(f1 = "winsorise",
                        f1_para = list(winmax = winmax,
                                       skew_thresh = skew_thresh,
                                       kurt_thresh = kurt_thresh),
                        f2 = f2,
                        f_pass_para = list(skew_thresh = skew_thresh,
                                           kurt_thresh = kurt_thresh))

  # treat
  Treat.purse(x, dset = dset, global_specs = global_specs)

}

#' Quick outlier treatment of a coin
#'
#' A simplified version of [Treat()] which allows direct access to the default parameters. This has less flexibility,
#' but is an easier interface and probably more convenient if the objective is to use the default treatment process
#' but with some minor adjustments.
#'
#' This function treats each indicator in the data set targeted by `dset` using the following process:
#'
#' * First, it checks whether skew and kurtosis are within the specified limits of `skew_thresh` and `kurt_thresh`
#' * If the indicator is not within the limits, it applies the [winsorise()] function, with maximum number of winsorised
#' points specified by `winmax`.
#' * If winsorisation does not bring the indicator within the skew/kurtosis limits, it is instead passed to `f2`, which is
#' a second outlier treatment function, default [log_CT()].
#'
#' The arguments of [qTreat()] are passed to [Treat()].
#'
#' See [Treat()] documentation for more details, and `vignette("treat")`.
#'
#' @param x A coin
#' @param dset Name of data set to treat for outliers
#' @param winmax Maximum number of points to Winsorise for each indicator. Default 5.
#' @param skew_thresh Absolute skew threshold - default 2.
#' @param kurt_thresh Kurtosis threshold - default 3.5.
#' @param f2 Function to call if Winsorisation does not bring skew and kurtosis within limits. Default `"log_CT"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated coin with treated data set at `.$Data$Treated`.
#' @export
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # treat with winmax = 3
#' coin <- qTreat(coin, dset = "Raw", winmax = 3)
#'
qTreat.coin <- function(x, dset, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5,
                        f2 = "log_CT", ...){

  # write log
  coin <- write_log(x, dont_write = "x", write2log = TRUE)

  # pass args to specs list
  global_specs <- list(f1 = "winsorise",
                        f1_para = list(winmax = winmax,
                                       skew_thresh = skew_thresh,
                                       kurt_thresh = kurt_thresh),
                        f2 = f2,
                        f_pass_para = list(skew_thresh = skew_thresh,
                                           kurt_thresh = kurt_thresh))

  # treat (note, don't write to log here since it has been written by qTreat)
  Treat.coin(coin, dset = dset, global_specs = global_specs, out2 = "coin", write2log = FALSE)
}


#' Quick outlier treatment of a data frame
#'
#' A simplified version of [Treat()] which allows direct access to the default parameters. This has less flexibility,
#' but is an easier interface and probably more convenient if the objective is to use the default treatment process
#' but with some minor adjustments.
#'
#' This function treats each column in `x` using the following process:
#'
#' * First, it checks whether skew and kurtosis are within the specified limits of `skew_thresh` and `kurt_thresh`
#' * If the column is not within the limits, it applies the [winsorise()] function, with maximum number of winsorised
#' points specified by `winmax`.
#' * If winsorisation does not bring the column within the skew/kurtosis limits, it is instead passed to `f2`, which is
#' a second outlier treatment function, default [log_CT()].
#'
#' The arguments of [qTreat()] are passed to [Treat()].
#'
#' See [Treat()] documentation for more details, and `vignette("treat")`.
#'
#' @param x A numeric data frame
#' @param winmax Maximum number of points to Winsorise for each column. Default 5.
#' @param skew_thresh Absolute skew threshold - default 2.
#' @param kurt_thresh Kurtosis threshold - default 3.5.
#' @param f2 Function to call if Winsorisation does not bring skew and kurtosis within limits. Default `"log_CT"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return A list
#' @export
#'
#' @examples
#' # select three indicators
#' df1 <- ASEM_iData[c("Flights", "Goods", "Services")]
#'
#' # treat data frame, changing winmax and skew/kurtosis limits
#' l_treat <- qTreat(df1, winmax = 1, skew_thresh = 1.5, kurt_thresh = 3)
#'
#' # Now we check what the results are:
#' l_treat$Dets_Table
#'
qTreat.data.frame <- function(x, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5,
                              f2 = "log_CT", ...){

  # pass args to specs list
  global_specs <- list(f1 = "winsorise",
                        f1_para = list(winmax = winmax,
                                       skew_thresh = skew_thresh,
                                       kurt_thresh = kurt_thresh),
                        f2 = f2,
                        f_pass_para = list(skew_thresh = skew_thresh,
                                           kurt_thresh = kurt_thresh))

  # treat
  Treat.data.frame(x, global_specs = global_specs)
}


#' Quick outlier treatment
#'
#' This is a generic wrapper function for [Treat()]. It offers a simpler syntax but less flexibility.
#'
#' See individual method documentation:
#'
#' * [qTreat.data.frame()]
#' * [qTreat.coin()]
#' * [qTreat.purse()]
#'
#' @param x Object to be normalised.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # See individual method examples
#'
#' @return A treated object
#'
#' @export
qTreat <- function (x, ...){
  UseMethod("qTreat")
}
