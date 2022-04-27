# QUICK FUNCTIONS

#' Quick normalisation of a purse
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
#' @return An updated purse
#' @export
#'
#' @examples
#' #
qNormalise.purse <- function(x, dset, f_n = "n_minmax", f_n_para = list(l_u = c(0,100)),
                             directions = NULL, global = TRUE, ...){

  # assemble default specs
  specs_def <- list(f_n = f_n,
                    f_n_para = f_n_para)

  # normalise
  Normalise.purse(x, dset = dset, default_specs = specs_def, directions = directions,
                  global = global, write_to = NULL)

}

#' Quick normalisation of a coin
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
#' @return An updated coin
#' @export
#'
#' @examples
#' #
qNormalise.coin <- function(x, dset, f_n = "n_minmax", f_n_para = list(l_u = c(0,100)),
                            directions = NULL, ...){

  # write log
  coin <- write_log(x, dont_write = "x", write2log = TRUE)

  # assemble default specs
  specs_def <- list(f_n = f_n,
                    f_n_para = f_n_para)

  # normalise
  Normalise.coin(coin, dset = dset, default_specs = specs_def,
                 directions = directions, out2 = "coin", write2log = FALSE)

}


#' Quick normalisation of a data frame
#'
#' @param x A numeric data frame
#' @param f_n Name of a normalisation function (as a string) to apply to each column of `x`. Default `"n_minmax"`.
#' @param f_n_para Any further arguments to pass to `f_n`, as a named list.
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data frame
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will all be assigned as `1`. Non-numeric columns do not need
#' to have directions assigned.
#' @param ... arguments passed to or from other methods.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' #
qNormalise.data.frame <- function(x, f_n = "n_minmax", f_n_para = list(l_u = c(0,100)),
                                  directions = NULL, ...){

  # assemble default specs
  specs_def <- list(f_n = f_n,
                    f_n_para = f_n_para)

  # normalise
  Normalise.data.frame(x, default_specs = specs_def, directions = directions)

}


#' Quick normalisation
#'
#' @param x Thing
#' @param ... thing
#'
#' @return message
#'
#' @export
qNormalise <- function (x, ...){
  UseMethod("qNormalise")
}

#' Quick outlier treatment of a purse
#'
#' @param x A purse
#' @param dset Name of data set to treat for outliers in each coin
#' @param winmax Maximum number of points to Winsorise for each indicator. Default 5.
#' @param skew_thresh Absolute skew threshold - default 2.
#' @param kurt_thresh Kurtosis threshold - default 3.5.
#' @param f2 Function to call if Winsorisation does not bring skew and kurtosis within limits. Default `"log_GII"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse
#' @export
#'
#' @examples
#' #
qTreat.purse <- function(x, dset, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5, f2 = "log_GII",
                         ...){

  # pass args to specs list
  default_specs <- list(f1 = "winsorise",
                        f1_para = list(winmax = winmax,
                                       skew_thresh = skew_thresh,
                                       kurt_thresh = kurt_thresh),
                        f2 = f2,
                        f_pass_para = list(skew_thresh = skew_thresh,
                                           kurt_thresh = kurt_thresh))

  # treat
  Treat.purse(x, dset = dset, default_specs = default_specs)

}

#' Quick outlier treatment of a coin
#'
#' @param x A coin
#' @param dset Name of data set to treat for outliers
#' @param winmax Maximum number of points to Winsorise for each indicator. Default 5.
#' @param skew_thresh Absolute skew threshold - default 2.
#' @param kurt_thresh Kurtosis threshold - default 3.5.
#' @param f2 Function to call if Winsorisation does not bring skew and kurtosis within limits. Default `"log_GII"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated coin
#' @export
#'
#' @examples
#' #
qTreat.coin <- function(x, dset, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5,
                        f2 = "log_GII", ...){

  # write log
  coin <- write_log(x, dont_write = "x", write2log = TRUE)

  # pass args to specs list
  default_specs <- list(f1 = "winsorise",
                        f1_para = list(winmax = winmax,
                                       skew_thresh = skew_thresh,
                                       kurt_thresh = kurt_thresh),
                        f2 = f2,
                        f_pass_para = list(skew_thresh = skew_thresh,
                                           kurt_thresh = kurt_thresh))

  # treat (note, don't write to log here since it has been written by qTreat)
  Treat.coin(coin, dset = dset, default_specs = default_specs, out2 = "coin", write2log = FALSE)
}


#' Quick outlier treatment of a data frame
#'
#' A simplified version of [Treat()] which allows direct access to the default parameters. This has less flexibility,
#' but is an easier interface and probably more convenient if the objective is to use the default treatment process
#' but with some minor adjustments.
#'
#' @param x A numeric data frame
#' @param winmax Maximum number of points to Winsorise for each column. Default 5.
#' @param skew_thresh Absolute skew threshold - default 2.
#' @param kurt_thresh Kurtosis threshold - default 3.5.
#' @param f2 Function to call if Winsorisation does not bring skew and kurtosis within limits. Default `"log_GII"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return A list
#' @export
#'
#' @examples
#' #
qTreat.data.frame <- function(x, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5,
                              f2 = "log_GII", ...){

  # pass args to specs list
  default_specs <- list(f1 = "winsorise",
                        f1_para = list(winmax = winmax,
                                       skew_thresh = skew_thresh,
                                       kurt_thresh = kurt_thresh),
                        f2 = f2,
                        f_pass_para = list(skew_thresh = skew_thresh,
                                           kurt_thresh = kurt_thresh))

  # treat
  Treat.data.frame(x, default_specs = default_specs)
}


#' Quick outlier treatment
#'
#' @param x Thing
#' @param ... thing
#'
#' @return message
#'
#' @export
qTreat <- function (x, ...){
  UseMethod("qTreat")
}
