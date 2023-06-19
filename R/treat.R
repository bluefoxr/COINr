#' Treat a purse of coins for outliers
#'
#' This function calls [Treat.coin()] for each coin in the purse. See the documentation of that function for
#' details. See also `vignette("treat")`.
#'
#' @param x A purse object
#' @param dset The data set to treat in each coin.
#' @param global_specs Default specifications. See details in [Treat.coin()].
#' @param indiv_specs Individual specifications. See details in [Treat.coin()].
#' @param combine_treat By default, if `f1` fails to pass `f_pass`, then `f2` is applied to the original `x`,
#' rather than the treated output of `f1`. If `combine_treat = TRUE`, `f2` will instead be applied to the output
#' of `f1`, so the two treatments will be combined.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Treated"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse with new treated data sets added at `.$Data$Treated` in each coin, plus
#' analysis information at `.$Analysis$Treated`
#' @export
#'
#' @examples
#' # See `vignette("treat")`.
Treat.purse <- function(x, dset, global_specs = NULL, indiv_specs = NULL,
                         combine_treat = FALSE, write_to = NULL, ...){

  # input check
  check_purse(x)

  # apply treatment to each coin
  x$coin <- lapply(x$coin, function(coin){
    Treat.coin(coin, dset = dset, global_specs = global_specs,
                indiv_specs = indiv_specs, combine_treat = combine_treat, write_to = write_to)
  })
  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}


#' Treat a data set in a coin for outliers
#'
#' Operates a two-stage data treatment process on the data set specified by `dset`, based on two data treatment functions, and a pass/fail
#' function which detects outliers. The method of data treatment can be either specified by the `global_specs` argument (which applies
#' the same specifications to all indicators in the specified data set), or else (additionally) by the `indiv_specs` argument which allows different
#' methods to be applied for each indicator. See details. For a simpler function for data treatment, see the wrapper function [qTreat()].
#'
#' @details
#' # Global specifications
#'
#' If the same method of data treatment should be applied to all indicators, use the `global_specs` argument. This argument takes a structured
#' list which looks like this:
#'
#' ```
#' global_specs = list(f1 = .,
#'                     f1_para = list(.),
#'                     f2 = .,
#'                     f2_para = list(.),
#'                     f_pass = .,
#'                     f_pass_para = list()
#'                     )
#' ```
#'
#' The entries in this list correspond to arguments in [Treat.numeric()], and the meanings of each are also described in more detail here
#' below. In brief, `f1` is the name of a function to apply at the first round of data treatment, `f1_para` is a list of any additional
#' parameters to pass to `f1`, `f2` and `f2_para` are equivalently the function name and parameters of the second round of data treatment, and
#' `f_pass` and `f_pass_para` are the function and additional arguments to check for the existence of outliers.
#'
#' The default values for `global_specs` are as follows:
#'
#' ```
#' global_specs = list(f1 = "winsorise",
#'                      f1_para = list(na.rm = TRUE,
#'                                     winmax = 5,
#'                                     skew_thresh = 2,
#'                                     kurt_thresh = 3.5,
#'                                     force_win = FALSE),
#'                      f2 = "log_CT",
#'                      f2_para = list(na.rm = TRUE),
#'                      f_pass = "check_SkewKurt",
#'                      f_pass_para = list(na.rm = TRUE,
#'                                         skew_thresh = 2,
#'                                         kurt_thresh = 3.5))
#' ```
#'
#' This shows that by default (i.e. if `global_specs` is not specified), each indicator is checked for outliers by the [check_SkewKurt()] function, which
#' uses skew and kurtosis thresholds as its parameters. Then, if outliers exist, the first function [winsorise()] is applied, which also
#' uses skew and kurtosis parameters, as well as a maximum number of winsorised points. If the Winsorisation function does not satisfy
#' `f_pass`, the [log_CT()] function is invoked.
#'
#' To change the global specifications, you don't have to supply the whole list. If, for example, you are happy with all the defaults but
#' want to simply change the maximum number of Winsorised points, you could specify e.g. `global_specs = list(f1_para = list(winmax = 3))`.
#' In other words, a subset of the list can be specified, as long as the structure of the list is correct.
#'
#' # Individual specifications
#'
#' The `indiv_specs` argument allows different specifications for each indicator. This is done by wrapping multiple lists of the format of the
#' list described in `global_specs` into one single list, named according to the column names of `x`. For example, if the date set has indicators with codes
#' "x1", "x2" and "x3", we could specify individual treatment as follows:
#'
#' ```
#' indiv_specs = list(x1 = list(.),
#'                    x2 = list(.)
#'                    x3 = list(.))
#' ```
#'
#' where each `list(.)` is a specifications list of the same format as `global_specs`. Any indicators that are *not* named in `indiv_specs` are
#' treated using the specifications from `global_specs` (which will be the defaults if it is not specified). As with `global_specs`,
#' a subset of the `global_specs` list may be specified for
#' each entry. Additionally, as a special case, specifying a list entry as e.g. `x1 = "none"` will apply no data treatment to the indicator "x1". See
#' `vignette("treat")` for examples of individual treatment.
#'
#' # Function methodology
#'
#' This function is set up to allow any functions to be passed as the
#' data treatment functions (`f1` and `f2`), as well as any function to be passed as the outlier detection
#' function `f_pass`, as specified in the `global_specs` and `indiv_specs` arguments.
#'
#' The arrangement of this function is inspired by a fairly standard data treatment process applied to
#' indicators, which consists of checking skew and kurtosis, then if the criteria are not met, applying
#' Winsorisation up to a specified limit. Then if Winsorisation still does not bring skew and kurtosis
#' within limits, applying a nonlinear transformation such as log or Box-Cox.
#'
#' This function generalises this process by using the following general steps:
#'
#' 1. Check if variable passes or fails using `f_pass`
#' 2. If `f_pass` returns `FALSE`, apply `f1`, else return `x` unmodified
#' 3. Check again using *`f_pass`
#' 4. If `f_pass` still returns `FALSE`, apply `f2`
#' 5. Return the modified `x` as well as other information.
#'
#' For the "typical" case described above `f1` is a Winsorisation function, `f2` is a nonlinear transformation
#' and `f_pass` is a skew and kurtosis check. Parameters can be passed to each of these three functions in
#' a named list, for example to specify a maximum number of points to Winsorise, or Box-Cox parameters, or anything
#' else. The constraints are that:
#'
#' * All of `f1`, `f2` and `f_pass` must follow the format `function(x, f_para)`, where `x` is a
#' numerical vector, and `f_para` is a list of other function parameters to be passed to the function, which
#' is specified by `f1_para` for `f1` and similarly for the other functions. If the function has no parameters
#' other than `x`, then `f_para` can be omitted.
#' * `f1` and `f2` should return either a list with `.$x` as the modified numerical vector, and any other information
#' to be attached to the list, OR, simply `x` as the only output.
#' * `f_pass` must return a logical value, where `TRUE` indicates that the `x` passes the criteria (and
#' therefore doesn't need any (more) treatment), and `FALSE` means that it fails to meet the criteria.
#'
#' See also `vignette("treat")`.
#'
#' @param x A coin
#' @param dset A named data set available in `.$Data`
#' @param global_specs A list specifying the treatment to apply to all columns. This will be applied to all columns, except any
#' that are specified in the `indiv_specs` argument. Alternatively, set to `"none"` to apply no treatment. See details.
#' @param indiv_specs A list specifying any individual treatment to apply to specific columns, overriding `global_specs`
#' for those columns. See details.
#' @param combine_treat By default, if `f1` fails to pass `f_pass`, then `f2` is applied to the original `x`,
#' rather than the treated output of `f1`. If `combine_treat = TRUE`, `f2` will instead be applied to the output
#' of `f1`, so the two treatments will be combined.
#' @param out2 The type of function output: either `"coin"` to return an updated coin, or `"list"` to return a
#' list with treated data and treatment details.
#' @param write2log Logical: if `FALSE`, the arguments of this function are not written to the coin log, so this
#' function will not be invoked when regenerating. Recommend to keep `TRUE` unless you have a good reason to do otherwise.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Treated"`.
#' @param ... arguments passed to or from other methods.
#' @param disable Logical: if `TRUE` will disable data treatment completely and write the unaltered data set. This option is mainly useful
#' in sensitivity and uncertainty analysis (to test the effect of turning imputation on/off).
#'
#' @return An updated coin with a new data set `.Data$Treated` added, plus analysis information in
#' `.$Analysis$Treated`.
#' @export
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin")
#'
#' # treat raw data set
#' coin <- Treat(coin, dset = "Raw")
#'
#' # summary of treatment for each indicator
#' head(coin$Analysis$Treated$Dets_Table)
#'
Treat.coin <- function(x, dset, global_specs = NULL, indiv_specs = NULL,
                       combine_treat = FALSE, out2 = "coin", write_to = NULL,
                       write2log = TRUE, disable = FALSE, ...){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x", write2log = write2log)

  # potentially skip all imputation
  stopifnot(is.logical(disable))
  if(disable){
    idata <- get_dset(coin, dset = dset)
    # output list
    if(out2 == "list"){
      message("Returning data frame since treatment was disabled.")
      return(idata)
    } else {
      if(is.null(write_to)){
        write_to <- "Treated"
      }
      return(write_dset(coin, idata, dset = write_to))
    }
  }

  # GET DSET, CHECKS --------------------------------------------------------

  iData <- get_dset(coin, dset)

  # TREAT DATA --------------------------------------------------------------

  l_treat <- Treat(iData, global_specs = global_specs,
                    indiv_specs = indiv_specs, combine_treat = combine_treat)

  # output list
  if(out2 == "list"){
    l_treat
  } else {
    if(is.null(write_to)){
      write_to <- "Treated"
    }
    coin <- write_dset(coin, l_treat$x_treat, dset = write_to)
    write2coin(coin, l_treat[names(l_treat) != "x_treat"], out2, "Analysis", write_to)
  }

}


#' Treat a data frame for outliers
#'
#' Operates a two-stage data treatment process, based on two data treatment functions, and a pass/fail
#' function which detects outliers. The method of data treatment can be either specified by the `global_specs` argument (which applies
#' the same specifications to all columns in `x`), or else (additionally) by the `indiv_specs` argument which allows different
#' methods to be applied for each column. See details. For a simpler function for data treatment, see the wrapper function [qTreat()].
#'
#' @details
#' # Global specifications
#'
#' If the same method of data treatment should be applied to all the columns, use the `global_specs` argument. This argument takes a structured
#' list which looks like this:
#'
#' ```
#' global_specs = list(f1 = .,
#'                     f1_para = list(.),
#'                     f2 = .,
#'                     f2_para = list(.),
#'                     f_pass = .,
#'                     f_pass_para = list()
#'                     )
#' ```
#'
#' The entries in this list correspond to arguments in [Treat.numeric()], and the meanings of each are also described in more detail here
#' below. In brief, `f1` is the name of a function to apply at the first round of data treatment, `f1_para` is a list of any additional
#' parameters to pass to `f1`, `f2` and `f2_para` are equivalently the function name and parameters of the second round of data treatment, and
#' `f_pass` and `f_pass_para` are the function and additional arguments to check for the existence of outliers.
#'
#' The default values for `global_specs` are as follows:
#'
#' ```
#' global_specs = list(f1 = "winsorise",
#'                      f1_para = list(na.rm = TRUE,
#'                                     winmax = 5,
#'                                     skew_thresh = 2,
#'                                     kurt_thresh = 3.5,
#'                                     force_win = FALSE),
#'                      f2 = "log_CT",
#'                      f2_para = list(na.rm = TRUE),
#'                      f_pass = "check_SkewKurt",
#'                      f_pass_para = list(na.rm = TRUE,
#'                                         skew_thresh = 2,
#'                                         kurt_thresh = 3.5))
#' ```
#'
#' This shows that by default (i.e. if `global_specs` is not specified), each column is checked for outliers by the [check_SkewKurt()] function, which
#' uses skew and kurtosis thresholds as its parameters. Then, if outliers exist, the first function [winsorise()] is applied, which also
#' uses skew and kurtosis parameters, as well as a maximum number of winsorised points. If the Winsorisation function does not satisfy
#' `f_pass`, the [log_CT()] function is invoked.
#'
#' To change the global specifications, you don't have to supply the whole list. If, for example, you are happy with all the defaults but
#' want to simply change the maximum number of Winsorised points, you could specify e.g. `global_specs = list(f1_para = list(winmax = 3))`.
#' In other words, a subset of the list can be specified, as long as the structure of the list is correct.
#'
#' # Individual specifications
#'
#' The `indiv_specs` argument allows different specifications for each column in `x`. This is done by wrapping multiple lists of the format of the
#' list described in `global_specs` into one single list, named according to the column names of `x`. For example, if `x` has column names
#' "x1", "x2" and "x3", we could specify individual treatment as follows:
#'
#' ```
#' indiv_specs = list(x1 = list(.),
#'                    x2 = list(.)
#'                    x3 = list(.))
#' ```
#'
#' where each `list(.)` is a specifications list of the same format as `global_specs`. Any columns that are not named in `indiv_specs` are
#' treated using the specifications from `global_specs` (which will be the defaults if it is not specified). As with `global_specs`,
#' a subset of the `global_specs` list may be specified for
#' each entry. Additionally, as a special case, specifying a list entry as e.g. `x1 = "none"` will apply no data treatment to the column "x1". See
#' `vignette("treat")` for examples of individual treatment.
#'
#' # Function methodology
#'
#' This function is set up to allow any functions to be passed as the
#' data treatment functions (`f1` and `f2`), as well as any function to be passed as the outlier detection
#' function `f_pass`, as specified in the `global_specs` and `indiv_specs` arguments.
#'
#' The arrangement of this function is inspired by a fairly standard data treatment process applied to
#' indicators, which consists of checking skew and kurtosis, then if the criteria are not met, applying
#' Winsorisation up to a specified limit. Then if Winsorisation still does not bring skew and kurtosis
#' within limits, applying a nonlinear transformation such as log or Box-Cox.
#'
#' This function generalises this process by using the following general steps:
#'
#' 1. Check if variable passes or fails using `f_pass`
#' 2. If `f_pass` returns `FALSE`, apply `f1`, else return `x` unmodified
#' 3. Check again using *`f_pass`
#' 4. If `f_pass` still returns `FALSE`, apply `f2`
#' 5. Return the modified `x` as well as other information.
#'
#' For the "typical" case described above `f1` is a Winsorisation function, `f2` is a nonlinear transformation
#' and `f_pass` is a skew and kurtosis check. Parameters can be passed to each of these three functions in
#' a named list, for example to specify a maximum number of points to Winsorise, or Box-Cox parameters, or anything
#' else. The constraints are that:
#'
#' * All of `f1`, `f2` and `f_pass` must follow the format `function(x, f_para)`, where `x` is a
#' numerical vector, and `f_para` is a list of other function parameters to be passed to the function, which
#' is specified by `f1_para` for `f1` and similarly for the other functions. If the function has no parameters
#' other than `x`, then `f_para` can be omitted.
#' * `f1` and `f2` should return either a list with `.$x` as the modified numerical vector, and any other information
#' to be attached to the list, OR, simply `x` as the only output.
#' * `f_pass` must return a logical value, where `TRUE` indicates that the `x` passes the criteria (and
#' therefore doesn't need any (more) treatment), and `FALSE` means that it fails to meet the criteria.
#'
#' See also `vignette("treat")`.
#'
#' @param x A data frame. Can have both numeric and non-numeric columns.
#' @param global_specs A list specifying the treatment to apply to all columns. This will be applied to all columns, except any
#' that are specified in the `indiv_specs` argument. Alternatively, set to `"none"` to apply no treatment. See details.
#' @param indiv_specs A list specifying any individual treatment to apply to specific columns, overriding `global_specs`
#' for those columns. See details.
#' @param combine_treat By default, if `f1` fails to pass `f_pass`, then `f2` is applied to the original `x`,
#' rather than the treated output of `f1`. If `combine_treat = TRUE`, `f2` will instead be applied to the output
#' of `f1`, so the two treatments will be combined.
#' @param ... arguments passed to or from other methods.
#'
#' @importFrom utils modifyList
#'
#' @examples
#' # select three indicators
#' df1 <- ASEM_iData[c("Flights", "Goods", "Services")]
#'
#' # treat the data frame using defaults
#' l_treat <- Treat(df1)
#'
#' # details of data treatment for each column
#' l_treat$Dets_Table
#'
#' @return A treated data frame of data
#'
#' @export
Treat.data.frame <- function(x, global_specs = NULL, indiv_specs = NULL, combine_treat = FALSE, ...){


  # SET DEFAULTS ------------------------------------------------------------

  # default treatment for all cols
  specs_def <- list(f1 = "winsorise",
                    f1_para = list(na.rm = TRUE,
                                   winmax = 5,
                                   skew_thresh = 2,
                                   kurt_thresh = 3.5,
                                   force_win = FALSE),
                    f2 = "log_CT",
                    f2_para = list(na.rm = TRUE),
                    f_pass = "check_SkewKurt",
                    f_pass_para = list(na.rm = TRUE,
                                       skew_thresh = 2,
                                       kurt_thresh = 3.5))
  # modify using input
  if(!is.null(global_specs)){
    if(is.character(global_specs)){
      stopifnot(length(global_specs) == 1)
      if(global_specs != "none"){
        stop("global_specs must either be a list or else 'none'.")
      }
    } else {
      stopifnot(is.list(global_specs))
      specs_def <- utils::modifyList(specs_def, global_specs)
    }
  }

  # individual: check and flag for later function
  indiv <- !is.null(indiv_specs)
  if(indiv){
    stopifnot(is.list(indiv_specs))
  }

  # TREAT COLS --------------------------------------------------------------

  # function for treating a column
  treat_col <- function(col_name){

    # get col and check if numeric
    xi <- x[[col_name]]
    if(!is.numeric(xi)){
      return(list(x = xi))
    }

    # get specs
    if(indiv){
      # check if spec for that col
      if(col_name %in% names(indiv_specs)){
        # lookup spec
        indiv_specs_col <- indiv_specs[[col_name]]
        # check if "none"
        if(is.character(indiv_specs_col) && length(indiv_specs_col) == 1){
          if(indiv_specs_col == "none"){
            return(list(x = xi))
          }
        }
        # merge with defaults (overwrites any differences)
        specs <- utils::modifyList(specs_def, indiv_specs_col)
      } else {
        # otherwise, use defaults
        specs <- specs_def
      }
    } else {
      # otherwise, use defaults
      specs <- specs_def
      if(is.character(specs) && length(specs) == 1){
        if(specs == "none"){
          return(list(x = xi))
        }
      }
    }

    # run function allowing for catching errors
    tryCatch(
      expr = do.call("Treat.numeric", c(list(x = xi, combine_treat = combine_treat), specs)),
      error = function(e){
        warning("Indicator could not be treated at column '", col_name, "' - returning untreated indicator. May be due to lack of unique values.", call. = FALSE)
        list(x = xi)
      }
    )
  }

  # now run function
  # output is one list
  treat_results <- lapply(names(x), treat_col)
  names(treat_results) <- names(x)

  # ORGANISE AND OUTPUT -----------------------------------------------------

  # the treated data frame
  x_treat <- as.data.frame(lapply(treat_results, `[`, "x"))
  names(x_treat) <- names(x)

  # a table of treatment information
  details <- lapply(treat_results, function(x) unlist(x$Dets_Table, recursive = F))
  not_null <- lengths(details) != 0
  details <- details[not_null]
  details_table <- Reduce(rbind_fill, details)
  details_table <- data.frame(iCode = names(x)[not_null], details_table)

  # a list of any remaining treatment info that can't go in a table
  details_list <- lapply(treat_results, `[[`, "Dets_List")
  details_list <- tidy_list(details_list)

  # ADD TREATED POINTS
  Treated_Points <- lapply(treat_results, `[[`, "Treated_Points")
  Treated_Points <- as.data.frame(tidy_list(Treated_Points))

  # output
  l_out <- list(x_treat = x_treat,
       Dets_Table = details_table,
       Treated_Points = Treated_Points,
       Dets_List = details_list)
  tidy_list(l_out)

}


#' Treat a numeric vector for outliers
#'
#' Operates a two-stage data treatment process, based on two data treatment functions, and a pass/fail
#' function which detects outliers. This function is set up to allow any functions to be passed as the
#' data treatment functions (`f1` and `f2`), as well as any function to be passed as the outlier detection
#' function `f_pass`.
#'
#' The arrangement of this function is inspired by a fairly standard data treatment process applied to
#' indicators, which consists of checking skew and kurtosis, then if the criteria are not met, applying
#' Winsorisation up to a specified limit. Then if Winsorisation still does not bring skew and kurtosis
#' within limits, applying a nonlinear transformation such as log or Box-Cox.
#'
#' This function generalises this process by using the following general steps:
#'
#' 1. Check if variable passes or fails using `f_pass`
#' 2. If `f_pass` returns `FALSE`, apply `f1`, else return `x` unmodified
#' 3. Check again using *`f_pass`
#' 4. If `f_pass` still returns `FALSE`, apply `f2` (by default to the original `x`, see `combine_treat`
#' parameter)
#' 5. Return the modified `x` as well as other information.
#'
#' For the "typical" case described above `f1` is a Winsorisation function, `f2` is a nonlinear transformation
#' and `f_pass` is a skew and kurtosis check. Parameters can be passed to each of these three functions in
#' a named list, for example to specify a maximum number of points to Winsorise, or Box-Cox parameters, or anything
#' else. The constraints are that:
#'
#' * All of `f1`, `f2` and `f_pass` must follow the format `function(x, f_para)`, where `x` is a
#' numerical vector, and `f_para` is a list of other function parameters to be passed to the function, which
#' is specified by `f1_para` for `f1` and similarly for the other functions. If the function has no parameters
#' other than `x`, then `f_para` can be omitted.
#' * `f1` and `f2` should return either a list with `.$x` as the modified numerical vector, and any other information
#' to be attached to the list, OR, simply `x` as the only output.
#' * `f_pass` must return a logical value, where `TRUE` indicates that the `x` passes the criteria (and
#' therefore doesn't need any (more) treatment), and `FALSE` means that it fails to meet the criteria.
#'
#' See also `vignette("treat")`.
#'
#' @param x A numeric vector.
#' @param f1 First stage data treatment function e.g. as a string.
#' @param f1_para First stage data treatment function parameters as a named list.
#' @param f2 First stage data treatment function as a string.
#' @param f2_para First stage data treatment function parameters as a named list.
#' @param combine_treat By default, if `f1` fails to pass `f_pass`, then `f2` is applied to the original `x`,
#' rather than the treated output of `f1`. If `combine_treat = TRUE`, `f2` will instead be applied to the output
#' of `f1`, so the two treatments will be combined.
#' @param f_pass A string specifying an outlier detection function - see details. Default `"check_SkewKurt"`
#' @param f_pass_para Any further arguments to pass to `f_pass()`, as a named list.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # numbers between 1 and 10
#' x <- 1:10
#'
#' # two outliers
#' x <- c(x, 30, 100)
#'
#' # check whether passes skew/kurt test
#' check_SkewKurt(x)
#'
#' # treat using winsorisation
#' l_treat <- Treat(x, f1 = "winsorise", f1_para = list(winmax = 2),
#'                  f_pass = "check_SkewKurt")
#'
#' # plot original against treated
#' plot(x, l_treat$x)
#'
#' @return A treated vector of data.
#'
#' @export
Treat.numeric <- function(x, f1, f1_para = NULL, f2 = NULL, f2_para = NULL,
                           f_pass, f_pass_para = NULL, combine_treat = FALSE, ...){

  # INPUT CHECKS ------------------------------------------------------------

  # check function for input functions
  check_fx <- function(f, f_para){
    stopifnot(is.character(f),
              length(f) == 1
    )
    if(!is.null(f_para)){
      if(!is.list(f_para)){
        stop("Parameters of " ,f, " are required to be wrapped in a list.")
      }
    }
  }
  # apply check function to each input function
  check_fx(f1, f1_para)
  check_fx(f_pass, f_pass_para)
  # f2 is optional
  if(!is.null(f2)){
    check_fx(f2, f2_para)
    n_f <- 2
  } else {
    n_f <- 1
  }

  # set up lists for recording any info from functions
  l_table <- vector(mode = "list") # for outputs to go into a table
  l_list <- vector(mode = "list") # for outputs to go into a list

  # df for recording treatment of individual points
  df_treat <- as.data.frame(matrix("", nrow = length(x) ,ncol = n_f))
  colnames(df_treat) <- c(f1, f2)

  # PASS CHECK -------------------------------------------------------------------
  # Requires function which returns TRUE = PASS or FALSE = fail, and optionally
  # attaches some extra information (e.g. skew and kurtosis values)

  proc_passing <- function(l, f_name, suffix){

    if(is.list(l)){

      # get pass/fail
      pass1 <- l$Pass
      if(is.null(pass1)){
        stop("Required list entry .$Pass of output of ",f_name," is not found.")
      }
      # check if l contains any sub-lists
      sub_lists <- sapply(l, is.list)
      # collect outputs for table (not x, and no lists)
      l_table[[paste0(f_name, suffix)]] <<- l[!sub_lists]
      # collect any other outputs (not x, lists)
      l_list[[paste0(f_name, suffix)]] <<- l[sub_lists]

    } else if (is.logical(l)) {
      pass1 <- l
      # collect outputs for table (not x, and no lists)
      l_table[[paste0(f_name, suffix)]] <<- pass1
    } else {
      stop("Output of ",f_name,"is not either a list with entry .$Pass or a logical")
    }

    if(length(pass1) != 1){
      stop("Logical output from ",f_name," is not of length 1.")
    }
    if(!is.logical(pass1)){
      stop("Output of f_pass is not logical - this is not allowed.")
    }
    if(is.na(pass1)){
      warning("f_pass has returned NA. Returning untreated vector.")
    }
    pass1
  }

  # INITIAL CHECK

  passing <- do.call(what = f_pass, args = c(list(x = x), f_pass_para))
  pass <- proc_passing(passing, f_pass, 0)
  # check output
  if(is.na(pass)){
    return(list(x = x,
                Passing = NA))
  }

  # FUNC PROCESSING -----------------------------------------------------------------

  # func to extract f1 and f2 outputs, write to list and check outputs
  # this is necessary because f1 and f2 may output either a numeric vector
  # or a list, plus optionally some other information.
  proc_output <- function(l, f_name){

    if(is.list(l)){

      # get modified x
      x1 <- l$x
      if(is.null(x1)){
        stop("Required list entry .$x of output of ",f_name," is not found.")
      }

      # get positions of treated points
      x_treat <- l$treated
      if(is.null(x_treat)){
        stop("Required list entry .$treated of output of ",f_name," is not found.")
      } else {
        if(!is.character(x_treat)){
          stop(".$treated of output of ",f_name," is not a character vector.")
        }
        if(length(x_treat) != length(x)){
          stop(".$treated of output of ",f_name," is not the same length as x.")
        }
        df_treat[, f_name] <<- x_treat
      }

      # check if l contains any sub-lists
      sub_lists <- sapply(l, is.list)
      # collect outputs for table (not x, and no lists)
      l_table[[f_name]] <<- l[(names(l) %nin% c("x", "treated")) & !sub_lists]
      # collect any other outputs (not x, lists)
      l_list[[f_name]] <<- l[(names(l) %nin% c("x", "treated")) & sub_lists]

    } else if (is.numeric(l)) {
      x1 <- l
    } else {
      stop("Output of ",f_name,"is not either a list with entry .$x or a numeric vector")
    }

    if(length(x1) != length(x)){
      stop("Vector output from ",f_name," is not the same length as x")
    }
    x1
  }

  # TREATMENT 1 -------------------------------------------------------------

  if(!pass){
    # treat data with f1
    l_f1 <- do.call(what = f1, args = c(list(x = x), f1_para))
    # sort output (also writes any extra info to l_table
    x1 <- proc_output(l_f1, f1)
    # check (for deciding whether to go to treatment 2)
    passing <- do.call(what = f_pass, args = c(list(x = x1), f_pass_para))
    pass <- proc_passing(passing, f_pass, 1)
    # check output
    if(is.na(pass)){
      return(list(x = x,
                  Passing = NA))
    }
  } else {
    x1 <- x
  }

  # TREATMENT 2 -------------------------------------------------------------

  if(!pass & n_f == 2){
    # optionally reset treatment 1 to original
    if(!combine_treat){
      x1 <- x
    }
    l_f2 <- do.call(what = f2, args = c(list(x = x1), f2_para))
    # sort output (also writes any extra info to l_table
    x2 <- proc_output(l_f2, f2)
    # check if passes again
    passing <- do.call(what = f_pass, args = c(list(x = x2), f_pass_para))
    pass <- proc_passing(passing, f_pass, 2)
    # check output
    if(is.na(pass)){
      return(list(x = x,
                  Passing = NA))
    }
  } else {
    x2 <- x1
  }

  # OUTPUT ------------------------------------------------------------------

  # First, glue cols of treated points record
  # remove NULL cols of df_treat
  df_treat <- df_treat[!is.null(colnames(df_treat))]

  # combine cols into one
  if(ncol(df_treat) > 1){
    Treat_Points <- apply(df_treat, MARGIN = 1, function(z){
      if(z[2] == ""){
        z[1]
      } else {
        paste(z, collapse = "+")
      }
    })
    Treat_Points[Treat_Points == "+"] <- ""
  } else {
    Treat_Points <- df_treat
  }

  list(x = x2,
       Dets_Table = l_table,
       Treated_Points = Treat_Points,
       Dets_List = tidy_list(l_list))

}


#' Treat outliers
#'
#' Generic function for treating outliers using a two-step process. See individual method documentation:
#'
#' * [Treat.numeric()]
#' * [Treat.data.frame()]
#' * [Treat.coin()]
#' * [Treat.purse()]
#'
#' See also `vignette("treat")`.
#'
#' This function replaces the now-defunct `treat()` from COINr < v1.0.
#'
#' @param x Object to be treated
#' @param ... arguments passed to or from other methods.
#'
#' @return Treated object plus details.
#'
#' @export
Treat <- function (x, ...){
  UseMethod("Treat")
}


#' Winsorise a vector
#'
#' Follows a "standard" Winsorisation approach: points are successively Winsorised in order to bring
#' skew and kurtosis thresholds within specified limits. Specifically, aims to bring absolute skew to
#' below a threshold (default 2.25) and kurtosis below another threshold (default 3.5).
#'
#' Winsorisation here is defined as reassigning the point with the highest/lowest value with the value of the
#' next highest/lowest point. Whether to Winsorise at the high or low end of the scale is decided by the direction
#' of the skewness of `x`.
#'
#' This function replaces the now-defunct `coin_win()` from COINr < v1.0.
#'
#' @param x A numeric vector.
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#' @param winmax Maximum number of points to Winsorise. Default 5. Set `NULL` to have no limit.
#' @param skew_thresh A threshold for absolute skewness (positive). Default 2.25.
#' @param kurt_thresh A threshold for kurtosis. Default 3.5.
#' @param force_win Logical: if `TRUE`, forces winsorisation up to winmax (regardless of skew/kurt).
#' Default `FALSE`. Note - this option should be used with care because the direction of Winsorisation
#' is based on the direction of skew. Successively Winsorising can switch the direction of skew and hence
#' the direction of Winsorisation, which may not produce the expected behaviour.
#'
#' @examples
#' # numbers between 1 and 10
#' x <- 1:10
#'
#' # two outliers
#' x <- c(x, 30, 100)
#'
#' # winsorise
#' l_win <- winsorise(x, skew_thresh = 2, kurt_thresh = 3.5)
#'
#' # see treated vector, number of winsorised points and details
#' l_win
#'
#' @return A list containing winsorised data, number of winsorised points, and the individual points that
#' were treated.
#'
#' @export
winsorise <- function(x, na.rm = FALSE, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5,
                          force_win = FALSE){

  # test skew and kurtosis
  passing <- check_SkewKurt(x, na.rm = na.rm,
                            skew_thresh = skew_thresh,
                            kurt_thresh = kurt_thresh)[["Pass"]]

  # set winsorisation counter
  nwin <- 0
  # vectors to record indices of winsorised points. Set NULL to begin with to keep track of
  # number of points in a sensible way. This is also passed through if no Winsorisation happens.
  imax<-imin<-NULL

  # if doesn't pass, go to winsorisation
  if(!passing | force_win){
    # set winsorisation limit logical flag. Use function because reused later.
    f_below_winmax <- function(){
      if(is.null(winmax)){
        TRUE
      } else {
        nwin < winmax
      }
    }
    below_winmax <- f_below_winmax()

    # else go to Winsorisation
    while((!passing | force_win) & below_winmax){

      # winsorise depending on whether outliers are high or low
      if(skew(x, na.rm = TRUE)>=0){ # skew is positive, implies high outliers

        imax <- which(x==max(x, na.rm = T)) # position(s) of maximum value(s)
        x[imax] <- max(x[-imax], na.rm = T) # replace imax with max value of indicator if imax value(s) excluded

      } else { # skew is negative, implies low outliers

        imin <- which(x==min(x, na.rm = T)) # ditto, but with min
        x[imin] <- min(x[-imin], na.rm = T)
      }

      # count number winsorised points. Defined this way because it is possible we Winsorise
      # two points at once if they are tied.
      nwin <- sum(length(imax) + length(imin))

      # setting winmax is NULL implies no limit on winsorisation
      below_winmax <- f_below_winmax()

      # check if it passes now
      passing <- check_SkewKurt(x, na.rm = na.rm,
                                skew_thresh = skew_thresh,
                                kurt_thresh = kurt_thresh)[["Pass"]]
    }
  }

  # return winsorised vector, plus positions of winsorised points
  treated <- rep("", length(x))
  treated[imax] <- "winhi"
  treated[imin] <- "winlo"

  list(
    x = x,
    nwin = nwin,
    treated = treated
  )

}

#' Log-transform a vector
#'
#' Performs a log transform on a numeric vector. This function is currently not recommended - see comments
#' below.
#'
#' Specifically, this performs a "GII log" transform, which is what was encoded in the GII2020 spreadsheet.
#'
#' Note that this transformation is currently NOT recommended because it seems quite volatile and can flip
#' the direction of the indicator. If the maximum value of the indicator is less than one, this reverses the
#' direction.
#'
#' @param x A numeric vector.
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#'
#' @examples
#' x <- runif(20)
#' log_GII(x)
#'
#' @return A log-transformed vector of data.
#'
#' @export
log_GII <- function(x, na.rm = FALSE){

  stopifnot(is.numeric(x),
            is.vector(x))

  mx <- max(x, na.rm = na.rm)
  mn <- min(x, na.rm = na.rm)

  x1 <- log(
    (mx - 1)*(x - mn) / (mx-mn) + 1
    )

  #x2 <- log( (max(x, na.rm = na.rm)-1)*(x-min(x, na.rm = na.rm))/
         #(max(x, na.rm = na.rm)-min(x, na.rm = na.rm)) + 1 )

  list(x = x,
       treated = rep("log_GII", length(x)))
}


#' Log-transform a vector
#'
#' Performs a log transform on a numeric vector.
#'
#' Specifically, this performs a modified "COIN Tool log" transform: `log(x-min(x) + a)`, where
#' `a <- 0.01*(max(x)-min(x))`.
#'
#' @param x A numeric vector.
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#'
#' @examples
#' x <- runif(20)
#' log_CT(x)
#'
#' @return A log-transformed vector of data, and treatment details wrapped in a list.
#'
#' @export
log_CT <- function(x, na.rm = FALSE){

  stopifnot(is.numeric(x))

  x <- log(x- min(x,na.rm = na.rm) + 0.01*(max(x, na.rm = na.rm)-min(x, na.rm = na.rm)))

  list(x = x,
       treated = rep("log_CT", length(x)))
}

#' Log transform a vector (skew corrected)
#'
#' Performs a log transform on a numeric vector, but with consideration for the direction of the skew. The aim
#' here is to reduce the absolute value of skew, regardless of its direction.
#'
#' Specifically:
#'
#' If the skew of `x` is positive, this performs a modified "COIN Tool log" transform: `log(x-min(x) + a)`, where
#' `a <- 0.01*(max(x)-min(x))`.
#'
#' If the skew of `x` is negative, it performs an equivalent transformation `-log(xmax + a - x)`.
#'
#' @param x A numeric vector
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#'
#' @return A log-transformed vector of data, and treatment details wrapped in a list.
#' @export
#'
#' @examples
#' x <- runif(20)
#' log_CT(x)
#'
log_CT_plus <- function(x, na.rm = FALSE){

  stopifnot(is.numeric(x))

  xmax <- max(x, na.rm = TRUE)
  xmin <- min(x, na.rm = TRUE)
  a <- 0.01 * (xmax - xmin)

  if(skew(x, na.rm = TRUE) > 0){

    list(x = log(x - xmin + a),
         treated = rep("log_CT", length(x)))

  } else {

    list(x = -log(xmax + a - x),
         treated = rep("log_CT_neg", length(x)))
  }
}

#' Log-transform a vector
#'
#' Performs a log transform on a numeric vector.
#'
#' Specifically, this performs a "COIN Tool log" transform: `log(x-min(x) + 1)`.
#'
#' @param x A numeric vector.
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#'
#' @examples
#' x <- runif(20)
#' log_CT_orig(x)
#'
#' @return A log-transformed vector of data, and treatment details wrapped in a list.
#'
#' @export
log_CT_orig <- function(x, na.rm = FALSE){

  stopifnot(is.numeric(x))

  x <- log(x- min(x, na.rm = na.rm) + 1)

  list(x = x,
       treated = rep("log_CT_orig", length(x)))
}


#' Box Cox transformation
#'
#' Simple Box Cox, with no optimisation of lambda.
#'
#' This function replaces the now-defunct `BoxCox()` from COINr < v1.0.
#'
#' @param x A vector or column of data to transform
#' @param lambda The lambda parameter of the Box Cox transform
#' @param makepos If `TRUE` (default) makes all values positive by subtracting the minimum and adding 1.
#' @param na.rm If `TRUE`, `NA`s will be removed: only relevant if `makepos = TRUE` which invokes `min()`.
#'
#' @examples
#' # example data
#' x <- runif(30)
#' # Apply Box Cox
#' xBox <- boxcox(x, lambda = 2)
#' # plot one against the other
#' plot(x, xBox)
#'
#' @return A vector of length `length(x)` with transformed values.
#'
#' @export

boxcox <- function(x, lambda, makepos = TRUE, na.rm = FALSE){

  stopifnot(is.numeric(x),
            is.vector(x))

  if(makepos){
    # make positive using COIN Tool style shift
    x <- x - min(x,na.rm = na.rm) + 1
  }

  # Box Cox
  if (lambda==0){
    log(x)
  } else {
    (x^lambda - 1)/lambda
  }
}


#' Calculate skewness
#'
#' Calculates skewness of the values of a numeric vector. This uses the same definition of skewness as
#' the "skewness()" function in the "e1071" package where `type == 2`, which is equivalent to the definition of skewness used in Excel.
#'
#' @param x A numeric vector.
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#'
#' @examples
#' x <- runif(20)
#' skew(x)
#'
#' @return A skewness value (scalar).
#'
#' @export
skew <- function(x, na.rm = FALSE){

  stopifnot(is.numeric(x),
            is.vector(x))

  if(any(is.na(x))){
    if(na.rm){
      x <- x[!is.na(x)]
    } else {
      return(NA)
    }
  }

  n <- length(x)

  # need min 3 points to work
  if(n<3){
    return(NA)
  }

  # calculate skewness. NOTE this is taken from e1071::skewness() to avoid dependencies.
  x <- x - mean(x)
  y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
  y <- y * sqrt(n * (n - 1))/(n - 2)
  return(y)

}

#' Calculate kurtosis
#'
#' Calculates kurtosis of the values of a numeric vector. This uses the same definition of kurtosis as
#' as the "kurtosis()" function in the e1071 package, where `type == 2`, which is equivalent to the definition of kurtosis used in Excel.
#'
#' @param x A numeric vector.
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#'
#' @examples
#' x <- runif(20)
#' kurt(x)
#'
#' @return A kurtosis value (scalar).
#'
#' @export
kurt <- function(x, na.rm = FALSE){

  stopifnot(is.numeric(x),
            is.vector(x))

  if(any(is.na(x))){
    if(na.rm){
      x <- x[!is.na(x)]
    } else {
      return(NA)
    }
  }

  n <- length(x)
  # need min 4 points to work
  if(n<4){
    return(NA)
  }

  # demean and calculate kurtosis. NOTE this is taken from e1071::kurtosis() to avoid dependencies.
  x <- x - mean(x)
  r <- n * sum(x^4)/(sum(x^2)^2)
  y <- ((n + 1) * (r - 3) + 6) * (n - 1)/((n - 2) * (n - 3))
  return(y)
}

#' Check skew and kurtosis of a vector
#'
#' Logical test: if `abs(skewness) < skew_thresh` OR `kurtosis < kurt_thresh`, returns `TRUE`, else `FALSE`
#'
#' @param x A numeric vector.
#' @param na.rm Set `TRUE` to remove `NA` values, otherwise returns `NA`.
#' @param skew_thresh A threshold for absolute skewness (positive). Default 2.25.
#' @param kurt_thresh A threshold for kurtosis. Default 3.5.
#'
#' @examples
#' set.seed(100)
#' x <- runif(20)
#' # this passes
#' check_SkewKurt(x)
#' # if we add an outlier, doesn't pass
#' check_SkewKurt(c(x, 1000))
#'
#' @return A list with `.$Pass` is a Logical, where `TRUE` is pass, `FALSE` is fail, and `.$Details` is a
#' sub-list with skew and kurtosis values.
#'
#' @export
check_SkewKurt <- function(x, na.rm = FALSE, skew_thresh = 2, kurt_thresh = 3.5){

  # get skew and kurtosis
  sk <- skew(x, na.rm = na.rm)
  kt <- kurt(x, na.rm = na.rm)

  # logical test
  ans <- (abs(sk) < skew_thresh) | (kt < kurt_thresh)

  # make sure output is sensible
  stopifnot(is.logical(ans),
            length(ans)==1)

  # output
  list(Pass = ans, Skew = sk, Kurt = kt)
}
