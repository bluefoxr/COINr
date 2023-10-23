#' Create normalised data sets in a purse of coins
#'
#' This creates normalised data sets for each coin in the purse. In most respects, this works in a similar way
#' to normalising on a coin, for which reason please see [Normalise.coin()] for most documentation. There is however
#' a special case in terms of operating on a purse of coins. This is because, when
#' dealing with time series data, it is often desirable to normalise over the whole panel data set at once
#' rather than independently for each time point. This makes the resulting index and aggregates comparable
#' over time. Here, the `global` argument controls whether to normalise each coin independently or to normalise
#' across all data at once. In other respects, this function behaves the same as [Normalise.coin()].
#'
#' The same specifications are passed to each coin in the purse. This means that each coin is normalised
#' using the same set of specifications and directions. If you need control over individual coins, you
#' will have to normalise coins individually.
#'
#' @param x A purse object
#' @param dset The data set to normalise in each coin
#' @param global_specs Default specifications
#' @param indiv_specs Individual specifications
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data set
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will be taken from the `iMeta` table in the coin, if available.
#' @param global Logical: if `TRUE`, normalisation is performed "globally" across all coins, by using e.g. the
#' max and min of each indicator in any coin. This effectively makes normalised scores comparable between coins
#' because they are all scaled using the same parameters. Otherwise if `FALSE`, coins are normalised individually.
#' @param write_to Optional character string for naming the data set in each coin. Data will be written to
#' `.$Data[[write_to]]`. Default is `write_to == "Normalised"`.
#' @param ... arguments passed to or from other methods.
#'
#' @return An updated purse with new normalised data sets added at `.$Data$Normalised` in each coin
#' @export
#'
#' @examples
#' # build example purse
#' purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)
#'
#' # normalise raw data set
#' purse <- Normalise(purse, dset = "Raw", global = TRUE)
#'
Normalise.purse <- function(x, dset, global_specs = NULL, indiv_specs = NULL,
                             directions = NULL, global = TRUE, write_to = NULL, ...){

  # input check
  check_purse(x)

  # GET DSETS ---------------------------------------------------------------

  iDatas <- get_dset(x, dset)
  iDatas_ <- iDatas[names(iDatas) != "Time"]


  # GLOBAL NORMALISATION ----------------------------------------------------

  if(global){

    # get directions first
    if(is.null(directions)){
      directions <- x$coin[[1]]$Meta$Ind[c("iCode", "Direction")]
    }

    # run global dset through normalise (as data frame), excluding Time col
    iDatas_n <- Normalise(iDatas_, global_specs = global_specs,
                           indiv_specs = indiv_specs, directions = directions)
    # split by Time
    iDatas_n_l <- split(iDatas_n, iDatas$Time)

    # now write dsets to coins
    x$coin <- lapply(x$coin, function(coin){

      # get Time
      tt <- coin$Meta$Unit$Time[[1]]
      if(is.null(tt)){
        stop("Time index is NULL or not found in writing normalised data set to coin.")
      }

      if(is.null(write_to)){
        write_to <- "Normalised"
      }

      # write dset first
      coin <- write_dset(coin, iDatas_n_l[[which(names(iDatas_n_l) == tt)]], dset = write_to)

      # also write to log - we signal that coin can't be regenerated any more
      coin$Log$can_regen <- FALSE
      coin$Log$message <- "Coin was normalised inside a purse with global = TRUE. Cannot be regenerated."

      coin
    })

  } else {

    # apply independent normalisation to each coin
    x$coin <- lapply(x$coin, function(coin){
      Normalise.coin(coin, dset = dset, global_specs = global_specs,
                      indiv_specs = indiv_specs, directions = directions,
                      out2 = "coin", write_to = write_to)
    })

  }

  # make sure still purse class
  class(x) <- c("purse", "data.frame")
  x
}

#' Create a normalised data set
#'
#' Creates a normalised data set using specifications specified in `global_specs`. Columns of `dset` can also optionally be
#' normalised with individual specifications using the `indiv_specs` argument. If indicators should have their
#' directions reversed, this can be specified using the `directions` argument. Non-numeric columns are ignored
#' automatically by this function. By default, this function normalises each indicator using the "min-max" method, scaling indicators to lie between
#' 0 and 100. This calls the [n_minmax()] function. Note, all COINr normalisation functions are of the form `n_*()`.
#'
#' ## Global specification
#'
#' The `global_specs` argument is a list which specifies the normalisation function and any function parameters
#' that should be used to normalise the indicators found in the data set. Unless `indiv_specs` is specified, this will be applied
#' to all indicators. The list should have two entries:
#'
#' * `.$f_n`: the name of the function to use to normalise each indicator
#' * `.$f_n_para`: any further parameters to pass to `f_n`, apart from the numeric vector (each column of the data set)
#'
#' In this list, `f_n` should be a character string which is the name of a normalisation
#' function. For example, `f_n = "n_minmax"` calls the [n_minmax()] function. `f_n_para` is a list of any
#' further arguments to `f_n`. This means that any function can be passed to [Normalise()], as long as its
#' first argument is `x`, a numeric vector, and it returns a numeric vector of the same length. See [n_minmax()]
#' for an example.
#'
#' `f_n_para` is *required* to be a named list. So e.g. if we define a function `f1(x, arg1, arg2)` then we should
#' specify `f_n = "f1"`, and `f_n_para = list(arg1 = val1, arg2 = val2)`, where `val1` and `val2` are the
#' values assigned to the arguments `arg1` and `arg2` respectively.
#'
#' The default list for `global_specs` is: `list(f_n = "n_minmax", f_n_para = list(l_u = c(0,100)))`, i.e.
#' min-max normalisation between 0 and 100.
#'
#' Note, all COINr normalisation functions (passed to `f_n`) are of the form `n_*()`. Type `n_` in the R Studio console and press the Tab key to see a list.
#'
#' ## Individual parameter specification with iMeta
#'
#' For some normalisation methods we may use the same function for all indicators but use different parameters - for example, using
#' distance to target normalisation or goalpost normalisation. COINr now supports specifying these parameters in the `iMeta` table.
#' To enable this, set `f_n_para = "use_iMeta"` within the `global_specs` list.
#'
#' For this to work you will also need to add the correct-named columns in the `iMeta` table. To see which column names to add, check the
#' function documentation of the normalisation function you wish to use (e.g. [n_goalposts()]). See also examples in the
#' [normalisation vignette](https://bluefoxr.github.io/COINr/articles/normalise.html). These columns should be added before construction of
#' the coin.
#'
#' ## Individual column specification
#'
#' To give full individual control, indicators can be normalised with different normalisation functions and parameters using the
#' `indiv_specs` argument. This must be specified as a named list e.g. `list(i1 = specs1, i2 = specs2)` where
#' `i1` and `i2` are `iCode`s to apply individual normalisation to, and `specs1` and `specs2` are
#' respectively lists of the same format as `global_specs` (see above). In other words, `indiv_specs` is a big
#' list wrapping together `global_specs`-style lists. Any `iCode`s not named in `indiv_specs` (
#' i.e. those not in `names(indiv_specs)`) are normalised using the specifications from `global_specs`. So
#' `indiv_specs` lists the exceptions to `global_specs`.
#'
#' See also `vignette("normalise")` for more details.
#'
#' @param x A coin
#' @param dset A named data set found in `.$Data`
#' @param global_specs Specifications to apply to all columns, apart from those specified by `indiv_specs`. See details.
#' @param indiv_specs Specifications applied to specific columns, overriding those specified in `global_specs`.
#' See details.
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data set
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will be taken from the `iMeta` table in the coin, if available.
#' @param out2 Either `"coin"` to return normalised data set back to the coin, or `df` to simply return a data
#' frame.
#' @param write_to Optional character string for naming the data set in the coin. Data will be written to
#' `.$Data[[write_to]]`. Default is `write_to == "Normalised"`.
#' @param write2log Logical: if `FALSE`, the arguments of this function are not written to the coin log, so this
#' function will not be invoked when regenerating. Recommend to keep `TRUE` unless you have a good reason to do otherwise.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin")
#'
#' # normalise the raw data set
#' coin <- Normalise(coin, dset = "Raw")
#'
#' @return An updated coin
#' @export
Normalise.coin <- function(x, dset, global_specs = NULL, indiv_specs = NULL,
                           directions = NULL, out2 = "coin", write_to = NULL,
                           write2log = TRUE, ...){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x", write2log = write2log)

  # GET DSET, DEFAULTS ------------------------------------------------------

  iData <- get_dset(coin, dset)
  iData_ <- iData[colnames(iData) != "uCode"]


  # DIRECTIONS --------------------------------------------------------------

  if(is.null(directions)){
    # get direction col from iMeta
    dirs_c <- coin$Meta$Ind[c("iCode", "Direction")]
    # if empty
    if(is.null(dirs_c)){
      stop("No directions provided, and no directions found in .$Meta$Ind")
    }
  } else {
    dirs_c <- directions
  }

  # NORMALISE DATA ----------------------------------------------------------

  # retrieve parameters from iMeta if necessary using dedicated function
  if(!is.null(global_specs[["f_n"]])){
    if(identical(global_specs[["f_n_para"]], "use_iMeta")){
      indiv_specs <- get_iMeta_norm_paras(coin, func_name = global_specs[["f_n"]])
    }
  }

  iData_n <- Normalise(iData_, global_specs = global_specs, indiv_specs = indiv_specs,
                       directions = dirs_c)

  # reunite with uCode col
  iData_n <- cbind(uCode = iData$uCode, iData_n)

  # output list
  if(out2 == "df"){
    iData_n
  } else {
    if(is.null(write_to)){
      write_to <- "Normalised"
    }
    write_dset(coin, iData_n, dset = write_to)
  }
}


#' Normalise a data frame
#'
#' Normalises a data frame using specifications specified in `global_specs`. Columns can also optionally be
#' normalised with individual specifications using the `indiv_specs` argument. If variables should have their
#' directions reversed, this can be specified using the `directions` argument. Non-numeric columns are ignored
#' automatically by this function. By default, this function normalises each indicator using the "min-max" method, scaling indicators to lie between
#' 0 and 100. This calls the [n_minmax()] function. Note, all COINr normalisation functions are of the form `n_*()`.
#'
#' ## Global specification
#'
#' The `global_specs` argument is a list which specifies the normalisation function and any function parameters
#' that should be used to normalise the columns of `x`. Unless `indiv_specs` is specified, this will be applied
#' to all numeric columns of `x`. The list should have two entries:
#'
#' * `.$f_n`: the name of the function to use to normalise each column
#' * `.$f_n_para`: any further parameters to pass to `f_n`, apart from the numeric vector (each column of `x`)
#'
#' In this list, `f_n` should be a character string which is the name of a normalisation
#' function. For example, `f_n = "n_minmax"` calls the [n_minmax()] function. `f_n_para` is a list of any
#' further arguments to `f_n`. This means that any function can be passed to [Normalise()], as long as its
#' first argument is `x`, a numeric vector, and it returns a numeric vector of the same length. See [n_minmax()]
#' for an example.
#'
#' `f_n_para` is *required* to be a named list. So e.g. if we define a function `f1(x, arg1, arg2)` then we should
#' specify `f_n = "f1"`, and `f_n_para = list(arg1 = val1, arg2 = val2)`, where `val1` and `val2` are the
#' values assigned to the arguments `arg1` and `arg2` respectively.
#'
#' The default list for `global_specs` is: `list(f_n = "n_minmax", f_n_para = list(l_u = c(0,100)))`.
#'
#' Note, all COINr normalisation functions (passed to `f_n`) are of the form `n_*()`. Type `n_` in the R Studio console and press the Tab key to see a list.
#'
#' ## Individual column specification
#'
#' Optionally, columns of `x` can be normalised with different normalisation functions and parameters using the
#' `indiv_specs` argument. This must be specified as a named list e.g. `list(i1 = specs1, i2 = specs2)` where
#' `i1` and `i2` are column names of `x` to apply individual normalisation to, and `specs1` and `specs2` are
#' respectively lists of the same format as `global_specs` (see above). In other words, `indiv_specs` is a big
#' list wrapping together `global_specs`-style lists. Any numeric columns of `x` not named in `indiv_specs` (
#' i.e. those not in `names(indiv_specs)`) are normalised using the specifications from `global_specs`. So
#' `indiv_specs` lists the exceptions to `global_specs`.
#'
#' See also `vignette("normalise")` for more details.
#'
#' @param x A data frame
#' @param global_specs Specifications to apply to all columns, apart from those specified by `indiv_specs`. See details.
#' @param indiv_specs Specifications applied to specific columns, overriding those specified in `global_specs`. See details.
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data frame
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will all be assigned as `1`. Non-numeric columns do not need
#' to have directions assigned.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' iris_norm <- Normalise(iris)
#' head(iris_norm)
#'
#' @return A normalised data frame
#' @export
Normalise.data.frame <- function(x, global_specs = NULL, indiv_specs = NULL,
                               directions = NULL, ...){

  # CHECKS ------------------------------------------------------------------

  # most input checks are performed in Normalise.numeric()

  if(is.null(directions)){
    directions <- data.frame(iCode = names(x),
                             Direction = rep(1, ncol(x)))
  }
  if(!is.data.frame(directions)){
    stop("'directions' must be specified as a data frame.")
  }
  if(any(colnames(directions) %nin% c("iCode", "Direction"))){
    stop("'directions' must contain both columns 'iCode' and 'Direction'.")
  }

  # SET DEFAULTS ------------------------------------------------------------

  # default treatment for all cols
  specs_def <- list(f_n = "n_minmax",
                    f_n_para = list(l_u = c(0,100)))

  # modify using input
  if(!is.null(global_specs)){
    stopifnot(is.list(global_specs))
    #specs_def <- utils::modifyList(specs_def, global_specs)
    specs_def <- global_specs
  }

  # individual: check and flag for later function
  indiv <- !is.null(indiv_specs)
  if(indiv){
    stopifnot(is.list(indiv_specs))
  }

  # NORMALISE ---------------------------------------------------------------

  # function for normalising a column
  norm_col <- function(col_name){

    # get col and check if numeric
    xi <- x[[col_name]]
    if(!is.numeric(xi)){
      return(xi)
    }

    # get specs
    if(indiv){
      # check if spec for that col
      if(col_name %in% names(indiv_specs)){
        # lookup spec and merge with defaults (overwrites any differences)
        specs <- indiv_specs[[col_name]]
      } else {
        # otherwise, use defaults
        specs <- specs_def
      }
    } else {
      # otherwise, use defaults
      specs <- specs_def
    }

    # add direction
    specs$direction <- directions$Direction[directions$iCode == col_name]
    if(length(specs$direction) != 1){
      stop("No 'direction' entry found for numerical column ", col_name)
    }

    # run function
    do.call("Normalise.numeric", c(list(x = xi), specs))
  }

  # now run function
  # output is one list
  norm_results <- as.data.frame(lapply(names(x), norm_col))

  names(norm_results) <- names(x)

  # CHECK and OUTPUT --------------------------------------------------------
  norm_results

}


#' Normalise a numeric vector
#'
#' Normalise a numeric vector using a specified function `f_n`, with possible reversal of direction
#' using `direction`.
#'
#' Normalisation is specified using the `f_n` and `f_n_para` arguments. In these, `f_n` should be a character
#' string which is the name of a normalisation
#' function. For example, `f_n = "n_minmax"` calls the [n_minmax()] function. `f_n_para` is a list of any
#' further arguments to `f_n`. This means that any function can be passed to [Normalise()], as long as its
#' first argument is `x`, a numeric vector, and it returns a numeric vector of the same length. See [n_minmax()]
#' for an example.
#'
#' `f_n_para` is *required* to be a named list. So e.g. if we define a function `f1(x, arg1, arg2)` then we should
#' specify `f_n = "f1"`, and `f_n_para = list(arg1 = val1, arg2 = val2)`, where `val1` and `val2` are the
#' values assigned to the arguments `arg1` and `arg2` respectively.
#'
#' See also `vignette("normalise")` for more details.
#'
#' @param x Object to be normalised
#' @param f_n The normalisation method, specified as string which refers to a function of the form `f_n(x, npara)`.
#' See details. Defaults to `"n_minmax"` which is the min-max function.
#' @param f_n_para Supporting list of arguments for `f_n`. This is required to be a list.
#' @param direction If `direction = -1` the highest values of `x` will correspond to the lowest
#' values of the normalised `x`. Else if `direction = 1` the direction of `x` in unaltered.
#' @param ... arguments passed to or from other methods.
#'
#' @examples
#' # example vector
#' x <- runif(10)
#'
#' # normalise using distance to reference (5th data point)
#' x_norm <- Normalise(x, f_n = "n_dist2ref", f_n_para = list(iref = 5))
#'
#' # view side by side
#' data.frame(x, x_norm)
#'
#' @return A normalised numeric vector
#'
#' @export
Normalise.numeric <- function(x, f_n = NULL, f_n_para = NULL,
                               direction = 1, ...){

  # CHECKS ------------------------------------------------------------------

  # x must be numeric to be here. f_n will be checked by do.call()

  if(direction %nin% c(-1, 1)){
    stop("direction must be either -1 or 1")
  }

  # change direction
  x <- x*direction

  # DEFAULTS ----------------------------------------------------------------

  # minmax is default
  if(is.null(f_n)){
    f_n <- "n_minmax"
  }
  # function args
  f_args <- list(x = x)
  if(!is.null(f_n_para)){
    if(!is.list(f_n_para)){
      stop("f_n_para must be a list")
    }
    f_args <- c(f_args, f_n_para)
  }

  # NORMALISE ---------------------------------------------------------------

  # call normalisation function
  if(f_n == "none"){
    xn <- x
  } else {
    xn <- do.call(what = f_n, args = f_args)
  }

  # CHECK and OUTPUT --------------------------------------------------------

  if(length(xn) != length(x)){
    stop("length of normalised vector not equal to length of x")
  }
  if(!is.numeric(xn)){
    stop("normalised vector is not numeric")
  }

  xn

}


#' Normalise data
#'
#' This is a generic function for normalising variables and indicators, i.e. bringing them onto
#' a common scale. Please see individual method documentation depending on your data class:
#'
#' * [Normalise.numeric()]
#' * [Normalise.data.frame()]
#' * [Normalise.coin()]
#' * [Normalise.purse()]
#'
#' See also `vignette("normalise")` for more details.
#'
#' This function replaces the now-defunct `normalise()` from COINr < v1.0.
#'
#' @param x Object to be normalised
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' # See individual method documentation.
#'
#' @export
Normalise <- function(x, ...){
  UseMethod("Normalise")
}

#' Minmax a vector
#'
#' Scales a vector using min-max method.
#'
#' This function also supports parameter specification in `iMeta` for the [Normalise.coin()] method.
#' To do this, add columns `minmax_lower`, and `minmax_upper` to the `iMeta` table, which specify the
#' lower and upper bounds to scale each indicator to. Then set `f_n_para = "use_iMeta"` within the
#' `global_specs` list. See also examples in the [normalisation vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).
#'
#' @param x A numeric vector
#' @param l_u A vector `c(l, u)`, where `l` is the lower bound and `u` is the upper bound. `x` will
#' be scaled exactly onto this interval.
#'
#' @examples
#' x <- runif(20)
#' n_minmax(x)
#'
#' @return Normalised vector
#'
#' @export
n_minmax <- function(x, l_u = c(0,100)){

  stopifnot(is.numeric(x),
            is.numeric(l_u),
            length(l_u) == 2,
            all(!is.na(l_u)))

  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  if(minx == maxx){
    warning("The range of x is 0: returning vector of NaNs")
  }
  (x-minx)/(maxx - minx)*(l_u[2]-l_u[1]) + l_u[1]
}


#' Scale a vector
#'
#' Scales a vector for normalisation using the method applied in the GII2020 for some indicators. This
#' does `x_scaled <- (x-l)/(u-l) * 100`. Note this is *not* the minmax transformation (see [n_minmax()]).
#' This is a linear transformation with shift `u` and scaling factor `u-l`.
#'
#' This function also supports parameter specification in `iMeta` for the [Normalise.coin()] method.
#' To do this, add columns `scaled_lower`, and `scaled_upper` to the `iMeta` table, which specify the
#' first and second elements of `npara`, respectively. Then set `f_n_para = "use_iMeta"` within the
#' `global_specs` list. See also examples in the [normalisation vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).
#'
#' @param x A numeric vector
#' @param npara Parameters as a vector `c(l, u)`. See description.
#'
#' @examples
#' x <- runif(20)
#' n_scaled(x, npara = c(1,10))
#'
#' @return Scaled vector
#'
#' @export
n_scaled <- function(x, npara = c(0,100)){

  stopifnot(is.numeric(x),
            is.vector(x))
  (x-npara[1])/(npara[2] - npara[1])*100
}


#' Z-score a vector
#'
#' Standardises a vector `x` by scaling it to have a mean and standard deviation specified by `m_sd`.
#'
#' This function also supports parameter specification in `iMeta` for the [Normalise.coin()] method.
#' To do this, add columns `zscore_mean`, and `zscore_sd` to the `iMeta` table, which specify the
#' mean and standard deviation to scale each indicator to, respectively. Then set `f_n_para = "use_iMeta"` within the
#' `global_specs` list. See also examples in the [normalisation vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).
#'
#' @param x A numeric vector
#' @param m_sd A vector `c(m, sd)`, where `m` is desired mean and `sd` is the target standard deviation.
#'
#' @importFrom stats sd
#'
#' @examples
#' x <- runif(20)
#' n_zscore(x)
#'
#' @return Numeric vector
#'
#' @export
n_zscore <- function(x, m_sd = c(0,1)){

  stopifnot(is.numeric(x),
            is.numeric(m_sd),
            length(m_sd) == 2,
            all(!is.na(m_sd)),
            m_sd[2] > 0)

  (x-mean(x, na.rm = TRUE))/stats::sd(x, na.rm = TRUE)*m_sd[2] + m_sd[1]
}


#' Normalise as distance to maximum value
#'
#' A measure of the distance to the maximum value, where the maximum value is the highest-scoring value. The
#' formula used is:
#'
#' \deqn{ 1 - (x_{max} - x)/(x_{max} - x_{min}) }
#'
#' This means that the closer a value is to the maximum, the higher its score will be. Scores will be in the
#' range of 0 to 1.
#'
#' @param x A numeric vector
#'
#' @examples
#' x <- runif(20)
#' n_dist2max(x)
#'
#' @return Numeric vector
#'
#' @export
n_dist2max <- function(x){

  stopifnot(is.numeric(x))

  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  if(minx == maxx){
    warning("The range of x is 0: returning vector of NaNs")
  }

  1 - (maxx - x)/(maxx- minx)
}


#' Normalise as distance to reference value
#'
#' A measure of the distance to a specific value found in `x`, specified by `iref`. The formula is:
#'
#' \deqn{ 1 - (x_{ref} - x)/(x_{ref} - x_{min}) }
#'
#' Values exceeding `x_ref` can be optionally capped at 1 if `cap_max = TRUE`.
#'
#' @param x A numeric vector
#' @param iref An integer which indexes `x` to specify the reference value. The reference value will be
#' `x[iref]`.
#' @param cap_max If `TRUE`, any value of `x` that exceeds `x[iref]` will be assigned a score of 1, otherwise
#' will have a score greater than 1.
#'
#' @examples
#' x <- runif(20)
#' n_dist2ref(x, 5)
#'
#' @return Numeric vector
#'
#' @export
n_dist2ref <- function(x, iref, cap_max = FALSE){

  stopifnot(is.numeric(x),
            is.logical(cap_max),
            is.numeric(iref),
            length(iref)==1,
            iref > 0)

  if(iref > length(x)){
    stop("iref must be an integer in 1:length(x).")
  }

  minx <- min(x, na.rm = TRUE)
  # get xref, check if NA
  xref <- x[iref]
  if(is.na(xref)){
    warning("The value of x identified as the reference is NA - returning vector of NAs")
  }

  y <- 1 - (xref - x)/(xref - minx)
  if(cap_max){
    y[y>1] <- 1
  }

  y
}


#' Normalise as distance to target
#'
#' A measure of the distance of each value of `x` to a specified target which can be a high or low target depending on `direction`. See details below.
#'
#'
#' If `direction = 1`, the formula is:
#'
#' \deqn{ \frac{x - x_{min}}{x_{targ} - x_{min}} }
#'
#' else if `direction = -1`:
#'
#' \deqn{ \frac{x_{max} - x}{x_{max} - x_{targ}} }
#'
#' Values surpassing `x_targ` in either case can be optionally capped at 1 if `cap_max = TRUE`.
#'
#' This function also supports parameter specification in `iMeta` for the [Normalise.coin()] method.
#' To do this, add columns `Target`, and `dist2targ_cap_max` to the `iMeta` table, which correspond
#' to the `targ` and `cap_max` parameters respectively. Then set `f_n_para = "use_iMeta"` within the
#' `global_specs` list. See also examples in the [normalisation vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).
#'
#' @param x A numeric vector
#' @param targ An target value
#' @param direction Either 1 (default) or -1. In the former case, the indicator is assumed to be "positive" so that the target is at the higher
#' end of the range. In the latter, the indicator is "negative" so that the target is typically at the low end of the range.
#' @param cap_max If `TRUE`, any value of `x` that exceeds `targ` will be assigned a score of 1, otherwise
#' will have a score greater than 1.
#'
#' @examples
#' x <- runif(20)
#' n_dist2targ(x, 0.8, cap_max = TRUE)
#'
#' @return Numeric vector
#'
#' @export
n_dist2targ <- function(x, targ, direction = 1, cap_max = FALSE){

  stopifnot(is.numeric(x),
            is.numeric(targ),
            length(targ)==1,
            is.logical(cap_max),
            is.numeric(direction),
            length(direction) == 1)

  if(is.na(targ)){
    stop("targ is NA")
  }

  if(direction == 1){

    minx <- min(x, na.rm = TRUE)
    if(targ < minx){
      warning("targ is less than min(x) - this will produce negative scores.")
    }
    y <- (x - minx)/(targ - minx)

  } else if (direction == -1){

    maxx <- max(x, na.rm = TRUE)
    if(targ > maxx){
      warning("targ is greater than max(x) - this will produce negative scores.")
    }
    y <- (maxx - x)/(maxx- targ)

  } else {
    stop("'direction' must be either -1 or 1")
  }

  # cap
  if(cap_max){
    y[y>1] <- 1
  }

  y
}


#' Normalise as fraction of max value
#'
#' The ratio of each value of `x` to `max(x)`.
#'
#' \deqn{ x / x_{max} }
#'
#' @param x A numeric vector
#'
#' @examples
#' x <- runif(20)
#' n_fracmax(x)
#'
#' @return Numeric vector
#'
#' @export
n_fracmax <- function(x){

  stopifnot(is.numeric(x))

  maxx <- max(x, na.rm = TRUE)

  x/maxx
}


#' Normalise using percentile ranks
#'
#' Calculates percentile ranks of a numeric vector using "sport" ranking. Ranks are calculated by [base::rank()]
#' and converted to percentile ranks. The `ties.method` can be changed - this is directly passed to
#' [base::rank()].
#'
#' @param x A numeric vector
#' @param ties.method This argument is passed to [base::rank()] - see there for details.
#'
#' @examples
#' x <- runif(20)
#' n_prank(x)
#'
#' @return Numeric vector
#'
#' @export
n_prank <- function(x, ties.method = "min"){

  stopifnot(is.numeric(x))

  # ranks
  rx <- rank(x, ties.method = "min", na.last = "keep")
  # perc ranks
  (rx - 1) / (sum(!is.na(x)) - 1)

}


#' Normalise using ranks
#'
#' This is simply a wrapper for [base::rank()]. Higher scores will give higher ranks.
#'
#' @param x A numeric vector
#' @param ties.method This argument is passed to [base::rank()] - see there for details.
#'
#' @examples
#' x <- runif(20)
#' n_rank(x)
#'
#' @return Numeric vector
#'
#' @export
n_rank <- function(x, ties.method = "min"){

  stopifnot(is.numeric(x))

  # ranks
  rank(x, ties.method = "min", na.last = "keep")

}

#' Normalise using Borda scores
#'
#' Calculates Borda scores as `rank(x) - 1`.
#'
#' @param x A numeric vector
#' @param ties.method This argument is passed to [base::rank()] - see there for details.
#'
#' @examples
#' x <- runif(20)
#' n_borda(x)
#'
#' @return Numeric vector
#'
#' @export
n_borda <- function(x, ties.method = "min"){

  stopifnot(is.numeric(x))

  # ranks
  rank(x, ties.method = "min", na.last = "keep") - 1

}


#' Normalise using goalpost method
#'
#' The fraction of the distance of each value of `x` from the lower "goalpost" to the upper one. Goalposts are specified by
#' `gposts = c(l, u, a)`, where `l` is the lower bound, `u` is the upper bound, and `a` is a scaling parameter.
#'
#' Specify `direction = -1` to "flip" the goalposts. In this case, the fraction from the upper to the lower goalpost is
#' measured.
#'
#' The goalposts equations are:
#'
#' \deqn{ (x - GP_{low})/(GP_{high} - GP_{low}) }
#'
#' and for a negative directionality indicator:
#'
#' \deqn{ (x - GP_{high})/(GP_{low} - GP_{high}) }
#'
#' This function also supports parameter specification in `iMeta` for the [Normalise.coin()] method.
#' To do this, add columns:
#'
#' * `goalpost_lower`: the lower goalpost
#' * `goalpost_upper`: the upper goalpost
#' * `goalpost_scale`: the scaling parameter
#' * `goalpost_trunc2posts`: corresponds to the `trunc2posts` argument
#'
#' to the `iMeta` table. Then set `f_n_para = "use_iMeta"` within the
#' `global_specs` list. See also examples in the [normalisation vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).
#'
#' @param x A numeric vector
#' @param gposts A numeric vector `c(l, u, a)`, where `l` is the lower bound, `u` is the upper bound,
#' and `a` is a scaling parameter.
#' @param direction Either 1 or -1. Set to -1 to flip goalposts.
#' @param trunc2posts If `TRUE` (default) will truncate any values that fall outside of the goalposts.
#'
#' @examples
#' # positive direction
#' n_goalposts(1, gposts = c(0, 10, 1))
#' # negative direction
#' n_goalposts(1, gposts = c(0, 10, 1), direction = -1)
#'
#' @return Numeric vector
#'
#' @export
n_goalposts <- function(x, gposts, direction = 1, trunc2posts = TRUE){

  stopifnot(is.numeric(x))

  # since indicators arrive with directions possibly reversed (*-1), we have to also multiply GPs by -1
  if(direction == -1){
    # here, indicators are multiplied by -1, so need to also multiply goalposts by -1
    # gposts[1:2] <- -1*gposts[1:2] # NOTE COMMENTED OUT FOR CONSISTENCY

    # then, the goalpost formula is reversed as well
    y <- (x-gposts[2])/(gposts[1] - gposts[2])
  } else {
    y <- (x-gposts[1])/(gposts[2] - gposts[1])
  }

  # this is the truncation bit
  if(trunc2posts){
    y[y > 1] <- 1
    y[y < 0] <- 0
  }
  # overall scaling
  y * gposts[3]

}

# This is a convenience function used in Normalise.coin() which returns a f_n_para()
# list from special columns stored in iMeta. It hard-codes in special cases, which
# are COINr internal normalisation functions n_*()
#
# Note since this deals with normalisation, only extracts indicator-level parameters.
#
# Special note on targets: in the body of this function the target passed to the the n_dist2targ()
# function is actually the target multiplied by the direction. This flips the target to negative
# if the direction is negative. The reason is that when the data set is normalised, if an indicator
# direction is negative, COINr muliplies the indicator by -1 before passing it to the normalisation
# function. This means that to be consistent, we also flip the target. Then for n_dist2targ() the
# direction is always set as 1 (since the direction is already accounted for before the data arrives
# to the function).
#
get_iMeta_norm_paras <- function(coin, func_name){

  iMeta <- coin$Meta$Ind[coin$Meta$Ind$Type == "Indicator",]

  # define iMeta cols required by each normalisation method
  required_cols <- switch(
    func_name,
    n_minmax = c("minmax_lower", "minmax_upper"),
    n_scaled = c("scaled_lower", "scaled_upper"),
    n_zscore = c("zscore_mean", "zscore_sd"),
    n_dist2targ = c("Target", "dist2targ_cap_max", "Direction"),
    n_goalposts = c("goalpost_lower", "goalpost_upper", "goalpost_scale" , "Direction", "goalpost_trunc2posts"),
    stop("Your normalisation function '", func_name, "' does not have support for iMeta parameters. Check the list of supported functions in the Normalise.coin() documentation or manually build a parameter list using the indiv_specs argument.")
  )

  # check special case for backwards comp
  dist2targ_exception <- (func_name == "n_dist2targ") && ("dist2targ_cap_max" %nin% names(iMeta))

  # check required cols exist in iMeta
  if(any(required_cols %nin% names(iMeta))){
    # special case for backward compatibility
    if(dist2targ_exception){
      message("Normalisation parameters retrieved from iMeta but 'dist2targ_cap_max' is missing - it is assumed as FALSE. It is recommended to include this parameter as an extra column in iMeta.")
    } else {
      stop("You have specified to use normalisation parameters in the iMeta data frame but one or more required columns cannot be found.
  Required columns for noramalisation method ", func_name, " are: ", toString(required_cols))
    }
  }

  # BUILD PARAMETER LISTS
  # This has to be done "manually" because of the unknown column numbers and name changes

  # base list
  l_n <- rep(list(list(f_n = func_name)), nrow(iMeta))
  names(l_n) <- iMeta$iCode

  # dirty for loop: maps the iMeta parameters into the list with correct parameter names
  # If more normalisation functions are added as "automated" through iMeta, this is the
  # place to add the mapping.
  for(ii in 1:nrow(iMeta)){
    # get list
    l <- l_n[[ii]]

    # get paras: with special case for backwards comp
    if(dist2targ_exception){
      paras <- iMeta[iMeta$iCode == names(l_n)[ii], c("Target", "Direction")]
      paras$dist2targ_cap_max <- FALSE
    } else {
      paras <- iMeta[iMeta$iCode == names(l_n)[ii], required_cols]
    }

    # add to list component
    l$f_n_para <- switch(
      func_name,
      n_minmax = list(l_u = c(paras$minmax_lower, paras$minmax_upper)),
      n_scaled = list(npara = c(paras$scaled_lower, paras$scaled_upper)),
      n_zscore = list(m_sd = c(paras$zscore_mean, paras$zscore_sd)),
      n_dist2targ = list(
        targ = paras$Target * paras$Direction, # See note at top of function
        cap_max = paras$dist2targ_cap_max
      ),
      n_goalposts = list(
        gposts = if(paras$Direction == -1){
          c(paras$goalpost_upper*paras$Direction, paras$goalpost_lower*paras$Direction, paras$goalpost_scale)
        } else {
          c(paras$goalpost_lower*paras$Direction, paras$goalpost_upper*paras$Direction, paras$goalpost_scale)
        },
        trunc2posts = paras$goalpost_trunc2posts
      )
    )

    # update big list
    l_n[[ii]] <- l
  }

  l_n
}
