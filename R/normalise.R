#' Create normalised data set in a purse of coins
#'
#' Normalisation is a special case in terms of operating on a purse of coins. This is because, when
#' dealing with time series data, it is often desirable to normalise over the whole panel data set
#' rather than independently for each time point. This makes the resulting index and aggregates comparable
#' over time.
#'
#' The same specifications are passed to each coin in the purse. This means that each coin is normalised
#' using the same set of specifications and directions. If you need control over individual coins, you
#' will have to normalise coins individually.
#'
#' @param x A purse object
#' @param dset The data set to normalise in each coin
#' @param default_specs Default specifications
#' @param indiv_specs Individual specifications
#' @param directions If `NULL`, extracts directions from indicator metadata, i.e. the `iMeta` data frame
#' that was passed to [new_coin()]. Else `directions` should be a vector with entries either -1 or 1, in
#' order of the columns of the data set.
#'
#' @return An updated purse with new normalised data sets added at `.$Data$Normalised` in each coin
#' @export
#'
#' @examples
#' #
Normalise.purse <- function(x, dset = NULL, default_specs = NULL, indiv_specs = NULL,
                             directions = NULL, global = TRUE, write_to = NULL){

  # input check
  check_purse(x)

  # GET DSETS ---------------------------------------------------------------

  iDatas <- get_dset(x, dset)
  iDatas_ <- iDatas[names(iDatas) != "Time"]


  # GLOBAL NORMALISATION ----------------------------------------------------

  if(global){

    # run global dset through normalise, excluding Time col
    iDatas_n <- Normalise(iDatas_, default_specs = default_specs,
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
      # write dset first
      coin <- write_dset(coin, iDatas_n_l[[which(names(iDatas_n_l) == tt)]], dset = "Normalised")

      # also write to log - we signal that coin can't be regenerated any more
      coin$Log$can_regen <- FALSE
      coin$Log$message <- "Coin was normalised inside a purse with global = TRUE. Cannot be regenerated."

      coin
    })

  } else {

    # apply independent normalisation to each coin
    x$coin <- lapply(x$coin, function(coin){
      Normalise.coin(coin, dset = dset, default_specs = default_specs,
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
#' @param x A coin
#' @param dset A named data set found in `.$Data`
#' @param default_specs Specifications to apply to all columns, apart from those specified by `indiv_specs`.
#' @param indiv_specs Specifications applied to specific columns, overriding those specified in `default_specs`
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data set
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will be taken from the `iMeta` table in the coin, if available.
#' @param out2 Either `"coin"` to return normalised data set back to the coin, or `df` to simply return a data
#' frame.
#' @param write_to Optional character string for naming the data set in the coin. Data will be written to
#' `.$Data[[write_to]]`. Default is `write_to == "Normalised"`.
#'
#' @return
#' @export
Normalise.coin <- function(x, dset, default_specs = NULL, indiv_specs = NULL,
                                  directions = NULL, out2 = "coin", write_to = NULL){

  # WRITE LOG ---------------------------------------------------------------

  coin <- write_log(x, dont_write = "x")

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
  }

  # NORMALISE DATA ----------------------------------------------------------

  iData_n <- Normalise(iData_, default_specs = default_specs, indiv_specs = indiv_specs,
                        directions = directions)
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
#' @param x A data frame
#' @param default_specs Specifications to apply to all columns, apart from those specified by `indiv_specs`.
#' @param indiv_specs Specifications applied to specific columns, overriding those specified in `default_specs`
#' @param directions An optional data frame containing the following columns:
#' * `iCode` The indicator code, corresponding to the column names of the data frame
#' * `Direction` numeric vector with entries either `-1` or `1`
#' If `directions` is not specified, the directions will all be assigned as `1`. Non-numeric columns do not need
#' to have directions assigned.
#'
#' @examples
#' iris_norm <- Normalise(iris)
#' head(iris_norm)
#'
#' @return
#' @export
Normalise.data.frame <- function(x, default_specs = NULL, indiv_specs = NULL,
                               directions = NULL){

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
  if(!is.null(default_specs)){
    stopifnot(is.list(default_specs))
    #specs_def <- utils::modifyList(specs_def, default_specs)
    specs_def <- default_specs
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
    if(is.null(specs$direction)){
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


#' Normalise indicator data sets
#'
#' Normalisation is specified using the `f_n` and `f_n_para` arguments. In these, `f_n` should be a character
#' string which is the name of a normalisation
#' function. For example, `f_n = "n_minmax"` calls the [n_minmax()] function. `f_n_para` is a list of any
#' further arguments to `f_n`. This means that any function can be passed to [normalise()], as long as its
#' first argument is `x`, a numeric vector, and it returns a numeric vector of the same length. See [n_minmax()]
#' for an example.
#'
#' `f_n_para` is *required* to be a named list. So e.g. if we define a function `f1(x, arg1, arg2)` then we should
#' specify `f_n = "f1"`, and `f_n_para = list(arg1 = val1, arg2 = val2)`, where `val1` and `val2` are the
#' values assigned to the arguments.
#'
#' @param x Object to be normalised
#' @param f_n The normalisation method, specified as string which refers to a function of the form `f_n(x, npara)`.
#' See details.
#' @param f_n_para Supporting list of arguments for `f_n`. This is required to be a list.
#' @param direction If `direction = -1` the highest values of `x` will correspond to the lowest
#' values of the normalised `x`. Else if `direction = 1` the direction of `x` in unaltered.
#'
#' @examples
#' #
#'
#' @return An updated GII with normalised data set added
#'
#' @export
Normalise.numeric <- function(x, f_n = NULL, f_n_para = NULL,
                               direction = 1){


  # CHECKS ------------------------------------------------------------------

  # x must be numeric to be here. f_n will be checked by do.call()

  if(direction %nin% c(-1, 1)){
    stop("direction must be either -1 or 1")
  }

  # DEFAULTS ----------------------------------------------------------------

  # minmax is default
  if(is.null(f_n)){
    f_n <- "minmax"
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

  # change direction
  x <- x*direction

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


#' Normalise indicator data sets
#'
#' Normalisation is specified using the `ntype` and `npara` arguments. In these, `ntype` should be a character
#' string which is the name of a normalisation
#' function. For example, `ntype = "n_minmax"` calls the [n_minmax()] function. `npara` is any
#' further argument to `ntype`. This means that any function can be passed to [normalise()], as long as it
#' is of the form `f(x, npara)`, where `x` is a vector (i.e. one column of data) and should return a
#' similar vector of normalised data. See [n_minmax()] for an example. If a function has more than one
#' additional argument, then `npara` can be passed as a list and a wrapper could be created.
#'
#' @param x Object to be normalised
#' @param ... Further arguments to be passed to methods.
#'
#' @examples
#' #
#'
#' @return An updated GII with normalised data set added
#'
#' @export
Normalise <- function(x, ...){
  UseMethod("Normalise")
}

#' Minmax a vector
#'
#' Scales a vector using min-max
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
#' $ 1 - (x_{max} - x)/(x_{max} - x_{min}) $
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
#' $ 1 - (x_{ref} - x)/(x_{ref} - x_{min}) $
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
#' n_dist2ref(x)
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
#' A measure of the distance of each value of `x` to a specified target. The formula is:
#'
#' $ 1 - (x_{targ} - x)/(x_{targ} - x_{min}) $
#'
#' Values exceeding `x_targ` can be optionally capped at 1 if `cap_max = TRUE`.
#'
#' @param x A numeric vector
#' @param targ An target value
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
n_dist2targ <- function(x, targ, cap_max = FALSE){

  stopifnot(is.numeric(x),
            is.numeric(targ),
            length(targ)==1,
            is.logical(cap_max))

  if(is.na(targ)){
    stop("targ is NA")
  }

  minx <- min(x, na.rm = TRUE)
  if(targ < minx){
    warning("targ is less than min(x) - this will produce negative scores.")
  }

  y <- 1 - (targ - x)/(targ - minx)
  if(cap_max){
    y[y>1] <- 1
  }

  y
}


#' Normalise as fraction of max value
#'
#' The ratio of each value of `x` to `max(x)`.
#'
#' $ x / x_{max} $
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
#' Calculates Borda scores as rank(x) - 1.
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
#' The distance of each value of `x` from the lower "goalpost" to the upper one. Goalposts are specified by
#' `gposts = c(l, u, a)`, where `l` is the lower bound, `u` is the upper bound, and `a` is a scaling parameter.
#'
#' Specify `direction = -1` to "flip" the goalposts. This may be necessary depending on how the goalposts
#' were defined.
#'
#' @param x A numeric vector
#' @param gposts A numeric vector `c(l, u, a)`, where `l` is the lower bound, `u` is the upper bound,
#' and `a` is a scaling parameter.
#' @param direction Either 1 or -1. Set to -1 to flip goalposts.
#' @param trunc2posts If `TRUE` (default) will truncate any values that fall outside of the goalposts.
#'
#' @examples
#' x <- runif(20)
#' n_goalposts(x, gposts = c(0.2, 0.8, 1))
#'
#' @return Numeric vector
#'
#' @export
n_goalposts <- function(x, gposts, direction = 1, trunc2posts = TRUE){

  stopifnot(is.numeric(x))

  # since indicators arrive with directions possibly reversed (*-1), we have to also multiply GPs by -1
  if(direction == -1){
    # here, indicators are multiplied by -1, so need to also multiply goalposts by -1
    gposts <- -1*gposts
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


