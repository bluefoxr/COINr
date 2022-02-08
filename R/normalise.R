#' Create normalised data set in a purse of coins
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
normalise2.purse <- function(x, dset = NULL, default_specs = NULL, indiv_specs = NULL,
                             directions = NULL){

  # input check
  check_purse(x)

  # apply unit screening to each coin
  x$coin <- lapply(x$coin, function(coin){
    treat2.coin(coin, dset = dset, default_specs = default_specs,
                indiv_specs = indiv_specs)
  })
  # make sure still purse class
  class(x) <- "purse"
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
#'
#' @return
#' @export
normalise2.coin <- function(x, dset, default_specs = NULL, indiv_specs = NULL,
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

  iData_n <- normalise2(iData_, default_specs = default_specs, indiv_specs = indiv_specs,
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
    coin <- write_dset(coin, iData_n, dset = write_to)
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
#' @return
#' @export
normalise2.data.frame <- function(x, default_specs = NULL, indiv_specs = NULL,
                               directions = NULL){

  # CHECKS ------------------------------------------------------------------

  # most input checks are performed in normalise2.numeric()

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
  specs_def <- list(f_n = "minmax",
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
        indiv_specs_col <- indiv_specs[[col_name]]
        specs <- utils::modifyList(specs_def, indiv_specs_col)
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
    do.call("normalise2.numeric", c(list(x = xi), specs))
  }

  # now run function
  # output is one list
  norm_results <- sapply(names(x), norm_col) |>
    as.data.frame()

  # CHECK and OUTPUT --------------------------------------------------------
  norm_results

}


#' Normalise indicator data sets
#'
#' Normalisation is specified using the `f_n` and `f_n_para` arguments. In these, `f_n` should be a character
#' string which is the name of a normalisation
#' function. For example, `f_n = "minmax"` calls the [minmax()] function. `f_n_para` is a list of any
#' further arguments to `f_n`. This means that any function can be passed to [normalise()], as long as its
#' first argument is `x`, a numeric vector, and it returns a numeric vector of the same length. See [minmax()]
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
normalise2.numeric <- function(x, f_n = "minmax", f_n_para = list(l_u = c(0,100)),
                               direction = 1){


  # CHECKS ------------------------------------------------------------------

  # x must be numeric to be here. f_n will be checked by do.call()

  if(direction %nin% c(-1, 1)){
    stop("direction must be either -1 or 1")
  }
  if(!is.list(f_n_para)){
    stop("f_n_para must be a list")
  }

  # NORMALISE ---------------------------------------------------------------

  # change direction
  x <- x*direction

  # call normalisation function
  xn <- do.call(what = f_n, args = c(list(x = x), f_n_para))

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
#' function. For example, `ntype = "minmax"` calls the [minmax()] function. `npara` is any
#' further argument to `ntype`. This means that any function can be passed to [normalise()], as long as it
#' is of the form `f(x, npara)`, where `x` is a vector (i.e. one column of data) and should return a
#' similar vector of normalised data. See [minmax()] for an example. If a function has more than one
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
normalise2 <- function(x, ...){
  UseMethod("normalise2")
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
#' minmax(x)
#'
#' @return Normalised vector
#'
#' @export
minmax <- function(x, l_u = c(0,100)){

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
#' does `x_scaled <- (x-l)/(u-l) * 100`. Note this is *not* the minmax transformation (see [minmax()]).
#' This is a linear transformation with shift `u` and scaling factor `u-l`.
#'
#' @param x A numeric vector
#' @param npara Parameters as a vector `c(l, u)`. See description.
#'
#' @examples
#' x <- runif(20)
#' scaled(x, npara = c(1,10))
#'
#' @return Scaled vector
#'
#' @export
scaled <- function(x, npara = c(0,100)){

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
#' zscore(x)
#'
#' @return Numeric vector
#'
#' @export
zscore <- function(x, m_sd = c(0,1)){

  stopifnot(is.numeric(x),
            is.numeric(m_sd),
            length(m_sd) == 2,
            all(!is.na(m_sd)),
            m_sd[2] > 0)

  (x-mean(x, na.rm = TRUE))/stats::sd(x, na.rm = TRUE)*m_sd[2] + m_sd[1]
}
