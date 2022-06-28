
#' Correlations between indicators and denominators
#'
#' Get a data frame containing any correlations between indicators and denominators that exceed a given
#' threshold. This can be useful when *whether* to denominate an indicator and *by what* may not be obvious.
#' If an indicator is strongly correlated with a denominator, this may suggest to denominate it by that
#' denominator.
#'
#' @param coin A coin class object.
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param cor_thresh A correlation threshold: the absolute value of any correlations between indicator-denominator pairs above this
#' threshold will be flagged.
#' @param cortype The type of correlation: to be passed to the `method` argument of `stats::cor`.
#' @param nround Optional number of decimal places to round correlation values to. Default 2, set `NULL` to
#' disable.
#'
#' @return A data frame of pairwise correlations that exceed the threshold.
#' @export
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get correlations >0.7 of any indicator with denominators
#' get_denom_corr(coin, dset = "Raw", cor_thresh = 0.7)
#'
get_denom_corr <- function(coin, dset, cor_thresh = 0.6, cortype = "pearson", nround = 2){

  # indicator data
  # get everything at this point to ensure matching rows
  iData <- get_dset(coin, dset = dset, also_get = "all")

  # iMeta
  iMeta <- coin$Meta$Ind

  # only the indicator data
  iData_ <- iData[iMeta$iCode[iMeta$Type == "Indicator"]]

  # only the denoms
  den_codes <- iMeta$iCode[iMeta$Type == "Denominator"]
  if(length(den_codes) == 0){
    stop("No denominators found. Denominators should be labelled as 'Denominator' in iMeta.")
  }
  if(any(den_codes %nin% names(iData))){
    stop("Denominators not found - they are present in iMeta but not found in selected data set.")
  }
  denoms <- iData[den_codes]

  # GET CORRS ---------------------------------------------------------------

  # get correlations
  corr_ind <- stats::cor(iData_, denoms, method = cortype, use = "pairwise.complete.obs")

  # make long
  crtable <- lengthen(corr_ind)


  # FIND HI CORRS -----------------------------------------------------------

  # remove self-correlations
  crtable <- crtable[crtable$V1 != crtable$V2, ]
  # remove NAs
  crtable <- crtable[!is.na(crtable$Value), ]

  # now filter to only high or low correlations
  crtable <- crtable[abs(crtable$Value) > cor_thresh, ]

  # CLEAN AND OUTPUT --------------------------------------------------------

  # col names
  colnames(crtable) <- c("Ind", "Denom", "Corr")

  # round
  if(!is.null(nround)){
    crtable$Corr <- round(crtable$Corr, nround)
  }

  # sort
  crtable <- crtable[order(crtable$Ind),]

  crtable
}


#' Find highly-correlated indicators within groups
#'
#' This returns a data frame of any highly correlated indicators within the same aggregation group. The level of the aggregation
#' grouping can be controlled by the `grouplev` argument.
#'
#' This function is motivated by the idea that having very highly-correlated indicators within the same group may
#' amount to double counting, or possibly redundancy in the framework.
#'
#' @param coin A coin class object
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param cor_thresh A threshold to flag high correlation. Default 0.9.
#' @param grouplev The level to group indicators in. E.g. if `grouplev = 2` it will look for high correlations between indicators that
#' belong to the same group in Level 2.
#' @param cortype The type of correlation, either `"pearson"` (default), `"spearman"` or `"kendall"`. See [stats::cor].
#' @param roundto Number of decimal places to round correlations to. Default 3. Set `NULL` to disable rounding.
#' @param thresh_type Either `"high"`, which will only flag correlations *above* `cor_thresh`, or `"low"`,
#' which will only flag correlations *below* `cor_thresh`.
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)
#'
#' # get correlations between indicator over 0.75 within level 2 groups
#' get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75,
#'                thresh_type = "high", grouplev = 2)
#'
#' @return A data frame with one entry for every indicator pair that is highly correlated within the same group, at the specified level.
#' Pairs are only reported once, i.e. only uses the upper triangle of the correlation matrix.
#'
#' @export
get_corr_flags <- function(coin, dset, cor_thresh = 0.9, thresh_type = "high", cortype = "pearson",
                     grouplev = NULL, roundto = 3){


  # CHECKS AND DEFAULTS -----------------------------------------------------

  grouplev <- set_default(grouplev, 2)

  stopifnot(thresh_type %in% c("high", "low"))

  if(grouplev > coin$Meta$maxlev){
    stop("grouplev is greater than the maximum level of ", coin$Meta$maxlev)
  }

  stopifnot(cor_thresh <= 1,
            cor_thresh >= -1)

  iData <- get_dset(coin, dset, also_get = "none")


  # GET CORRS ---------------------------------------------------------------

  # get correlations
  corr_ind <- stats::cor(iData, method = cortype, use = "pairwise.complete.obs")

  # make long
  crmat_melt <- lengthen(corr_ind)


  # FIND HI CORRS -----------------------------------------------------------

  # get index structure
  lin <- coin$Meta$Lineage
  # select cols corresponding to what is being correlated against what
  lin <- unique(lin[c(1,grouplev)])

  # get parent group of each of V1 and V2
  crtable <- merge(crmat_melt, lin, by.x = "V1", by.y = colnames(lin)[1])
  colnames(crtable)[ncol(crtable)] <- "P1"
  crtable <- merge(crtable, lin, by.x = "V2", by.y = colnames(lin)[1])
  colnames(crtable)[ncol(crtable)] <- "P2"

  # filter to only include entries from the same group
  crtable <- crtable[crtable$P1 == crtable$P2 ,]
  # remove self-correlations
  crtable <- crtable[crtable$V1 != crtable$V2, ]
  # remove NAs
  crtable <- crtable[!is.na(crtable$Value), ]

  # now filter to only high or low correlations
  if(thresh_type == "high"){
    crtable <- crtable[crtable$Value > cor_thresh, ]
  } else {
    crtable <- crtable[crtable$Value < cor_thresh, ]
  }

  # CLEAN AND OUTPUT --------------------------------------------------------

  # col names
  colnames(crtable) <- c("Ind1", "Ind2", "Corr", "Group", "P2")

  if(!is.null(roundto)){
    crtable$Corr <- round(crtable$Corr, roundto)
  }

  crtable[c("Group", "Ind1", "Ind2", "Corr")]

}


#' Get correlations
#'
#' Helper function for getting correlations between indicators and aggregates. This retrieves subsets of correlation
#' matrices between different aggregation levels, in different formats. By default, it will return a
#' long-form data frame, unless `make_long = FALSE`.
#'
#' This function allows you to obtain correlations between any subset of indicators or aggregates, from
#' any data set present in a coin. Indicator selection is performed using [get_data()]. Two different
#' indicator sets can be correlated against each other by specifying `iCodes` and `Levels` as vectors.
#'
#' The correlation type can be specified by the `cortype` argument, which is passed to [stats::cor()].
#'
#' The `withparent` argument will optionally only return correlations which correspond to the structure
#' of the index. For example, if `Levels = c(1,2)` (i.e. we wish to correlate indicators from Level 1 with
#' aggregates from Level 2), and we set `withparent = TRUE`, only the correlations between each indicator
#' and its parent group will be returned (not correlations between indicators and other aggregates to which
#' it does not belong). This can be useful to check whether correlations of an indicator/aggregate with
#' any of its parent groups exceeds or falls below thresholds.
#'
#' Similarly, the `grouplev` argument can be used to restrict correlations to within groups corresponding
#' to the index structure. Setting e.g. `grouplev = 2` will only return correlations within the groups
#' defined at Level 2.
#'
#' The `grouplev` and `withparent` options are disabled if `make_long = FALSE`.
#'
#' Note that this function can only call correlations within the same data set (i.e. only one data set in `.$Data`).
#'
#' @param coin A coin class coin object
#' @param dset  The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param iCodes An optional list of character vectors where the first entry specifies the indicator/aggregate
#' codes to correlate against the second entry (also a specification of indicator/aggregate codes).
#' @param Levels The aggregation levels to take the two groups of indicators from. See [get_data()] for details.
#' Defaults to indicator level.
#' @param ... Further arguments to be passed to [get_data()] (`uCodes` and `use_group`).
#' @param cortype The type of correlation to calculate, either `"pearson"`, `"spearman"`, or `"kendall"`.
#' @param withparent If `TRUE`, and `aglev[1] != aglev[2]`, will only return correlations of each row with its parent.
#' @param grouplev The aggregation level to group correlations by if `aglev[1] == aglev[2]`. Requires that
#' `make_long = TRUE`.
#' @param pval The significance level for including correlations. Correlations with \eqn{p > pval} will be returned as `NA`.
#' Default 0.05. Set to 0 to disable this.
#' @param make_long Logical: if `TRUE`, returns correlations in long format (default), else if `FALSE`
#' returns in wide format. Note that if wide format is requested, features specified by `grouplev`
#' and `withparent` are not supported.
#'
#' @importFrom stats cor
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get correlations
#' cmat <- get_corr(coin, dset = "Raw", iCodes = list("Environ"),
#'                  Levels = 1, make_long = FALSE)
#'
#' @return A data frame of pairwise correlation values in wide or long format (see `make_long`).
#' Correlations with \eqn{p > pval} will be returned as `NA`.
#'
#' @seealso
#' * [plot_corr()] Plot correlation matrices of indicator subsets
#'
#' @export
get_corr <- function(coin, dset, iCodes = NULL, Levels = NULL, ...,
                     cortype = "pearson", pval = 0.05, withparent = FALSE,
                     grouplev = NULL, make_long = TRUE){

  # CHECKS ------------------------------------------------------------------

  check_coin_input(coin)

  # DEFAULTS ----------------------------------------------------------------

  # set Levels, repeat iCodes etc if only one input
  if(is.null(Levels)){Levels <- 1}
  if(is.null(iCodes)){iCodes <- list(NULL)}
  stopifnot(is.list(iCodes))

  if (length(iCodes) == 1){
    iCodes = rep(iCodes, 2)
  }
  if (length(Levels) == 1){
    Levels = rep(Levels, 2)
  }
  if (Levels[2] > Levels [1]){
    Levels <- rev(Levels)
    iCodes <- rev(iCodes)
  }

  # GET FAM (RECURSIVE) -----------------------------------------------------

  if(!is.logical(withparent)){
    stopifnot(is.character(withparent),
              length(withparent)==1)
    if(withparent != "family"){
      stop("withparent not recognised - should be either logical or 'family'.")
    }

    lin <- coin$Meta$Lineage

    if(ncol(lin) <= Levels[1]){
      stop("If withparent = 'family', Levels[1] cannot be the top level.")
    }

    par_levs <- (Levels[1] + 1) : ncol(lin)

    cr_fam <- lapply(par_levs, function(lev){
      cmat <- get_corr(coin, dset = dset, iCodes = iCodes, Levels = c(Levels[1], lev),
               cortype = cortype, pval = pval, withparent = TRUE, grouplev = grouplev,
               make_long = TRUE, ... = ...)
      # rename to level
      cmat[1] <- names(lin)[lev]
      cmat
    })

    cr_fam <- Reduce(rbind, cr_fam)

    return(cr_fam)
  }

  # GET DATA ----------------------------------------------------------------

  # get data sets
  iData1 <- get_data(coin, dset = dset, iCodes = iCodes[[1]],
                     Level = Levels[[1]], ..., also_get = "none")
  iData2 <- get_data(coin, dset = dset, iCodes = iCodes[[2]],
                     Level = Levels[[2]], ..., also_get = "none")

  # GET CORRELATIONS --------------------------------------------------------

  # get corr matrix
  crmat <- stats::cor(iData1, iData2,
                      use = "pairwise.complete.obs", method = cortype)

  # set insignificant correlations to zero if requested
  if(pval > 0){
    # p values
    p_ind <- get_pvals(cbind(iData1, iData2), method = cortype)
    # relevant part of the matrix
    p_ind2 <- p_ind[1:ncol(iData1), ((ncol(iData1)+1):ncol(p_ind))]
    # set elements of crmat to zero, where correlations are below significance level
    # when plotted, these will be white, so invisible
    crmat[p_ind2 > pval] <- NA
  }

  crmat_melt <- lengthen(crmat)
  # remove rows with NAs
  #crmat_melt <- crmat_melt[!is.na(crmat_melt$value),]

  #- PARENTS -------------------------------------

  if (withparent & (ncol(crmat)>1) & Levels[1]!=Levels[2]){

    # get index structure
    lin <- coin$Meta$Lineage
    # select cols corresponding to what is being correlated against what
    lin <- unique(lin[Levels])
    # rename corr matrix cols to prepare for join
    colnames(crmat_melt) <- c(colnames(lin), "Correlation")

    # now merge - we are matching correlation rows that agree with the structure of the index
    # essentially, we subset the rows of crmat_melt to only include the ones that agree with lin
    crtable <- merge(lin, crmat_melt, by = colnames(lin))

    # rename cols for plotting
    colnames(crtable)[1:2] <- c("Var1", "Var2")

  } else {

    crtable <- crmat_melt
    colnames(crtable) <- c("Var1", "Var2", "Correlation")

  }

  ##- GROUP ----------------------------------------
  # If correlating against the same level, only show within groups if asked

  if(!is.null(grouplev)){

    if(!make_long){
      warning("Grouping is not supported for make_long = FALSE. Set make_long = TRUE to group.")
    } else {

      if (Levels[1] == Levels[2]){

        # get index structure
        lin <- coin$Meta$Lineage

        if(grouplev <= Levels[1]){
          stop("grouplev must be at least the aggregation level above Levels.")
        }

        if(grouplev > ncol(lin)){
          stop("Grouping level is out of range - should be between 2 and ", ncol(lin), " or zero to turn off.")
        }

        # select cols corresponding to current level, plus the one above
        # remember here we are correlating a level against itself, so Levels[1]==Levels[2]
        lin <- lin[c(Levels[1], grouplev)]
        # get unique groups in level above
        lev2 <- unique(lin[[2]])

        # initialise df for recording entries of corr matrix to keep
        keeprows <- data.frame(Var1 = NA, Var2 = NA)

        # loop over the levels above
        for (lev2ii in lev2){
          # get child codes
          lev1 <- lin[lin[2]==lev2ii, 1]
          lev1 <- unique(unlist(lev1, use.names = FALSE)) # otherwise it is still a tibble, also remove dups
          # get all 2-way combos of these codes
          lev1pairs <- expand.grid(lev1, lev1)
          # add to df
          keeprows <- rbind(keeprows, lev1pairs)
        }
        # remove first row (dummy)
        keeprows <- keeprows[-1,]

        # rename corr matrix cols to prepare for join
        colnames(crmat_melt)[3] <- "Correlation"
        colnames(keeprows) <- colnames(crmat_melt)[1:2]

        # now do inner join - we are matching correlation rows that agree with the structure of the index
        crtable <- merge(keeprows, crmat_melt, by = colnames(keeprows))
        # sometimes this throws duplicates, so remove
        crtable <- unique(crtable)

      }

    }
  }

  colnames(crtable) <- c("Var1", "Var2", "Correlation")

  # widen or not
  if(!make_long){
    crtable <- widen(crtable)
  }

  crtable
}



#' P-values for correlations in a data frame or matrix
#'
#' This is a stripped down version of the "cor.mtest()" function from the "corrplot" package. It uses
#' the [stats::cor.test()] function to calculate pairwise p-values. Unlike the corrplot version, this
#' only calculates p-values, and not confidence intervals. Credit to corrplot for this code, I only
#' replicate it here to avoid depending on their package for a single function.
#'
#' @param X A numeric matrix or data frame
#' @param \dots Additional arguments passed to function [cor.test()], e.g. \code{conf.level = 0.95}.
#'
#' @importFrom stats cor.test
#'
#' @examples
#' # a matrix of random numbers, 3 cols
#' x <- matrix(runif(30), 10, 3)
#'
#' # get correlations between cols
#' cor(x)
#'
#' # get p values of correlations between cols
#' get_pvals(x)
#'
#' @return Matrix of p-values
#' @export
get_pvals = function(X, ...) {

  # convert to matrix, get number cols
  X = as.matrix(X)
  n = ncol(X)

  # prep matrix for p values
  p.X <- matrix(NA, n, n)
  diag(p.X) = 0

  # populate matrix
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {

      # get p val for pair
      tmp = stats::cor.test(x = X[, i], y = X[, j], ...)
      p.X[i, j] = p.X[j, i] = tmp$p.value

    }
  }

  # rename cols
  colnames(p.X) = rownames(p.X) = colnames(X)

  # output
  p.X
}


#' Cronbach's alpha
#'
#' Calculates Cronbach's alpha, a measure of statistical reliability. Cronbach's alpha is a simple measure
#' of "consistency" of a data set, where a high value implies higher reliability/consistency. The
#' selection of indicators via [get_data()] allows to calculate the measure on any group of
#' indicators or aggregates.
#'
#' This function simply returns Cronbach's alpha. If you want a lot more details on reliability, the 'psych' package has
#' a much more detailed analysis.
#'
#' @param coin A coin or a data frame containing only numerical columns of data.
#' @param ... Further arguments passed to [get_data()], other than those explicitly specified here.
#' @param use Argument to pass to [stats::cor] to calculate the covariance matrix. Default `"pairwise.complete.obs"`.
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param iCodes Indicator codes to retrieve. If `NULL` (default), returns all iCodes found in
#' the selected data set. See [get_data()].
#' @param Level The level in the hierarchy to extract data from. See [get_data()].
#'
#' @importFrom stats cov
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # Cronbach's alpha for the "P2P" group
#' get_cronbach(coin, dset = "Raw", iCodes = "P2P", Level = 1)
#'
#' @return Cronbach alpha as a numerical value.
#'
#' @export
get_cronbach <- function(coin, dset, iCodes, Level, ..., use = "pairwise.complete.obs"){

  # get data
  iData <- get_data(coin, dset = dset, iCodes = iCodes, Level = Level, ...)

  # get only indicator cols
  iData <- extract_iData(coin, iData, "iData_")

  # get number of variables
  k = ncol(iData)

  # get covariance matrix
  cvtrix <- stats::cov(iData, use = use)

  # sum of all elements of cov matrix
  sigall <- sum(cvtrix, na.rm = TRUE)

  # mean of all elements except diagonal
  sigav <- (sigall - sum(diag(cvtrix), na.rm = TRUE))/(k*(k-1))

  # calculate Cronbach alpha
  (k^2 * sigav)/sigall

}
