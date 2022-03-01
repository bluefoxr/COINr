

#' Find highly-correlated indicators within groups
#'
#' This returns a data frame of any highly correlated indicators within the same aggregation group. The level of the aggregation
#' group can be controlled by the `grouplev` argument.
#'
#' @param coin A coin class object
#' @param dset The data set to use for correlations.
#' @param hicorval A threshold to flag high correlation. Default 0.9.
#' @param grouplev The level to group indicators in. E.g. if `grouplev = 2` it will look for high correlations between indicators that
#' belong to the same group in Level 2.
#' @param cortype The type of correlation, either `"pearson"` (default), `"spearman"` or `"kendall"`. See [stats::cor].
#' @param roundto Number of decimal places to round correlations to. Default 3. Set `NULL` to disable rounding.
#'
#' @examples
#' #
#'
#' @seealso
#' * [rew8r()] Interactive app for adjusting weights and seeing effects on correlations
#' * [getCorr()] Get correlations between indicators/levels
#'
#' @return A data frame with one entry for every indicator pair that is highly correlated within the same group, at the specified level.
#' Pairs are only reported once, i.e. only uses the upper triangle of the correlation matrix.
#'
#' @export
get_hicorr <- function(coin, dset, hicorval = 0.9, cortype = "pearson",
                     grouplev = NULL, roundto = 3){


  # CHECKS AND DEFAULTS -----------------------------------------------------

  grouplev <- set_default(grouplev, 2)

  if(grouplev > coin$Meta$maxlev){
    stop("grouplev is greater than the maximum level of ", coin$Meta$maxlev)
  }

  stopifnot(hicorval <= 1,
            hicorval >= 0)

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

  # now filter to only high correlations
  crtable <- crtable[abs(crtable$Value) > hicorval, ]


  # CLEAN AND OUTPUT --------------------------------------------------------

  # col names
  colnames(crtable) <- c("Ind1", "Ind2", "Corr", "Group", "P2")

  if(!is.null(roundto)){
    crtable$Corr <- round(crtable$Corr, roundto)
  }

  crtable[c("Group", "Ind1", "Ind2", "Corr")]

}


#' Recalculate correlations and ranks based on new weights
#'
#' This is a short cut function which takes a new set of indicator weights, and recalculates the coin results
#' based on these weights. It returns a summary of rankings and the correlations between indicators and index.
#'
#' This function is principally used inside [rew8r()]. The `w` argument should be a data frame of weights, of the same format
#' as the data frames found in `.$Parameters$Weights`.
#'
#' @param coin coin object
#' @param w Full data frame of weights for each level
#' @param Levels A 2-length vector with two aggregation levels to correlate against each other
#' @param iCodes List of two character vectors of indicator codes, corresponding to the two aggregation levels
#' @param cortype Correlation type. Either `"pearson"` (default), `"kendall"` or `"spearman"`. See [stats::cor].
#' @param withparent Logical: if `TRUE`, only correlates with the parent, e.g. sub-pillars are only correlated with their parent pillars and not others.
#'
#' @importFrom reshape2 melt
#' @importFrom dplyr inner_join
#'
#' @return A list where `.$cr` is a vector of correlations between each indicator and the index, and
#' `.$dat` is a data frame of rankings, with unit code, and index, input and output scores
#'
#' @examples
#' # build ASEM coin up to aggregation
#' ASEM <- build_ASEM()
#' # get correlations between pillars (level 2) and index (level 4)
#' # original weights used just for demonstration, normally you would alter first.
#' l <- weights2corr(ASEM, ASEM$Parameters$Weights$Original, Levels = c(2,4))
#'
#' @seealso
#' * [rew8r()] Interactive app for adjusting weights and seeing effects on correlations
#' * [getCorr()] Get correlations between indicators/levels
#'
#' @export

weights2corr <- function(coin, w, Levels = NULL, iCodes = NULL,
                         cortype = "pearson", withparent = TRUE){

  if(is.null(Levels)){
    Levels <- c(1, coin$Parameters$Nlevels)
  }

  if(is.null(coin$Method$aggregate)){
    stop("You have not yet aggregated your data. This needs to be done first.")
  }

  # aggregate
  coin2 <- aggregate(coin, agtype = coin$Method$aggregate$agtype,
                     agweights = w,
                     dset = coin$Method$aggregate$dset,
                     agtype_bylevel = coin$Method$aggregate$agtype_bylevel,
                     agfunc = coin$Method$aggregate$agfunc
  )

  # get data to correlate against each other
  out1 <- getIn(coin2, dset = "Aggregated", iCodes = iCodes[[1]], aglev = Levels[1])
  idata1 <- out1$ind_data_only
  idata2 <- getIn(coin2, dset = "Aggregated", iCodes = iCodes[[2]], aglev = Levels[2])$ind_data_only
  # table of results data
  dfres <- coin2$Data$Aggregated[c("UnitName",
                                   coin$Parameters$AggCodes[[length(coin$Parameters$AggCodes)]],
                                   coin$Parameters$AggCodes[[length(coin$Parameters$AggCodes)-1]])]
  dfres <- cbind("Rank"=rank(coin2$Data$Aggregated$Index*-1, ties.method = "min"), dfres)
  # get correlations
  cr = stats::cor(idata1, idata2, method = cortype, use = "pairwise.complete.obs")

  # get index structure
  agcols <- dplyr::select(coin$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg"))

  # select cols corresponding to what is being correlated against what
  agcols <- agcols[Levels]
  # change correlation to long form
  # WHY because this makes a nice table also with names and what is correlated with what
  crlong <- suppressMessages(reshape2::melt(cr))
  colnames(crlong) <- c(colnames(agcols), "Correlation")

  # only correlate with parents, if asked. This is necessary if using inside the rew8r app because
  # we need a vector output. Also, it is the most relevant information.
  if (withparent & ncol(cr)>1){

    # now do inner join - we are matching correlation rows that agree with the structure of the index
    crtable <- dplyr::inner_join(agcols, crlong, by = colnames(agcols))

  } else {
    crtable <- crlong
  }
  # sometimes this throws duplicates, so remove
  crtable <- unique(crtable)

  # now we want the correlations...
  out <- list(cr = crtable,
              dat = dfres,
              iCodes1 = out1$ind_names)

  return(out)

}


#' Get different types of correlation matrices
#'
#' Helper function for getting correlations between indicators. This retrieves subsets of correlation
#' matrices between different aggregation levels, in different formats.
#'
#' Note that this function can only call correlations within the same data set (i.e. only one data set in `.$Data`).
#'
#' @param coin The coin object
#' @param dset The target data set
#' @param iCodes An optional list of character vectors where the first entry specifies the indicator/aggregate
#' codes to correlate against the second entry (also a specification of indicator/aggregate codes).
#' @param Levels The aggregation levels to take the two groups of indicators from. See [getIn()] for details. Defaults to indicator level.
#' @param cortype The type of correlation to calculate, either `"pearson"`, `"spearman"`, or `"kendall"`.
#' @param withparent If `TRUE`, and `aglev[1] != aglev[2]`, will only return correlations of each row with its parent.
#' @param grouplev The aggregation level to group correlations by if `aglev[1] == aglev[2]`. By default, groups correlations into the
#' aggregation level above. Set to 0 to disable grouping and return the full matrix.
#' @param pval The significance level for including correlations. Correlations with \eqn{p > pval} will be returned as `NA`.
#' Default 0.05. Set to 0 to disable this.
#'
#' @importFrom stats cor
#'
#' @examples
#' # build ASEM coin
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # correlations of indicators in Political pillar
#' corrs <- getCorr(ASEM, dset = "Raw", iCodes = "Political", Levels = 1)
#'
#' @return A data frame of correlation values in long format. Correlations with \eqn{p > pval} will be returned as `NA`.
#'
#' @seealso
#' * [plotCorr()] Plot correlation matrices of indicator subsets
#'
#' @export

get_corr <- function(coin, dset, iCodes = NULL, Levels = NULL, uCodes = NULL, use_group = NULL,
                     cortype = "pearson", pval = 0.05, withparent = TRUE, grouplev = NULL){


  # CHECKS ------------------------------------------------------------------

  check_coin_input(coin)

  # DEFAULTS ----------------------------------------------------------------

  # set Levels, repeat iCodes etc if only one input
  if(is.null(Levels)){Levels = 1}
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

  # GET DATA ----------------------------------------------------------------

  # get data sets
  iData1 <- get_data(coin, dset = dset, iCodes = iCodes[[1]],
                     Level = Levels[[1]], uCodes = uCodes, use_group = use_group, also_get = "none")
  iData2 <- get_data(coin, dset = dset, iCodes = iCodes[[2]],
                     Level = Levels[[2]], uCodes = uCodes, use_group = use_group, also_get = "none")


  # GET CORRELATIONS --------------------------------------------------------

  # get corr matrix
  crmat <- stats::cor(iData1, iData2,
                      use = "pairwise.complete.obs", method = cortype)
  crmat <- round(crmat,2)

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

  # first, if grouplev not specified, group by level above

  if(is.null(grouplev)) grouplev <- Levels[1] + 1

  if ((grouplev > 0) & (Levels[1] == Levels[2])){

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

    # now do inner join - we are matching correlation rows that agree with the structure of the index
    crtable <- merge(keeprows, crmat_melt, by = colnames(keeprows))
    # sometimes this throws duplicates, so remove
    crtable <- unique(crtable)

  }

  crtable

}



#' P-values for correlations in a data frame
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
#' of "consistency" of a data set, where a high value implies higher reliability/consistency.
#'
#' This function simply returns Cronbach's alpha. If you want a lot more details on reliability, the 'psych' package has
#' a much more detailed analysis.
#'
#' @param COIN A COIN or a data frame containing only numerical columns of data.
#' @param dset The data set to check the consistency of.
#' @param icodes Indicator codes if a subset of `dset` is requested
#' @param aglev The aggregation level to take `icodes` from (see [getIn()] for details)
#' @param use Argument to pass to [stats::cor] to calculate the covariance matrix. Default `"pairwise.complete.obs"`.
#'
#' @importFrom stats cov
#'
#' @examples
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # get Cronbach of indicators in Physical pillar
#' getCronbach(ASEM, dset = "Raw", icodes = "Physical", aglev = 1)
#'
#' @return Cronbach alpha as a numerical value.
#'
#' @export

getCronbach <- function(COIN, dset = "Raw", icodes = NULL,
                        aglev = NULL, use = "pairwise.complete.obs"){

  if(is.null(aglev)){aglev = 1}

  # get data
  df <- getIn(COIN, dset = dset, icodes = icodes, aglev = aglev)$ind_data_only

  # get number of variables
  k = ncol(df)

  # get covariance matrix
  cvtrix <- stats::cov(df, use = use)

  # sum of all elements of cov matrix
  sigall <- sum(cvtrix, na.rm = TRUE)

  # mean of all elements except diagonal
  sigav <- (sigall - sum(diag(cvtrix), na.rm = TRUE))/(k*(k-1))

  # calculate Cronbach alpha
  cron <- (k^2 * sigav)/sigall
  return(cron)

}
