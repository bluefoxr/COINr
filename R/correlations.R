

#' Highly correlated indicators in the same aggregation group
#'
#' This returns a data frame of any highly correlated indicators within the same aggregation group. The level of the aggregation
#' group can be controlled by the `grouplev` argument.
#'
#' @param COIN Data frame with first col indicator codes, second is weights, third is correlations
#' @param dset The data set to use for correlations.
#' @param hicorval A threshold to flag high correlation. Default 0.9.
#' @param grouplev The level to group indicators in. E.g. if `grouplev = 2` it will look for high correlations between indicators that
#' belong to the same group in Level 2.
#' @param cortype The type of correlation, either `"pearson"` (default), `"spearman"` or `"kendall"`. See [stats::cor].
#'
#' @importFrom reshape2 melt
#'
#' @examples
#' # Assemble ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # check for any within-pillar correlations of > 0.7
#' hicorrSP(ASEM, dset = "Raw", hicorval = 0.7, , grouplev = 2)
#'
#' @seealso
#' * [rew8r()] Interactive app for adjusting weights and seeing effects on correlations
#' * [getCorr()] Get correlations between indicators/levels
#'
#' @return A data frame with one entry for every indicator pair that is highly correlated within the same group, at the specified level.
#' Pairs are only reported once, i.e. only uses the upper triangle of the correlation matrix.
#'
#' @export

hicorrSP <- function(COIN, dset = "Normalised", hicorval = 0.9, cortype = "pearson",
                     grouplev = NULL){

  if(is.null(grouplev)){grouplev <- 2}

  out1 <- getIn(COIN, dset = dset)

  corr_ind <- stats::cor(out1$ind_data_only,
                         method = cortype, use = "pairwise.complete.obs") %>% round(2)

  subpill <- dplyr::select(COIN$Input$IndMeta, dplyr::starts_with("Agg"))[[grouplev-1]]

  samepill <- matrix(FALSE, nrow = nrow(corr_ind), ncol = ncol(corr_ind))

  for(ii in 1:ncol(samepill)){
    pill_ii <- subpill[ii]
    samepill[,ii] <- subpill == pill_ii
  }

  corr_ind[!samepill] <- NA
  diag(corr_ind) <- NA
  corr_ind[lower.tri(corr_ind)] <- NA
  hicorr <- reshape2::melt(corr_ind)
  colnames(hicorr) <- c("Ind1", "Ind2", "Corr")
  hicorr <- cbind(AggGroup = rep(subpill,nrow(corr_ind)), hicorr)
  hicorr <- hicorr[!is.na(hicorr$Corr),]
  hicorr <- hicorr[hicorr$Corr>hicorval,]

  return(tibble::as_tibble(hicorr))

}


#' Recalculate correlations and ranks based on new weights
#'
#' This is a short cut function which takes a new set of indicator weights, and recalculates the COIN results
#' based on these weights. It returns a summary of rankings and the correlations between indicators and index.
#'
#' This function is principally used inside [rew8r()]. The `w` argument should be a data frame of weights, of the same format
#' as the data frames found in `.$Parameters$Weights`.
#'
#' @param COIN COIN object
#' @param w Full data frame of weights for each level
#' @param aglevs A 2-length vector with two aggregation levels to correlate against each other
#' @param icodes List of two character vectors of indicator codes, corresponding to the two aggregation levels
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
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # get correlations between pillars (level 2) and index (level 4)
#' # original weights used just for demonstration, normally you would alter first.
#' l <- weights2corr(ASEM, ASEM$Parameters$Weights$Original, aglevs = c(2,4))
#'
#' @seealso
#' * [rew8r()] Interactive app for adjusting weights and seeing effects on correlations
#' * [getCorr()] Get correlations between indicators/levels
#'
#' @export

weights2corr <- function(COIN, w, aglevs = NULL, icodes = NULL,
                         cortype = "pearson", withparent = TRUE){

  if(is.null(aglevs)){
    aglevs <- c(1, COIN$Parameters$Nlevels)
  }

  if(is.null(COIN$Method$aggregate)){
    stop("You have not yet aggregated your data. This needs to be done first.")
  }

  # aggregate
  COIN2 <- aggregate(COIN, agtype = COIN$Method$aggregate$agtype,
                     agweights = w,
                     dset = COIN$Method$aggregate$dset,
                     agtype_bylevel = COIN$Method$aggregate$agtype_bylevel,
                     agfunc = COIN$Method$aggregate$agfunc
  )

  # get data to correlate against each other
  out1 <- getIn(COIN2, dset = "Aggregated", icodes = icodes[[1]], aglev = aglevs[1])
  idata1 <- out1$ind_data_only
  idata2 <- getIn(COIN2, dset = "Aggregated", icodes = icodes[[2]], aglev = aglevs[2])$ind_data_only
  # table of results data
  dfres <- COIN2$Data$Aggregated[c("UnitName",
                                   COIN$Parameters$AggCodes[[length(COIN$Parameters$AggCodes)]],
                                   COIN$Parameters$AggCodes[[length(COIN$Parameters$AggCodes)-1]])]
  dfres <- cbind("Rank"=rank(COIN2$Data$Aggregated$Index*-1, ties.method = "min"), dfres)
  # get correlations
  cr = stats::cor(idata1, idata2, method = cortype, use = "pairwise.complete.obs")

  # get index structure
  agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg"))

  # select cols corresponding to what is being correlated against what
  agcols <- agcols[aglevs]
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
              icodes1 = out1$ind_names)

  return(out)

}
