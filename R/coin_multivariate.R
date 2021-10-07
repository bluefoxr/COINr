#' Static heatmaps of correlation matrices
#'
#' Generates heatmaps of correlation matrices using **ggplot2**. This enables correlating any set of indicators against any other,
#' and supports calling named aggregation groups of indicators. The `withparent` argument generates tables of correlations only with
#' parents of each indicator. Also supports discrete colour maps using `flagcolours`, different types of correlation, and groups
#' plots by higher aggregation levels.
#'
#' This function calls [getCorr()].
#'
#' Note that this function can only call correlations within the same data set (i.e. only one data set in `.$Data`).
#'
#' @param COIN The COIN object
#' @param dset The target data set.
#' @param icodes An optional list of character vectors where the first entry specifies the indicator/aggregate
#' codes to correlate against the second entry (also a specification of indicator/aggregate codes)
#' @param aglevs The aggregation levels to take the two groups of indicators from. See [getIn()] for details.
#' @param cortype The type of correlation to calculate, either `"pearson"`, `"spearman"`, or `"kendall"` (see [stats::cor()]).
#' @param withparent If `aglev[1] != aglev[2]`, and equal `"parent"` will only plot correlations of each row with its parent (default).
#' If `"family"`, plots the lowest aggregation level in `aglevs` against all its parent levels.
#' If `"none"` plots the full correlation matrix.
#' @param grouplev The aggregation level to group correlations by if `aglev[1] == aglev[2]`. By default, groups correlations into the
#' aggregation level above. Set to 0 to disable grouping and plot the full matrix.
#' @param showvals If `TRUE`, shows correlation values. If `FALSE`, no values shown.
#' @param flagcolours If `TRUE`, uses discrete colour map with thresholds defined by `flagthresh`. If `FALSE` uses continuous colour map.
#' @param flagthresh A 3-length vector of thresholds for highlighting correlations, if `flagcolours = TRUE`.
#' `flagthresh[1]` is the negative threshold. Below this value, values will be flagged red.
#' `flagthresh[2]` is the "weak" threshold. Values between `flagthresh[1]` and `flagthresh[2]` are coloured grey.
#' `flagthresh[3]` is the "high" threshold. Anything between `flagthresh[2]` and `flagthresh[3]` is flagged "OK",
#' and anything above `flagthresh[3]` is flagged "high".
#' @param pval The significance level for plotting correlations. Correlations with \eqn{p < pval} will be shown,
#' otherwise they will be plotted as the colour specified by `insig_colour`. Set to 0 to disable this.
#' @param insig_colour The colour to plot insignificant correlations. Defaults to a light grey.
#' @param text_colour The colour of the correlation value text (default white).
#' @param discrete_colours An optional 4-length character vector of colour codes or names to define the discrete
#' colour map if `flagcolours = TRUE` (from high to low correlation categories). Defaults to a green/blue/grey/purple.
#' @param out2 If `"fig"` returns a plot, if `"dflong"` returns the correlation matrix in long form, if `"dfwide"`,
#' returns the correlation matrix in wide form. The last option here is probably useful if you want to
#' present a table of the data in a report.
#'
#' @importFrom ggplot2 ggplot aes geom_tile
#'
#' @examples
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # correlation data frame of indicators in connectivity sub-index, grouped by pillar
#' corrs <- plotCorr(ASEM, dset = "Raw", icodes = "Conn", aglevs = 1,
#' showvals = F, out2 = "dflong")
#' # NOTE to create a plot instead set out2 = "fig"
#'
#' @return If `out2 = "fig"` returns a plot generated with **ggplot2**. These can be edited further with **ggplot2** commands.
#' If `out2 = "dflong"` returns the correlation matrix as a data frame in long form, if `out2 = "dfwide"`,
#' returns the correlation matrix in wide form. The last option here is probably useful if you want to
#' present a table of the data in a report.
#'
#' @seealso
#' * [getCorr()] Getting correlation matrices of indicator subsets
#'
#' @export

plotCorr <- function(COIN, dset = "Raw", icodes = NULL, aglevs = 1, cortype = "pearson",
                     withparent = "parent", grouplev = NULL, showvals = TRUE, flagcolours = FALSE,
                     flagthresh = c(-0.4, 0.3, 0.9), pval = 0.05, insig_colour = "#F0F0F0",
                     text_colour = "white", discrete_colours = NULL, out2 = "fig"){

  if (length(icodes) == 1){
    icodes = rep(icodes, 2)
  }
  if (length(aglevs) == 1){
    aglevs = rep(aglevs, 2)
  }
  if (aglevs[2] > aglevs [1]){
    aglevs <- rev(aglevs)
    icodes <- rev(icodes)
  }

  ##- GET CORRELATIONS -----------------------------

  if(withparent == "none" | withparent == "parent"){

    if(withparent == "none") withparent <- FALSE
    if(withparent == "parent") withparent <- TRUE

    crtable <- getCorr(COIN, dset = dset, icodes = icodes, aglevs = aglevs, cortype = cortype,
                       pval = pval, withparent = withparent, grouplev = grouplev)

  } else if (withparent == "family"){

    # we repeat for all levels above. Start with the lowest agg level specified
    aglev1 <- min(aglevs)

    # get index structure
    agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg"))

    crtable <- data.frame(Var1 = NA, Var2 = NA, Correlation = NA)

    for (ii in (aglev1 + 1):ncol(agcols)){
      crtableii <- getCorr(COIN, dset = dset, icodes = icodes, aglevs = c(ii, aglev1), cortype = cortype,
                           pval = pval, withparent = TRUE, grouplev = grouplev)
      crtableii$Var1 <- colnames(agcols)[ii]
      crtable <- rbind(crtable, crtableii)
    }
    crtable <- crtable[-1,]

  }

  # remove diags, otherwise plot looks annoying
  crtable <- crtable[crtable$Var1 != crtable$Var2,]

  ##- PLOT -----------------------------------------

  if(out2 == "fig"){
    # get orders (otherwise ggplot reorders)
    ord1 <- unique(crtable$Var1)
    ord2 <- unique(crtable$Var2)

    # if we are correlating a set with itself, we make sure the orders match
    if(setequal(ord1,ord2)){
      # reversing agrees with the "classical" view of a correlation matrix
      ord2 <- rev(ord1)
    }

    # for discrete colour map
    hithresh <- 0.9
    weakthresh <- 0.3
    negthresh <- -0.4

    if (flagcolours){

      # make new col with flags for each correlation
      crtable$Flag <- ifelse(crtable$Correlation >= hithresh, yes = "High", no = "OK")
      crtable$Flag[(crtable$Correlation <= weakthresh)] <- "Weak"
      crtable$Flag[(crtable$Correlation <= negthresh)] <- "Negative"

      # heatmap plot
      plt <- ggplot2::ggplot(crtable,
                             ggplot2::aes(x = factor(.data$Var1, levels = ord1),
                                          y = factor(.data$Var2, levels = ord2),
                                          fill = .data$Flag,
                                          label = .data$Correlation)) +
        ggplot2::geom_tile(colour = "white") +
        ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::scale_x_discrete(expand=c(0,0)) +
        ggplot2::scale_y_discrete(expand=c(0,0))

      if(is.null(discrete_colours)){
        discrete_colours <- c("#62910c", "#b8e8b5", "#e2e6e1", "#b25491")
      } else {
        stopifnot(is.character(discrete_colours),
                  length(discrete_colours)==4)
      }

      plt <- plt + ggplot2::scale_fill_manual(
        breaks = c("High", "OK", "Weak", "Negative"),
        values = discrete_colours,
        na.value = insig_colour
      )
    } else {

      # heatmap plot
      plt <- ggplot2::ggplot(crtable,
                             ggplot2::aes(x = factor(.data$Var1, levels = ord1),
                                          y = factor(.data$Var2, levels = ord2),
                                          fill = .data$Correlation,
                                          label = .data$Correlation)) +
        ggplot2::geom_tile(colour = "white") +
        ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::scale_x_discrete(expand=c(0,0)) +
        ggplot2::scale_y_discrete(expand=c(0,0))

      plt <- plt + ggplot2::scale_fill_gradient2(mid="#FBFEF9",low="#A63446",high="#0C6291", limits=c(-1,1),
                                                 na.value = insig_colour)
    }

    if (showvals){
      plt <- plt + ggplot2::geom_text(colour = text_colour, size = 3)
    }
    return(plt)

  } else if (out2 == "dflong"){

    # output df instead in long form
    return(crtable)

  } else if (out2 == "dfwide"){

    # output df instead in wide form
    crtable <- tidyr::pivot_wider(crtable, values_from = .data$Correlation, names_from = .data$Var1)
    colnames(crtable)[1] <- "Indicator"
    return(crtable)

  }

}


#' Get different types of correlation matrices
#'
#' Helper function for getting correlations between indicators. This retrieves subsets of correlation
#' matrices between different aggregation levels, in different formats.
#'
#' Note that this function can only call correlations within the same data set (i.e. only one data set in `.$Data`).
#'
#' @param COIN The COIN object
#' @param dset The target data set
#' @param icodes An optional list of character vectors where the first entry specifies the indicator/aggregate
#' codes to correlate against the second entry (also a specification of indicator/aggregate codes).
#' @param aglevs The aggregation levels to take the two groups of indicators from. See [getIn()] for details. Defaults to indicator level.
#' @param cortype The type of correlation to calculate, either `"pearson"`, `"spearman"`, or `"kendall"`.
#' @param withparent If `TRUE`, and `aglev[1] != aglev[2]`, will only return correlations of each row with its parent.
#' @param grouplev The aggregation level to group correlations by if `aglev[1] == aglev[2]`. By default, groups correlations into the
#' aggregation level above. Set to 0 to disable grouping and return the full matrix.
#' @param pval The significance level for including correlations. Correlations with \eqn{p > pval} will be returned as `NA`.
#' Default 0.05. Set to 0 to disable this.
#'
#' @examples
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # correlations of indicators in Political pillar
#' corrs <- getCorr(ASEM, dset = "Raw", icodes = "Political", aglevs = 1)
#'
#' @return A data frame of correlation values in long format. Correlations with \eqn{p > pval} will be returned as `NA`.
#'
#' @seealso
#' * [plotCorr()] Plot correlation matrices of indicator subsets
#'
#' @export

getCorr <- function(COIN, dset, icodes = NULL, aglevs = NULL, cortype = "pearson",
                    pval = 0.05, withparent = TRUE, grouplev = NULL){

  if(is.null(aglevs)){aglevs = 1}

  if (length(icodes) == 1){
    icodes = rep(icodes, 2)
  }
  if (length(aglevs) == 1){
    aglevs = rep(aglevs, 2)
  }
  if (aglevs[2] > aglevs [1]){
    aglevs <- rev(aglevs)
    icodes <- rev(icodes)
  }

  #- GET DATA AND CORR ---------------------------

  # get data sets
  out1 <- getIn(COIN, dset = dset, icodes = icodes[[1]], aglev = aglevs[1])
  out2 <- getIn(COIN, dset = dset, icodes = icodes[[2]], aglev = aglevs[2])

  # get corr matrix
  crmat <- stats::cor(out1$ind_data_only, out2$ind_data_only,
                      use = "pairwise.complete.obs", method = cortype)
  crmat <- round(crmat,2)

  # set insignificant correlations to zero if requested
  if(pval > 0){
    # p values
    p_ind <- corrplot::cor.mtest(cbind(out1$ind_data_only, out2$ind_data_only), method = cortype)$p
    # relevant part of the matrix
    p_ind2 <- p_ind[1:ncol(out1$ind_data_only), ((ncol(out1$ind_data_only)+1):ncol(p_ind))]
    # set elements of crmat to zero, where correlations are below significance level
    # when plotted, these will be white, so invisible
    crmat[p_ind2 > pval] <- NA
  }

  crmat_melt <- reshape2::melt(crmat)
  # remove rows with NAs
  #crmat_melt <- crmat_melt[!is.na(crmat_melt$value),]

  #- PARENTS -------------------------------------

  if (withparent & (ncol(crmat)>1) & aglevs[1]!=aglevs[2]){

    # get index structure
    agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg"))
    # select cols corresponding to what is being correlated against what
    agcols <- agcols[aglevs]
    # rename corr matrix cols to prepare for join
    colnames(crmat_melt) <- c(colnames(agcols), "Correlation")

    # now do inner join - we are matching correlation rows that agree with the structure of the index
    crtable <- dplyr::inner_join(agcols, crmat_melt, by = colnames(agcols))
    # sometimes this throws duplicates, so remove
    crtable <- unique(crtable)
    # rename cols for plotting
    colnames(crtable)[1:2] <- c("Var1", "Var2")

  } else {

    crtable <- crmat_melt
    colnames(crtable) <- c("Var1", "Var2", "Correlation")

  }

  ##- GROUP ----------------------------------------
  # If correlating against the same level, only show within groups if asked

  # first, if grouplev not specified, group by level above

  if(is.null(grouplev)) grouplev <- aglevs[1] + 1

  if ((grouplev > 0) & (aglevs[1] == aglevs[2])){

    # get index structure
    agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg"))

    if(grouplev > ncol(agcols)){
      stop("Grouping level is out of range - should be between 2 and ", ncol(agcols), " or zero to turn off.")
    }

    # select cols corresponding to current level, plus the one above
    # remember here we are correlating a level against itself, so aglevs[1]==aglevs[2]
    agcols <- agcols[c(aglevs[1], grouplev)]
    # get unique groups in level above
    lev2 <- unique(agcols[[2]])

    # initialise df for recording entries of corr matrix to keep
    keeprows <- data.frame(Var1 = NA, Var2 = NA)

    # loop over the levels above
    for (lev2ii in lev2){
      # get child codes
      lev1 <- agcols[agcols[2]==lev2ii, 1]
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
    crtable <- dplyr::inner_join(keeprows, crmat_melt, by = colnames(keeprows))
    # sometimes this throws duplicates, so remove
    crtable <- unique(crtable)

  }

  crtable

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
