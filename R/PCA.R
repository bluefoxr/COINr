#' Perform PCA on a coin
#'
#' Performs Principle Component Analysis (PCA) on a specified data set and subset of indicators or aggregation groups.
#' This function has two main outputs: the output(s) of [stats::prcomp()], and optionally the weights resulting from
#' the PCA. Therefore it can be used as an analysis tool and/or a weighting tool. For the weighting aspect, please
#' see the details below.
#'
#' PCA must be approached with care and an understanding of what is going on. First, let's consider the PCA excluding
#' the weighting component. PCA takes a set of data consisting of variables (indicators) and observations. It then
#' rotates the coordinate system such that in the new coordinate system, the first axis (called the first principal
#' component (PC)) aligns with the direction of maximum variance of the data set. The amount of variance explained by the
#' first PC, and by the next several PCs, can help to understand whether the data can be explained by simpler set of
#' variables. PCA is often used for dimensionality reduction in modelling, for example.
#'
#' In the context of composite indicators, PCA can be used first as an analysis tool. We can check for example, within
#' an aggregation group, can the indicators mostly be explained by one PC? If so, this gives a little extra justification
#' to aggregating the indicators because the information lost in aggregation will be less. We can also check this over
#' the entire set of indicators.
#'
#' The complications are in a composite indicator, the indicators are grouped and arranged into a hierarchy. This means
#' that when performing a PCA, we have to decide which level to perform it at, and which groupings to use, if any. The [get_PCA()]
#' function, using the `by_groups` argument, allows to automatically apply PCA by group if this is required.
#'
#' The output of [get_PCA()] is a PCA object for each of the groups specified, which can then be examined using existing
#' tools in R, see `vignette("analysis")`.
#'
#' The other output of [get_PCA()] is a set of "PCA weights" if the `weights_to` argument is specified. Here we also need
#' to say some words of caution. First, what constitutes "PCA weights" in composite indicators is not very well-defined.
#' In COINr, a simple option is adopted. That is, the loadings of the first principal component are taken as the weights.
#' The logic here is that these loadings should maximise the explained variance - the implication being that if we use
#' these as weights in an aggregation, we should maximise the explained variance and hence the information passed from
#' the indicators to the aggregate value. This is a nice property in a composite indicator, where one of the aims is to
#' represent many indicators by single composite. See \doi{10.1016/j.envsoft.2021.105208} for a
#' discussion on this.
#'
#' But. The weights that result from PCA have a number of downsides. First, they can often include negative weights
#' which can be hard to justify. Also PCA may arbitrarily flip the axes (since from a variance point of view the
#' direction is not important). In the quest for maximum variance, PCA will also weight the strongest-correlating
#' indicators the highest, which means that other indicators may be neglected. In short, it often results in a very
#' unbalanced set of weights. Moreover, PCA can only be performed on one level at a time.
#'
#' All these considerations point to the fact: while PCA as an analysis tool is well-established, please use PCA weights
#' with care and understanding of what is going on.
#'
#' @param coin A coin
#' @param dset The name of the data set in `.$Data` to use.
#' @param iCodes An optional character vector of indicator codes to subset the indicator data, passed to [get_data()]
#' @param Level The aggregation level to take indicator data from. Integer from 1 (indicator level)
#' to N (top aggregation level, typically the index).
#' @param by_groups If `TRUE` (default), performs PCA inside each aggregation group inside the specified level. If `FALSE`,
#' performs a single PCA over all indicators/aggregates in the specified level.
#' @param nowarnings If `FALSE` (default), will give warnings where missing data are found. Set to `TRUE` to suppress these warnings.
#' @param out2 If the input is a coin object, this controls where to send the output. If `"coin"`, it
#' sends the results to the coin object, otherwise if `"list"`, outputs to a separate list (default).
#' @param weights_to A string to name the resulting set of weights. If this is specified, and `out2 = "coin"`,
#' will write a new set of "PCA weights" to the `.$Meta$Weights` list. This is experimental - see details. If
#' `NULL`, does not write any weights (default).
#'
#' @importFrom stats prcomp na.omit
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # PCA on "Sust" group of indicators
#' l_pca <- get_PCA(coin, dset = "Raw", iCodes = "Sust", out2 = "list")
#'
#' # Summary of results for one of the sub-groups
#' summary(l_pca$PCAresults$Social$PCAres)
#'
#' @return
#' If `out2 = "coin"`, results are appended to the coin object. Specifically:
#' * A list is added to `.$Analysis` containing PCA weights (loadings) of the first principle component, and the output of [stats::prcomp], for each
#' aggregation group found in the targeted level.
#' * If `weights_to` is specified, a new set of PCA weights is added to `.$Meta$Weights`
#' If `out2 = "list"` the same outputs are contained in a list.
#'
#' @seealso
#' * [stats::prcomp] Principle component analysis
#'
#' @export
get_PCA <- function(coin, dset = "Raw", iCodes = NULL, Level = NULL, by_groups = TRUE,
                   nowarnings = FALSE, weights_to = NULL, out2 = "list"){

  if(is.null(Level)){
    Level <- 1
  }

  # There is a catch here because we might want to do PCA weights across one level, but that level
  # may have multiple groups. This means we have to call PCA separately for each group.

  # first we define a function which returns weights for a given set of indicator data
  # this function implicitly calls other variables from the environment inside getPCA() so we don't need
  # to explicitly pass everything to it.
  PCAwts <- function(icodes1){

    # get ind data
    iData_ <- get_data(coin, dset = dset, iCodes = icodes1, Level = Level, also_get = "none")

    # check for missing vals
    nNA <- sum(is.na(iData_))

    # remove any rows with missing data
    if (nNA > 0){
      dat4PCA <- stats::na.omit(iData_)
      if(!nowarnings){
        warning(paste0(nNA, " missing values found. Removing ", nrow(iData_)-nrow(dat4PCA), " rows with missing values in order to perform
PCA. You can also try imputing data first to avoid this."))
      }

    } else {
      dat4PCA <- iData_
    }

    # perform PCA
    PCAres <- stats::prcomp(dat4PCA, center = TRUE, scale = TRUE)

    # just for writing results - if Level not specified then we are working at ind level
    if(is.null(Level)){Level<-1}

    # weight from first PC should be the max variance weights
    wts <- as.numeric(PCAres$rotation[,1])

    list(wts = wts, PCAres = PCAres, iCodes = names(iData_))
  }

  # We need to know the codes of the inds/aggs to get weights from
  iData_full <- get_data(coin, dset = dset, iCodes = iCodes, Level = Level, also_get = "none")
  IndCodes <- names(iData_full)

  if(by_groups){
    # OK, first thing is to find what groups we have
    # Get index structure
    lin <- coin$Meta$Lineage
    # Get cols of interest: the present one plus the parents
    lin <- lin[c(Level, Level + 1)]
    # Get parents of these codes
    parents <- unlist(unique(lin[(lin[[1]] %in% IndCodes) ,2]))
  } else {
    parents = "All"
  }


  # Right, now we need to cycle through these groups and do PCA on each group.
  # List for general PCA results
  PCAlist <- vector(mode = "list", length = length(parents))
  # copy of weights to modify
  wlist <- coin$Meta$Weights$Original

  for (ii in 1: length(parents)){
    if(by_groups){
      # get PCA results for group
      outPCA <- PCAwts(parents[ii])
    } else {
      # get PCA results for group
      outPCA <- PCAwts(NULL)
    }
    # attach weights to list
    # wts should be in the same order as out$iCodes. We have to make sure they match exactly here as
    # sometimes things get reordered. This is done with match() rather than %in% for this reason.
    wlist$Weight[match(outPCA$iCodes, wlist$iCode)] <- outPCA$wts
    # add general results to list
    PCAlist[[ii]] <- outPCA
  }
  # rename list
  names(PCAlist) <- parents

  # write results
  if(out2 == "coin"){

    if(!is.null(weights_to)){
      #w_name <- paste0("PCA_",dset,"L",Level)
      # write weights
      coin$Meta$Weights[[weights_to]] <- wlist
      message("Weights written to .$Meta$Weights$", weights_to)
    }

    # write other info
    coin$Analysis[[dset]][[paste0("$PCA$L",Level)]] <- PCAlist

    coin

  } else {

    list("Weights" = wlist,
                   "PCAresults" = PCAlist)
  }

}
