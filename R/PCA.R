#' Perform PCA on a coin
#'
#' Performs Principle Component Analysis (PCA) on a specified data set and subset of indicators or aggregation groups. Returns weights
#' corresponding to the first principal component, i.e the weights that maximise the variance explained
#' by the linear combination of indicators.
#'
#' Note that `getPCA()` is simply a quick wrapper for [stats::prcomp()] which makes PCA on COINs quicker.
#' See [COINr online documentation](https://bluefoxr.github.io/COINrDoc/multivariate-analysis.html#pca) for more details and examples.
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
#' sends the results to the coin object, otherwise if `"list"`, outputs to a separate list.
#'
#' @importFrom stats prcomp na.omit
#'
#' @examples
#' #
#'
#' @return
#' If `out2 = "coin"`, results are appended to the coin object. Specifically:
#' * A new set of PCA weights is added to `.$Meta$Weights`
#' * A list is added to `.$Analysis` containing PCA weights (loadings) of the first principle component, and the output of [stats::prcomp], for each
#' aggregation group found in the targeted level.
#' If `out2 = "list"` the same outputs are contained in a list.
#'
#' @seealso
#' * [stats::prcomp] Principle component analysis
#'
#' @export
get_PCA <- function(coin, dset = "Raw", iCodes = NULL, Level = NULL, by_groups = TRUE,
                   nowarnings = FALSE, out2 = "coin"){

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

    list(wts = wts, PCAres = PCAres, IndCodes = names(iData_))
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
    # wts should be in the same order as out$IndCodes. We have to make sure they match exactly here as
    # sometimes things get reordered. This is done with match() rather than %in% for this reason.
    wlist$Weight[match(outPCA$IndCodes, wlist$iCode)] <- outPCA$wts
    # add general results to list
    PCAlist[[ii]] <- outPCA
  }
  # rename list
  names(PCAlist) <- parents

  # write results
  if(out2 == "coin"){

    if(is.null(w_name)){
      w_name <- paste0("PCA_",dset,"L",Level)
    }
    # write weights
    coin$Meta$Weights[[w_name]] <- wlist
    # write other info
    coin$Analysis[[dset]][[paste0("$PCA$L",Level)]] <- PCAlist

    coin

  } else {

    list("Weights" = wlist,
                   "PCAresults" = PCAlist)
  }

}
