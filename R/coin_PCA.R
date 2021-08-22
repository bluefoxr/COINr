#' Perform PCA on a COIN
#'
#' Performs Principle Component Analysis (PCA) on a specified data set and subset of indicators or aggregation groups. Returns weights
#' corresponding to the first principal component, i.e the weights that maximise the variance explained
#' by the linear combination of indicators.
#'
#' @param COIN An input object. The function can handle either the COIN object, or a data frame.
#' The data frame should have each column as an indicator, and an optional column `"UnitCode"` which
#' specifies the code (or name) of each unit. Any other type of object will return an error.
#' @param dset If input is a COIN object, this specifies which data set in `.$Data` to use.
#' @param icodes An optional character vector of indicator codes to subset the indicator data. Usefully, can also refer to
#' an aggregation group name, and data will be sub-setted accordingly. NOTE does not work with multiple aggregate group names.
#' @param aglev The aggregation level to take indicator data from. Integer from 1 (indicator level)
#' to N (top aggregation level, typically the index).
#' @param nowarnings If `FALSE` (default), will give warnings where missing data are found. Set to `TRUE` to suppress these warnings.
#' @param out2 If the input is a COIN object, this controls where to send the output. If `"COIN"`, it
#' sends the results to the COIN object, otherwise if `"list"`, outputs to a separate list.
#'
#' @importFrom stats prcomp na.omit
#' @importFrom rlang .data
#'
#' @examples \dontrun{
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta,
#' AggMeta = ASEMAggMeta)
#' # get PCA results for pillar groups inside "Conn" sub-index
#' # (warnings about missing data are suppressed)
#' PCAres <- getPCA(ASEM, dset = "Raw", icodes = "Sust",
#' aglev = 1, nowarnings = TRUE, out2 = "list")}
#'
#' @return PCA data, plus PCA weights corresponding to the loadings of the first principle component.
#' This should correspond to the linear combination of indicators that explains the most variance.
#'
#' @export
#'

getPCA <- function(COIN, dset = "Raw", icodes = NULL, aglev = NULL, nowarnings = FALSE, out2 = "COIN"){

  # There is a catch here because we might want to do PCA weights across one level, but that level
  # may have multiple groups. This means we have to call PCA separately for each group.

  # first we define a function which returns weights for a given set of indicator data
  # this function implicitly other variables from the environment inside getPCA() so we don't need
  # to explicitly pass everything to it.
  PCAwts <- function(icodes1){

    # get ind data
    out <- getIn(COIN, dset = dset, icodes = icodes1, aglev = aglev)

    # check for missing vals
    nNA <- sum(is.na(out$ind_data_only))

    # remove any rows with missing data
    if (nNA > 0){
      dat4PCA <- stats::na.omit(out$ind_data_only)
      if(!nowarnings){
        warning(paste0(nNA, " missing values found. Removing ", nrow(out$ind_data_only)-nrow(dat4PCA), " rows with missing values in order to perform
PCA. You can also try imputing data first to avoid this."))
      }

    } else {
      dat4PCA <- out$ind_data_only
    }

    # perform PCA
    PCAres <- stats::prcomp(dat4PCA, center = TRUE, scale = TRUE)

    # just for writing results - if aglev not specified then we are working at ind level
    if(is.null(aglev)){aglev<-1}

    # weight from first PC should be the max variance weights
    wts <- PCAres$rotation[,1] %>% as.numeric()

    return(list(wts = wts, PCAres = PCAres, IndCodes = out$IndCodes))
  }

  # OK, first thing is to find what groups we have
  # Get index structure
  agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg"))
  # Get cols of interest: the present one plus the parents
  agcols <- agcols[c(aglev, aglev + 1)]
  # We need to know the codes of the inds/aggs to get weights from
  out3 <- getIn(COIN, dset = dset, icodes = icodes, aglev = aglev)
  IndCodes <- out3$IndCodes
  # Get parents of these codes
  parents <- unique(agcols[(agcols[[1]] %in% IndCodes) ,2])
  parents <- parents[[1]]

  # Right, now we need to cycle through these groups and do PCA on each group.
  # List for general PCA results
  PCAlist <- vector(mode = "list", length = length(parents))
  # copy of weights to modify
  wlist <- COIN$Parameters$Weights$Original

  for (ii in 1: length(parents)){
    # get PCA results for group
    outPCA <- PCAwts(parents[ii])
    # attach weights to list
    # wts should be in the same order as out$IndCodes. We have to make sure they match exactly here as
    # sometimes things get reordered. This is done with match() rather than %in% for this reason.
    wlist$Weight[match(outPCA$IndCodes, wlist$Code)] <- outPCA$wts
    # add general results to list
    PCAlist[[ii]] <- outPCA
  }
  # rename list
  names(PCAlist) <- parents

  # write results
  if( (out3$otype == "COINobj") & (out2 == "COIN")){
    eval(parse(text=paste0("COIN$Parameters$Weights$PCA_",dset,"L",aglev,"<-wlist")))
    eval(parse(text=paste0("COIN$Analysis$",dset,"$PCA$L",aglev,"<- PCAlist")))
    return(COIN)
  } else {
    output <- list("Weights" = wlist,
                   "PCAresults" = PCAlist)
    return(output)
  }

}
