#' Perform PCA
#'
#' Performs PCA on a specified data set and subset of indicators or aggregation groups. Returns weights
#' corresponding to the first principal component, i.e the weights that maximise the variance explained
#' by the linear combination of indicators.
#'
#' @param COINobj An input object. The function can handle either the COIN object, or a data frame.
#' The data frame should have each column as an indicator, and an optional column "UnitCode" which
#' specifies the code (or name) of each unit. Any other type of object will return an error.
#' @param dset If input is a COIN object, this specifies which data set in .$Data to use.
#' @param inames An optional character vector of indicator codes to subset the indicator data. Usefully, can also refer to
#' an aggregation group name, and data will be subsetted accordingly. NOTE does not work with multiple aggregate group names.
#' @param aglev The aggregation level to take indicator data from. Integer from 1 (indicator level)
#' to N (top aggregation level, typically the index).
#' @param out2 If the input is a COIN object, this controls where to send the output. If "obj", it
#' sends the results to the COIN object, otherwise if "list", outputs to a separate list.
#'
#' @importFrom stats prcomp na.omit
#' @importFrom rlang .data
#'
#' @examples \dontrun{PCAresults <- coin_PCA(COINobj, dset = "Raw")}
#'
#' @return PCA data, plus PCA weights corresponding to the loadings of the first principle component.
#' This should correspond to the linear combination of indicators that explains the most variance.
#'
#' @export
#'

coin_PCA <- function(COINobj, dset = "Raw", inames = NULL, aglev = NULL, out2 = "obj"){

  # get ind data
  out <- getIn(COINobj, dset = dset, inames = inames, aglev = aglev)

  # check for missing vals
  nNA <- sum(is.na(out$ind_data_only))

  # remove any rows with missing data
  if (nNA > 0){
    dat4PCA <- stats::na.omit(out$ind_data_only)
    warning(paste0(nNA, " missing values found. Removing ", nrow(out$ind_data_only)-nrow(dat4PCA), " rows with missing values in order to perform
PCA. You can also try imputing data first to avoid this."))
  } else {
    dat4PCA <- out$ind_data_only
  }

  # perform PCA
  PCAres <- stats::prcomp(dat4PCA, center = TRUE, scale = TRUE)

  # just for writing results - if aglev not specified then we are working at ind level
  if(is.null(aglev)){aglev<-1}

  # weight from first PC should be the max variance weights
  wts <- PCAres$rotation[,1] %>% as.numeric()

  # write results
  if( (out$otype == "COINobj") & (out2 == "obj")){
    eval(parse(text=paste0("COINobj$Parameters$Weights$PCA$",dset,"L",aglev,"<-wts")))
    eval(parse(text=paste0("COINobj$Analysis$",dset,"$PCA$L",aglev,"<- PCAres")))
    return(COINobj)
  } else {
    output <- list("Weights" = wts,
                   "PCAresults" = PCAres)
    return(output)
  }

}
