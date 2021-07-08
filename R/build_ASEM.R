#' Builds ASEM example
#'
#' A short cut for building the ASEM index. Mainly used in development.
#'
#' @importFrom magrittr extract
#' @importFrom dplyr select starts_with
#'
#' @examples \dontrun{ASEM <- build_ASEM()}
#'
#' @return COIN object of ASEM index.
#'
#' @export

build_ASEM <- function(){

  ASEM <- assemble(IndData = COINr::ASEMIndData, IndMeta = COINr::ASEMIndMeta, AggMeta = COINr::ASEMAggMeta)
  ASEM <- denominate(ASEM, dset = "Raw")
  ASEM <- impute(ASEM, dset = "Denominated", imtype = "indgroup_mean", groupvar = "Group_EurAsia")
  ASEM <- treat(ASEM, dset = "Imputed")
  ASEM <- normalise(ASEM, dset = "Treated")
  ASEM <- aggregate(ASEM, dset = "Normalised")
  return(ASEM)

}
