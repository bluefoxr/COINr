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

  ASEM <- coin_assemble(IndData = COINr::ASEMIndData, IndMeta = COINr::ASEMIndMeta, AggMeta = COINr::ASEMAggMeta)
  ASEM <- coin_denominate(ASEM)
  ASEM <- coin_impute(ASEM, dset = "Denominated", imtype = "indgroup_mean", groupvar = "Group_EurAsia")
  ASEM <- coin_treat(ASEM, dset = "Imputed")
  ASEM <- coin_normalise(ASEM, dset = "Treated")
  ASEM <- coin_aggregate(ASEM, dset = "Normalised")
  return(ASEM)

}
