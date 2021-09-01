#' Builds ASEM example
#'
#' A short cut function for building the ASEM COIN. This builds the ASEM COIN up to and including aggregated results. See
#' the [ASEM Portal](https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository) for the underlying
#' data set and [online documentation](https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html) for more information on COINs.
#'
#' @importFrom magrittr extract
#' @importFrom dplyr select starts_with
#'
#' @examples
#' # Build the ASEM COIN
#' ASEM <- build_ASEM()
#'
#' @return COIN class object (a list) of the ASEM index, as well as information printed to the console - see [assemble()].
#'
#' @export

build_ASEM <- function(){

  # assemble the COIN
  ASEM <- assemble(IndData = COINr::ASEMIndData, IndMeta = COINr::ASEMIndMeta, AggMeta = COINr::ASEMAggMeta)
  # denominate data
  ASEM <- denominate(ASEM, dset = "Raw")
  # impute data using Asia/Europe group mean
  ASEM <- impute(ASEM, dset = "Denominated", imtype = "indgroup_mean", groupvar = "Group_EurAsia")
  # treat data, using default procedure
  ASEM <- treat(ASEM, dset = "Imputed")
  # normalise using default (min-max)
  ASEM <- normalise(ASEM, dset = "Treated")
  # aggregate using default (weighted arithmetic mean)
  ASEM <- aggregate(ASEM, dset = "Normalised")
  # generate some results tables
  ASEM <- getResults(ASEM, tab_type = "Aggregates", use = "scores", out2 = "COIN")
  ASEM <- getResults(ASEM, tab_type = "Aggregates", use = "ranks", out2 = "COIN")
  return(ASEM)

}
