#' Static heatmaps of correlation using ggplot2
#'
#' Correlation heatmaps
#'
#' @param COIN The COIN object
#' @param dset The data set to treat
#' @param icodes An optional list or character vector where the first entry specifies the indicator/agg
#' codes to correlate against the second entry (also a specification of ind/agg codes)
#' @param aglevs The aggregation levels to take the two groups of indicators from. See getIn() for details.
#'
#' @importFrom ggplot2 ggplot aes geom_tile
#'
#' @return A treated data set plus information about how the data was treated.
#'
#' @export

plotCorr <- function(COIN, dset = "Raw", icodes = NULL, aglevs = 1){

  if (length(icodes) == 1){
    icodes = rep(icodes, 2)
  }
  if (length(aglevs) == 1){
    aglevs = rep(aglevs, 2)
  }

  # get data sets
  out1 <- getIn(COIN, dset = dset, icodes = icodes[[1]], aglev = aglevs[1])
  out2 <- getIn(COIN, dset = dset, icodes = icodes[[2]], aglev = aglevs[2])

  # get corr matrix
  crmat <- stats::cor(out1$ind_data_only, out2$ind_data_only,
                     use = "pairwise.complete.obs", method = "pearson")
  crmat <- round(crmat,2)
  crmat_melt <- reshape2::melt(crmat)

  # heatmap plot
  ggplot2::ggplot(crmat_melt,
                  ggplot2::aes(x = Var1, y = Var2, fill = value, label = value)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
    ggplot2::scale_fill_gradient2(mid="#FBFEF9",low="#A63446",high="#0C6291", limits=c(-1,1)) +
    ggplot2::geom_text(colour = "white") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_x_discrete(expand=c(0,0)) +
    ggplot2::scale_y_discrete(expand=c(0,0))

}
