#' Plots of correlations
#'
#' Visualisation of correlations between indicators.
#'
#' @param COINobj A list of indicator data, stuctured using the COIN_assemble function
#' @param inames An optional character vector of indicator codes to plot
#' @param dset The source dataset to select
#' @param plotgroups Logical: if TRUE, overlays rectangles representing aggregation groups
#'
#' @importFrom stats cor
#' @importFrom corrplot cor.mtest corrplot.mixed corrRect
#' @importFrom dplyr select starts_with pull
#' @importFrom purrr map_dbl
#'
#' @examples \dontrun{coin_plotcorr(COINobj)}
#'
#' @return A correlation plot.
#'
#' @export

coin_plotcorr <- function(COINobj, inames = NULL, dset = "Raw", plotgroups = TRUE){

  # First, get relevant bits of the input
  out <- coin_aux_objcheck(COINobj, dset, inames)
  ind_data <- out$ind_data
  ind_data_only <- out$ind_data_only
  ind_names <- out$ind_names

  corr_ind <- stats::cor(ind_data_only, method = "pearson", use = "na.or.complete") # get correlation matrix, just indicators
  p_ind <- corrplot::cor.mtest(ind_data_only, method = "pearson") # p values

  # plot indicator correlations (perhaps move this to another function...)
  corrplot::corrplot.mixed(corr_ind, p.mat = p_ind$p, sig.level = .01, insig = "blank",
                 upper = "ellipse", tl.pos = "lt")

  agspec <- COINobj$Input$IndMeta[COINobj$Input$IndMeta$IndCode %in% ind_names,] %>%
    dplyr::select(dplyr::starts_with("Agg"))

  # Now overlay aggregation groups
  if(plotgroups == TRUE){ # plot groups as rectangles if asked
    colourz <- c("black","blue","green","red")
    for (ii in 1:(COINobj$Parameters$Nlevels-2)){
      agcol <- dplyr::pull(agspec, ii)
      groupz <- purrr::map_dbl(unique(agcol), ~{sum(agcol==.x) })
      #groupz <- as.data.frame(table(agspec[,ii])) # this gets a summary of the number of repetitions of each value, so the number of indicators in each agg. group
      corrplot::corrRect(groupz, col = colourz[ii], lwd = ii) # plot rectangle, changing colour and thickness
    }
  }
}
