#' Interactive violin plot
#'
#' Generates a javascript violin plot of a single indicator, using Plotly. Plot can be embedded e.g. in
#' HTML documents, websites, etc, or used for more interactive data exploration. This only plots one
#' indicator at a time - for multiple plots you can use plotIndDist()
#'
#' @param COINobj The COIN object, or a data frame of indicator data
#' @param dset The source data set to use for indicator data (if input is COIN object)
#' @param inames A character vector of a single indicator name or aggregate name to plot.
#' @param aglev The aggregation level to extract the indicator data from. Defaults to indicator level (1)
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_light geom_dotplot geom_violin geom_histogram labs facet_wrap
#'
#' @examples \dontrun{plotIndDist(ASEM, type = "Box", inames = "Physical")}
#'
#' @return Nice plots
#'
#' @export

iplotViolin <- function(COINobj, dset = "Raw", inames = NULL, aglev = 1, axlims = NULL){

  out1 <- getIn(COINobj, dset = dset, inames = inames, aglev = aglev)
  ind_data_only <- out1$ind_data_only
  ind_names <- out1$IndNames
  ind_code <- out1$IndCodes

  if(length(ind_names)>1){stop("This function only supports plotting single indicators.")}

  if(out1$otype=="COINobj"){
    # look for units
    if(exists("IndUnit",COINobj$Input$IndMeta)){
      # find unit for indicator
      indunit <- COINobj$Input$IndMeta$IndUnit[COINobj$Input$IndMeta$IndCode == ind_code]
    } else {
      # if not, NULL
      indunit <- ""
    }
  } else {
    # if not COINobj, no units
    indunit <- ""
  }

  fig <- plotly::plot_ly(data =ind_data_only, y = ~get(ind_code), type = 'violin',
                         box = list(visible = T),
                         meanline = list(visible = T),
                         x0 = ind_names,
                         points = 'all',
                         pointpos = -1.5,
                         jitter = 0.1,
                         hoveron = "points+kde",
                         color = I("#8dd3c7"),
                         marker = list(
                           line = list(
                             width = 2,
                             color = "#8dd3c7"
                           ),
                           symbol = 'line-ew'
                         )
  )
  fig <- fig %>% plotly::layout( yaxis = list(title = indunit, zeroline = F, range = axlims) )

  fig

}
