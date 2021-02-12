#' Interactive indicator distribution plots
#'
#' Generates a javascript distribution plot of a single indicator, using Plotly. Plot can be embedded e.g. in
#' HTML documents, websites, etc, or used for more interactive data exploration. This only plots one
#' indicator at a time - for multiple plots you can use plotIndDist()
#'
#' @param COINobj The COIN object, or a data frame of indicator data
#' @param dset The source data set to use for indicator data (if input is COIN object)
#' @param inames A character vector of a single indicator name or aggregate name to plot.
#' @param ptype The type of plot to produce. Currently supports "Violin" and "Histogram".
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

iplotIndDist <- function(COINobj, dset = "Raw", inames = NULL, ptype = "Violin", aglev = 1,
                        axlims = NULL){

  out1 <- getIn(COINobj, dset = dset, inames = inames, aglev = aglev)
  ind_data_only <- out1$ind_data_only
  ind_names <- out1$IndNames
  ind_code <- out1$IndCodes

  if(length(ind_names)>1){stop("This function only supports plotting single indicators. You may need to use the aglev argument if you are calling an aggregation group.")}

  if((out1$otype=="COINobj") & (aglev == 1)){
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

  if (ptype == "Violin"){

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

    # scale axes (for matching with another plot, for example).
    # Not scaled if axlims not specified (range = NULL does nothing)
    fig <- fig %>% plotly::layout( yaxis = list(title = indunit, zeroline = F, range = axlims) )

  } else if (ptype == "Histogram"){

    fig <- plot_ly(data = ind_data_only, x = ~get(ind_code), type = "histogram")

    # scale axes (for matching with another plot, for example).
    # Not scaled if axlims not specified (range = NULL does nothing)
    fig <- fig %>% plotly::layout(bargap=0.1, xaxis = list(title = indunit, range = axlims),
                                  yaxis = list(title = "Count"),
                                  title = ind_names)

    fig

  }

  fig

}
