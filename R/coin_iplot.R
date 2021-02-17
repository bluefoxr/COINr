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
#' @param axlims Optional parameter specifying axis limits. Useful mainly for matching with another plot.
#'
#' @importFrom plotly plot_ly layout
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


#' Interactive indicator distribution plots for two indicators simultaneously
#'
#' Generates a javascript distribution plot of two indicators, using Plotly. Plot can be embedded e.g. in
#' HTML documents, websites, etc, or used for more interactive data exploration.
#'
#' @param COIN The COIN, or a data frame of indicator data
#' @param dsets The source data sets to use for indicator data (if input is COIN object). If the source
#' data sets are the same, this can be a single character string, otherwise, a character vector, e.g.
#' c("Raw", "Treated").
#' @param inames A character vector of two indicator codes to plot (corresponding to the two dsets specified)
#' @param ptype The type of plot to produce. Currently supports "Histogram" and "Scatter".
#' @param aglevs The aggregation level to extract the indicator data from. Defaults to indicator level (1). This also can
#' be specified as a vector if the two indicators are from different levels.
#'
#' @importFrom plotly plot_ly
#'
#' @examples \dontrun{plotIndDist(ASEM, type = "Box", inames = "Physical")}
#'
#' @return Nice plots
#'
#' @export

iplotIndDist2 <- function(COIN, dsets = "Raw", inames = NULL, ptype = "Scatter", aglevs = 1){

  if(length(inames)>2){stop("This function only supports plotting two indicators. You may need to use the aglev argument if you are calling an aggregation group.")}

  # If only one dset specified, use this for both indicators
  if(length(dsets)==1){dsets <- rep(dsets,2)}

  # if only one ind specified, use for both
  if(length(inames)==1){inames <- rep(inames,2)}

  # if only one aglev specified, use for both
  if(length(aglevs)==1){aglevs <- rep(aglevs,2)}

  # get indicator data
  out1 <- getIn(COIN, dset = dsets[1], inames = inames[1], aglev = aglevs[1])
  out2 <- getIn(COIN, dset = dsets[2], inames = inames[2], aglev = aglevs[2])

  ind_data_only <- out1$ind_data_only
  ind_names <- out1$IndNames
  ind_code <- out1$IndCodes


  if((out1$otype=="COINobj") & (aglevs[1] == 1)){
    # look for units
    if(exists("IndUnit",COIN$Input$IndMeta)){
      # find unit for indicator
      indunit1 <- COIN$Input$IndMeta$IndUnit[COIN$Input$IndMeta$IndCode == inames[1]]
    } else {
      # if not, NULL
      indunit1 <- ""
    }
  } else {
    # if not COIN, no units
    indunit1 <- ""
  }

  if((out2$otype=="COINobj") & (aglevs[2] == 1)){
    # look for units
    if(exists("IndUnit",COIN$Input$IndMeta)){
      # find unit for indicator
      indunit2 <- COIN$Input$IndMeta$IndUnit[COIN$Input$IndMeta$IndCode == inames[2]]
    } else {
      # if not, NULL
      indunit2 <- "score"
    }
  } else {
    # if not COIN, no units
    indunit2 <- "score"
  }

  # build data frame first, since the variables may come from different dfs
  # First, get the unit codes that the two inds have in common (some might have been excluded)
  UnitCodes2 <- intersect(out1$ind_data$UnitCode,out2$ind_data$UnitCode)
  UnitNames2 <- intersect(out1$ind_data$UnitName,out2$ind_data$UnitName)

  df <- data.frame(v1 = out1$ind_data_only[out1$UnitCodes %in% UnitCodes2,],
                   v2 = out2$ind_data_only[out1$UnitCodes %in% UnitCodes2,])
  colnames(df) <- c("v1", "v2")

  if (ptype == "Scatter"){

    # axis labels use units where possible
    xlab <- paste0(out1$IndNames, " <br> (", indunit1,")")
    ylab <- paste0(out2$IndNames, " <br> (", indunit2,")")

    fig <- plotly::plot_ly(data = df, type = 'scatter', mode = 'markers') %>%
      plotly::add_trace(
        x = ~v1,
        y = ~v2,
        text = UnitNames2,
        hoverinfo = 'text',
        marker = list(size = 15),
        showlegend = F
      ) %>%
      plotly::layout(xaxis = list(title = xlab),
                     yaxis = list(title = ylab))

  } else if (ptype == "Histogram"){

    fig <- plotly::plot_ly(df, alpha = 0.6)
    fig <- fig %>% plotly::add_histogram(x = ~v1, name = out1$IndNames)
    fig <- fig %>% plotly::add_histogram(x = ~v2, name = out2$IndNames)
    fig <- fig %>% plotly::layout(barmode = "overlay",
                                  xaxis = list(title = ""))
  }

  fig

}
