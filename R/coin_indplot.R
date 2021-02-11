#' Static indicator distribution plots
#'
#' Plots indicator distributions using box plots, dot plots, violin plots, violin-dot plots, and histograms.
#' Supports plotting multiple indicators by calling aggregation groups.
#'
#' @param COINobj The COIN object, or a data frame of indicator data
#' @param dset The source data set to use for indicator data (if input is COIN object)
#' @param inames A character vector of indicator names to plot. Defaults to all indicators.
#' @param aglev The aggregation level to extract the indicator data from. Defaults to indicator level (1)
#' @param type The type of plot. Currently supported "Box", "Dot", "Violin", "Violindot", "Histogram".
#' @param ntype The type of nomalisation to apply. If NULL, no normalisation applied, otherwise specify
#' using labels in coin_normalise.
#' @param npara Optional parameters to pass to coin_normalise if normalisation required.
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_light geom_dotplot geom_violin geom_histogram labs facet_wrap
#'
#' @examples \dontrun{plotIndDist(ASEM, type = "Box", inames = "Physical")}
#'
#' @return Nice plots
#'
#' @export

plotIndDist <- function(COINobj, dset = "Raw", inames = NULL, aglev = 1, type = "Box",
                         ntype = NULL, npara = NULL){

  out1 <- getIn(COINobj, dset = dset, inames = inames, aglev = aglev)
  ind_data_only <- out1$ind_data_only
  ind_names <- out1$IndCodes

  # Normalise if required
  if (!is.null(ntype)){
    ind_data_only<-coin_normalise(ind_data_only, ntype, npara)
  }

  datamelt <- suppressMessages(reshape2::melt(ind_data_only)) # have to put dataframe in long format for ggplot

  if (type == "Box"){

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(y = .data$value))
    plt <- plt + ggplot2::geom_boxplot() + ggplot2::theme_light() +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  } else if (type == "Dot"){

    # Note that this might be messy, and can be adusted with stackratio and dotsize

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$variable, y = .data$value))
    plt <- plt + ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1,
                                       stackratio=0.5, alpha = 0.3) + ggplot2::theme_light()+
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  } else if (type == "Violin"){

    # You might have to resize the window here to make it look OK

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$variable, y = .data$value))
    plt <- plt + ggplot2::geom_violin(scale = "area") + ggplot2::theme_light()+ ggplot2::theme_light()+
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  } else if (type == "Violindot"){

    # You might have to resize the window here to make it look OK

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$variable, y = .data$value))
    plt <- plt + ggplot2::geom_violin(scale = "area") +
      ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) +
      ggplot2::theme_light()+ ggplot2::theme_light()+
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  } else if (type == "Histogram"){

    # You can adjust the bin width

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$value))
    plt <- plt + ggplot2::geom_histogram(colour = "#e9ecef", bins = 10) + ggplot2::theme_light()

  }

  # If plotting single indicator, use long name, otherwise use codes and facet plotting
  if (length(out1$IndCodes) == 1){
    plt <- plt + ggplot2::labs(x = out1$IndNames[1], y = "Frequency") # add axis labels
  } else {
    nfrows <- ceiling(sqrt(nlevels(datamelt$variable))/2) # A way to get the number of rows so that we have about twice as many cols as rows
    plt <- plt + ggplot2::facet_wrap(~ variable, nrow = nfrows, scales="free") +
      ggplot2::labs(x = "", y = "")
  }

  plt

}

#' Interactive violin plot
#'
#' Generates a javascript violin plot of a single indicator, using Plotly. Plot can be embedded e.g. in
#' HTML documents, websites, etc, or used for more interactive data exploration.
#'
#' @param COINobj The COIN object, or a data frame of indicator data
#' @param dset The source data set to use for indicator data (if input is COIN object)
#' @param inames A character vector of indicator names to plot. Defaults to all indicators.
#' @param aglev The aggregation level to extract the indicator data from. Defaults to indicator level (1)
#' @param type The type of plot. Currently supported "Box", "Dot", "Violin", "Violindot", "Histogram".
#' @param ntype The type of nomalisation to apply. If NULL, no normalisation applied, otherwise specify
#' using labels in coin_normalise.
#' @param npara Optional parameters to pass to coin_normalise if normalisation required.
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_light geom_dotplot geom_violin geom_histogram labs facet_wrap
#'
#' @examples \dontrun{plotIndDist(ASEM, type = "Box", inames = "Physical")}
#'
#' @return Nice plots
#'
#' @export

iplotViolin <- function(COINobj, dset = "Raw", inames = NULL, aglev = 1){

  out1 <- getIn(COINobj, dset = dset, inames = inames, aglev = aglev)
  ind_data_only <- out1$ind_data_only
  ind_names <- out1$IndNames
  ind_code <- out1$IndCodes

  if(length(ind_names)>1){stop("This function only supports plotting single indicators.")}

  fig <- plotly::plot_ly(data =ind_data_only, y = ~get(ind_code), type = 'violin',
                 box = list(visible = T),
                 meanline = list(visible = T),
                 x0 = ind_names,
                 points = 'all',
                 pointpos = -1.5,
                 jitter = 0.1,
                 hoveron = "violins+points+kde"
  )
  fig <- fig %>% plotly::layout( yaxis = list(title = "", zeroline = F) )

  fig

  # # match axes with other variable if requested
  # if (input$axmatch==TRUE){
  #   fig <- fig %>% plotly::layout( yaxis = list(title = "", zeroline = F, range = axlims()) )
  # } else {
  #   fig <- fig %>% plotly::layout( yaxis = list(title = "", zeroline = F) )
  # }
  # fig

}
