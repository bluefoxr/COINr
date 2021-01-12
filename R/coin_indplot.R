#' Plotting indicators in many ways
#'
#' Plots indicators
#'
#' @param COINobj The COIN object, or a data frame of indicator data
#' @param dset The source data set to use for indicator data (if input is COIN object)
#' @param inames A character vector of indicator names to plot. Defaults to all indicators.
#' @param type The type of plot. Currently supported "Box", "Dot", "Violin", "Violindot", "Histogram".
#' @param facetplot If TRUE, plots each indicator
#' @param ntype The type of nomalisation to apply. If NULL, no normalisation applied, otherwise specify
#' using labels in coin_normalise.
#' @param npara Optional parameters to pass to coin_normalise if normalisation required.
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_light geom_dotplot geom_violin geom_histogram labs facet_wrap
#'
#' @examples \dontrun{coin_indicatorplot(COINobj, type = "Box")}
#'
#' @return Nice plots
#'
#' @export

coin_indplot <- function(COINobj, dset = "Raw", inames = NULL, type = "Box", facetplot = TRUE,
                         ntype = NULL, npara = NULL){

  out1 <- coin_aux_objcheck(COINobj, dset, inames)
  ind_data_only <- out1$ind_data_only
  ind_names <- out1$ind_names

  # Normalise if required
  if (!is.null(ntype)){
    ind_data_only<-coin_normalise(ind_data_only, ntype, npara)
  }

  datamelt <- suppressMessages(reshape2::melt(ind_data_only)) # have to put dataframe in long format for ggplot

  if (type == "Box"){

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(y = .data$value))
    plt <- plt + ggplot2::geom_boxplot() + ggplot2::theme_light()

  } else if (type == "Dot"){

    # Note that this might be messy, and can be adusted with stackratio and dotsize

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$variable, y = .data$value))
    plt <- plt + ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1,
                                       stackratio=0.5, alpha = 0.3) + ggplot2::theme_light()

  } else if (type == "Violin"){

    # You might have to resize the window here to make it look OK

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$variable, y = .data$value))
    plt <- plt + ggplot2::geom_violin(scale = "area") + ggplot2::theme_light()

  } else if (type == "Violindot"){

    # You might have to resize the window here to make it look OK

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$variable, y = .data$value))
    plt <- plt + ggplot2::geom_violin(scale = "area") +
      ggplot2::geom_dotplot(binaxis = "y", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) +
      ggplot2::theme_light()

  } else if (type == "Histogram"){

    # You can adjust the bin width

    plt <- ggplot2::ggplot(data = datamelt, ggplot2::aes(x = .data$value))
    plt <- plt + ggplot2::geom_histogram(colour = "#e9ecef", bins = 10) + ggplot2::theme_light()

  }

  if (facetplot == FALSE){
    plt <- plt + ggplot2::labs(x = "Indicator", y = "Value") # add axis labels
  } else {
    nfrows <- ceiling(sqrt(nlevels(datamelt$variable))/2) # A way to get the number of rows so that we have about twice as many cols as rows
    plt <- plt + ggplot2::facet_wrap(~ variable, nrow = nfrows, scales="free") +
      ggplot2::labs(x = "", y = "")
  }

  plt

}
