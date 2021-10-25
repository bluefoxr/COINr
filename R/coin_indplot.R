#' Static indicator distribution plots
#'
#' Plots indicator distributions using box plots, dot plots, violin plots, violin-dot plots, and histograms.
#' Supports plotting multiple indicators by calling aggregation groups.
#'
#' This function also optionally normalises indicators so they can be compared more easily side by side. For this
#' purpose it calls [normalise()] - see `ntype` and `npara` arguments.
#'
#' See [COINr online documentation](https://bluefoxr.github.io/COINrDoc/initial-visualisation-and-analysis.html#distributions) and [getIn()] for more information on accessing/plotting groups.
#'
#' @param COINobj The COIN object, or a data frame of indicator data
#' @param dset The source data set to use for indicator data (if input is COIN object)
#' @param icodes A character vector of indicator names to plot. Defaults to all indicators.
#' @param aglev The aggregation level to extract the indicator data from. Defaults to indicator level (1).
#' @param type The type of plot. Currently supported `"Box"`, `"Dot"`, `"Violin"`, `"Violindot"`, `"Histogram"`.
#' @param ntype The type of normalisation to apply. If `NULL`, no normalisation applied, otherwise specify
#' using `ntype` options in [normalise()].
#' @param npara Optional parameters to pass to [normalise()] if normalisation required.
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_light geom_dotplot geom_violin geom_histogram labs facet_wrap
#'
#' @examples
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # plot indicators in Physical pillar
#' plotIndDist(ASEM, type = "Box", dset = "Raw", icodes = "Physical")
#'
#' @return Plots generated with **ggplot2**. These can be edited further with **ggplot2** commands.
#'
#' @export

plotIndDist <- function(COINobj, dset = "Raw", icodes = NULL, aglev = 1, type = "Box",
                         ntype = NULL, npara = NULL){

  out1 <- getIn(COINobj, dset = dset, icodes = icodes, aglev = aglev)
  ind_data_only <- out1$ind_data_only
  ind_names <- out1$IndCodes

  # Normalise if required
  if (!is.null(ntype)){
    ind_data_only<-normalise(ind_data_only, ntype, npara)
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

  } else {
    stop("Plot type not recognised.")
  }

  # If plotting single indicator, use long name, otherwise use codes and facet plotting
  if (length(out1$IndCodes) == 1){
    plt <- plt + ggplot2::labs(x = out1$IndNames[1]) # add axis labels
  } else {
    nfrows <- ceiling(sqrt(nlevels(datamelt$variable))/2) # A way to get the number of rows so that we have about twice as many cols as rows
    plt <- plt + ggplot2::facet_wrap(~ variable, nrow = nfrows, scales="free") +
      ggplot2::labs(x = "", y = "")
  }

  plt

}


#' Dot plots of single indicator with highlighting
#'
#' Plots a single indicator as a line of dots, and optionally highlights a selected unit.
#'
#' @param COIN The COIN
#' @param dset The source data set to use for indicator data
#' @param icode An indicator code to plot.
#' @param marker_type The type of marker, either `"circle"` (default) or `"cross"`, or a marker number to pass to ggplot2 (0-25).
#' @param usel A unit or set of units (as a string or character vector) to highlight.
#' @param add_stat A statistic to overlay, either `"mean"`, `"median"` or else a specified value.
#' @param plabel Controls the labelling of the indicator. If not specified, returns the indicator name,
#' plus units if found. Otherwise if `"indname"`, returns only indicator name, if `"indname+unit"`, returns
#' indicator name plus unit (if found), if `"unit"` returns only unit (if found), otherwise if `"none"`,
#' displays no text. Finally, any other string can be passed, so e.g. `"My indicator"` will display this on the
#' axis.
#' @param usel_label If `TRUE` (default) also labels selected units with their unit codes. `FALSE` to disable.
#'
#' @importFrom ggplot2 ggplot aes theme_minimal ylab geom_point theme element_blank
#'
#' @examples
#' # add
#'
#' @return Plots generated with **ggplot2**. These can be edited further with **ggplot2** commands.
#'
#' @export

plotIndDot <- function(COIN, dset = NULL, icode = NULL, usel = NULL,
                       marker_type = "circle", add_stat = NULL, show_ticks = TRUE,
                       plabel = NULL, usel_code = TRUE){

  # some discarded stuff from the "orient" option which doesn't work very well
  # @param orient If `"horizontal"` (default), displays chart horizontally, otherwise if `"vertical"`, vertically.
  # orient = "horizontal",


  if(!is.coin(COIN)){
    stop("This function currently requires a COIN as input.")
  }
  stopifnot(!is.null(icode),
            is.character(icode),
            length(icode) == 1)

  out1 <- getIn(COIN, dset = dset, icodes = icode)

  ind_data <- cbind(y = 1, out1$ind_data_only)
  colnames(ind_data) <- c("y", "x")

  if(marker_type=="circle"){
    mno <- 21
  } else if (marker_type == "cross"){
    mno <- 3
  } else {
    mno <- marker_type
  }

  plt <- ggplot2::ggplot(ind_data, ggplot2::aes(x=x, y=y)) +
    ggplot2::theme_minimal() +
    ggplot2::geom_point(
      color="transparent",
      fill="blue",
      shape=mno,
      alpha=0.5,
      size=3,
      #stroke = 0
      ) +
    ggplot2::ylab(NULL) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())

  if(!show_ticks){
    plt <- plt +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
  }

  if(!is.null(usel)){
    # get df of just selected units
    if(!("UnitCode" %in% colnames(out1$ind_data))){
      stop("UnitCode column not found in data.")
    }
    # select indicator plus unit code col
    ind_data_wcodes <- out1$ind_data[c("UnitCode", out1$IndCodes)]
    # filter to selected units

    udfi <- ind_data_wcodes[ind_data_wcodes$UnitCode %in% usel,]
    # check sth is left
    if(nrow(udfi) == 0){
      stop("None of the specified usel found in indicator data.")
    }
    # make into df ready for ggplot
    udf <- data.frame(y = 1, udfi[[out1$IndCodes]])
    colnames(udf) <- c("y", "x")

    # overlay on plot
    plt <- plt + ggplot2::geom_point(
      data = udf,
      ggplot2::aes(x=x, y=y),
      color="red",
      fill="blue",
      shape=21,
      alpha=0.7,
      size=3,
      stroke = 2
    )

    if(usel_code){
      # add text labels
      plt <- plt +
        ggplot2::annotate("text", x = udf$x, y = 1.008, label = udfi$UnitCode,
                          angle = 45, hjust = 0.3, size = 3.5)
    }

  }

  if(!is.null(add_stat)){
    if(add_stat == "mean"){
      stat_val <- mean(unlist(out1$ind_data_only), na.rm = TRUE)
    } else if (add_stat == "median"){
      stat_val <- stats::median(unlist(out1$ind_data_only), na.rm = TRUE)
    } else if (is.numeric(add_stat)){
      stat_val <- add_stat
    } else {
      stop("add_stat not recognised. Should be 'mean', 'median', or a number.")
    }

    #plt <- plt + ggplot2::geom_vline(xintercept = stat_val)
    plt <- plt + ggplot2::annotate(
      "segment", x = stat_val, y= 0.99,
      xend = stat_val, yend = 1.01,
      alpha = 0.5, size = 2, colour = "#8B8000")
  }

  if(is.null(plabel)){

    plabel <- out1$IndNames

    if(exists("IndUnit", ASEM$Input$IndMeta)){
      plabel <- paste0(plabel, " (", ASEM$Input$IndMeta$IndUnit[ASEM$Input$IndMeta$IndCode == icode], ")")
    }
  } else if  (plabel == "none"){
    plabel <- NULL
  } else if (plabel == "indname"){
    plabel <- out1$IndNames
  } else if (plabel == "indname+unit"){
    plabel <- out1$IndNames
    if(exists("IndUnit", ASEM$Input$IndMeta)){
      plabel <- paste0(plabel, " (", ASEM$Input$IndMeta$IndUnit[ASEM$Input$IndMeta$IndCode == icode], ")")
    }
  } else if (plabel == "unit"){
    if(exists("IndUnit", ASEM$Input$IndMeta)){
      plabel <- paste0(plabel, " (", ASEM$Input$IndMeta$IndUnit[ASEM$Input$IndMeta$IndCode == icode], ")")
    } else {
      plabel <- NULL
    }
  }
  plt <- plt + ggplot2::xlab(plabel)

  # if(orient == "vertical"){
  #   plt <- plt + ggplot2::coord_flip()
  # }

  plt + ggplot2::ylim(c(0.98, 1.02))


}
