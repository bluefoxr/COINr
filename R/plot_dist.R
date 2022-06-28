#' Static indicator distribution plots
#'
#' Plots indicator distributions using box plots, dot plots, violin plots, violin-dot plots, and histograms.
#' Supports plotting multiple indicators by calling aggregation groups.
#'
#' This function uses ggplot2 to generate plots, so the plot can be further manipulated using ggplot2 commands.
#' See `vignette("visualisation`) for more details on plotting.
#'
#' @param coin The coin object, or a data frame of indicator data
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param iCodes Indicator code(s) to plot. See details.
#' @param ... Further arguments passed to [get_data()] (other than `coin`, `dset` and `iCodes`).
#' @param normalise Logical: if `TRUE`, normalises the data first, using `global_specs`. If `FALSE` (default),
#' data is not normalised.
#' @param global_specs Specifications for normalising data if `normalise = TRUE`. This is passed to the
#' `global_specs` argument of [Normalise()].
#' @param type The type of plot. Currently supported `"Box"`, `"Dot"`, `"Violin"`, `"Violindot"`, `"Histogram"`.
#'
#' @importFrom utils stack
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_light geom_dotplot geom_violin geom_histogram labs facet_wrap labs
#' @importFrom rlang .data
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin")
#'
#' # plot all indicators in P2P group
#' plot_dist(coin, dset = "Raw", iCodes = "P2P", Level = 1, type = "Violindot")
#'
#' @return A ggplot2 plot object.
#'
#' @export
plot_dist <- function(coin, dset, iCodes, ..., type = "Box", normalise = FALSE,
                      global_specs = NULL){

  # GET DATA ----------------------------------------------------------------

  # get data set
  iData <- get_data(coin, dset = dset, iCodes = iCodes, ...)

  # col names that are NOT indicators
  not_iCodes <- names(iData)[names(iData) %in% names(coin$Meta$Unit)]

  # only indicator data
  iData_ <- iData[colnames(iData) %nin% not_iCodes]

  # Normalise if required
  if (normalise){
    iData_ <- Normalise(iData_, global_specs = global_specs)
  }

  # have to put dataframe in long format for ggplot
  datamelt <- utils::stack(iData_)
  # remove NAs to avoid warnings from ggplot2
  datamelt <- datamelt[!is.na(datamelt$values), ]

  # PLOT --------------------------------------------------------------------

  # the base
  plt <- ggplot2::ggplot(data = datamelt)

  if (type == "Box"){

    plt <- plt + ggplot2::geom_boxplot(aes(y = .data$values)) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())

  } else if (type == "Dot"){

    # Note that this might be messy, and can be adusted with stackratio and dotsize
    plt <- plt + ggplot2::geom_dotplot(aes(x = .data$ind, y = .data$values),
                                       binaxis = "y", stackdir = "center", dotsize=1,
                                       stackratio=0.5, alpha = 0.3) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())

  } else if (type == "Violin"){

    plt <- plt + ggplot2::geom_violin(ggplot2::aes(x = .data$ind, y = .data$values),
                                      scale = "area") +
      ggplot2::theme_light() +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())

  } else if (type == "Violindot"){

    plt <- plt + ggplot2::geom_violin(ggplot2::aes(x = .data$ind, y = .data$values),
                                      scale = "area") +
      ggplot2::geom_dotplot(ggplot2::aes(x = .data$ind, y = .data$values),
                            binaxis = "y", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) +
      ggplot2::theme_light()+ ggplot2::theme_light()+
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  } else if (type == "Histogram"){

    plt <- plt + ggplot2::geom_histogram(ggplot2::aes(x = .data$values),
                                         colour = "#e9ecef", bins = 10) +
      ggplot2::theme_light()

  } else {
    stop("Plot type not recognised.")
  }

  # If plotting multiple indicators, use facet plotting
  if (ncol(iData_) > 1){
    nfrows <- ceiling(sqrt(nlevels(datamelt$ind))/2) # A way to get the number of rows so that we have about twice as many cols as rows
    plt <- plt + ggplot2::facet_wrap(~ ind, nrow = nfrows, scales="free") +
      ggplot2::labs(x = "", y = "")
  } else {
    # otherwise, add a title
    plt <- plt + ggplot2::labs(title = names(iData_))
  }

  plt

}


#' Dot plots of single indicator with highlighting
#'
#' Plots a single indicator as a line of dots, and optionally highlights selected units and statistics.
#' This is intended for showing the relative position of units to other units, rather than as a statistical
#' plot. For the latter, use [plot_dist()].
#'
#' This function uses ggplot2 to generate plots, so the plot can be further manipulated using ggplot2 commands.
#' See `vignette("visualisation`) for more details on plotting.
#'
#' @param coin The coin
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#' @param iCode Code of indicator or aggregate found in `dset`. Required to be of length 1.
#' @param Level The level in the hierarchy to extract data from. See [get_data()].
#' @param ... Further arguments to pass to [get_data()], other than those explicitly specified here.
#' @param marker_type The type of marker, either `"circle"` (default) or `"cross"`, or a marker number to pass to ggplot2 (0-25).
#' @param usel A subset of units to highlight.
#' @param add_stat A statistic to overlay, either `"mean"`, `"median"` or else a specified value.
#' @param stat_label An optional string to use as label at the point specified by `add_stat`.
#' @param show_ticks Set `FALSE` to remove axis ticks.
#' @param plabel Controls the labelling of the indicator. If `NULL` (default), returns the indicator code.
#' Otherwise if `"iName"`, returns only indicator name, if `"iName+unit"`, returns
#' indicator name plus unit (if found), if `"unit"` returns only unit (if found), otherwise if `"none"`,
#' displays no text. Finally, any other string can be passed, so e.g. `"My indicator"` will display this on the
#' axis.
#' @param usel_label If `TRUE` (default) also labels selected units with their unit codes. `FALSE` to disable.
#' @param vert_adjust Adjusts the vertical height of text labels and stat lines, which matters depending on plot size.
#' Takes a value between 0 to 2 (higher will probably remove the label from the axis space).
#'
#' @importFrom ggplot2 ggplot aes theme_minimal ylab geom_point theme element_blank
#' @importFrom rlang .data
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin")
#'
#' # dot plot of LPI, highlighting two countries and with median shown
#' plot_dot(coin, dset = "Raw", iCode = "LPI", usel = c("JPN", "ESP"),
#'          add_stat = "median", stat_label = "Median", plabel = "iName+unit")
#'
#' @return A ggplot2 plot object.
#'
#' @export
plot_dot <- function(coin, dset, iCode, Level = NULL, ..., usel = NULL, marker_type = "circle",
                     add_stat = NULL, stat_label = NULL, show_ticks = TRUE, plabel = NULL,
                     usel_label = TRUE, vert_adjust = 0.5){

  # GET DATA ----------------------------------------------------------------

  iData <- get_data(coin, dset = dset, iCodes = iCode, Level = Level)

  iData_ <- extract_iData(coin, iData, "iData_")
  if(ncol(iData_) != 1){
    stop("More than one indicator selected. This plot requires selection of a single indicator.")
  }

  ind_data <- cbind(y = 1, iData_)
  colnames(ind_data) <- c("y", "x")


  # BASE PLOT ---------------------------------------------------------------

  if(marker_type=="circle"){
    mno <- 21
  } else if (marker_type == "cross"){
    mno <- 3
  } else {
    mno <- marker_type
  }

  plt <- ggplot2::ggplot(ind_data, ggplot2::aes(x=.data$x, y=.data$y)) +
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

  # TICKS -------------------------------------------------------------------

  if(!show_ticks){
    plt <- plt +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
  }


  # HIGHLIGHT UNITS ---------------------------------------------------------

  if(!is.null(usel)){

    # select indicator plus unit code col
    ind_data_wcodes <- iData[c("uCode", colnames(iData_))]

    # filter to selected units
    udfi <- ind_data_wcodes[ind_data_wcodes$uCode %in% usel,]
    # check sth is left
    if(nrow(udfi) == 0){
      stop("None of the specified usel found in indicator data.")
    }
    # make into df ready for ggplot
    udf <- data.frame(y = 1, udfi[[names(iData_)]])
    colnames(udf) <- c("y", "x")

    # overlay on plot
    plt <- plt + ggplot2::geom_point(
      data = udf,
      ggplot2::aes(x=.data$x, y=.data$y),
      color="red",
      fill="blue",
      shape=21,
      alpha=0.7,
      size=3,
      stroke = 2
    )

    if(usel_label){
      # add text labels
      plt <- plt +
        ggplot2::annotate("text", x = udf$x, y = 1 + vert_adjust/100, label = udfi$uCode,
                          angle = 45, hjust = 0.3, size = 3.5)
    }
  }

  # STATS -------------------------------------------------------------------

  if(!is.null(add_stat)){
    if(add_stat == "mean"){
      stat_val <- mean(unlist(iData_), na.rm = TRUE)
    } else if (add_stat == "median"){
      stat_val <- stats::median(unlist(iData_), na.rm = TRUE)
    } else if (is.numeric(add_stat)){
      stat_val <- add_stat
    } else {
      stop("add_stat not recognised. Should be 'mean', 'median', or a number.")
    }

    plt <- plt + ggplot2::annotate(
      "segment", x = stat_val, y= 1 - vert_adjust/80,
      xend = stat_val, yend = 1 + vert_adjust/80,
      alpha = 0.5, size = 2, colour = "#3CB371")

    if(!is.null(stat_label)){
      # add text labels
      plt <- plt +
        ggplot2::annotate("text", x = stat_val, y = 1 + vert_adjust/60, label = stat_label,
                          angle = 45, hjust = 0.2, size = 3.5)
    }
  }

  # AXIS LABEL --------------------------------------------------------------

  if(is.null(plabel)){

    # just iCode
    plabel <- names(iData_)

  } else if  (plabel == "none"){

    # nothing
    plabel <- NULL

  } else if (plabel == "iName"){

    plabel <- get_names(coin, iCodes = names(iData_))

  } else if (plabel == "iName+unit"){

    plabel <- paste0(get_names(coin, iCodes = names(iData_)), " (",
                     get_units(coin, names(iData_)), ")")

  } else if (plabel == "unit"){

    plabel <- get_units(coin, names(iData_))

  }

  plt <- plt + ggplot2::xlab(plabel)


  # OUTPUT ------------------------------------------------------------------

  plt + ggplot2::ylim(c(0.98, 1.02))

}
