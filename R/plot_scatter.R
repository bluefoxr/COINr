

#' Scatter plot of two variables
#'
#' This is a convenient quick scatter plot function for plotting any two variables x and y in a coin against each other.
#' At a minimum, you must specify the data set and iCode of both x and y using the `dsets` and `iCodes` arguments.
#'
#' Optionally, the scatter plot can be coloured by grouping variables specified in the coin (see `by_group`). Points
#' and axes can be labelled using other arguments.
#'
#' This function is powered by ggplot2 and outputs a ggplot2 object. To further customise the plot, assign the output
#' of this function to a variable and use ggplot2 commands to further edit.
#'
#' @param coin A coin object
#' @param dsets A 2-length character vector specifying the data sets to extract v1 and v2 from,
#' respectively (passed as `dset` argument to [get_data()]. Alternatively specify as a single string
#' which will be used for both x and y.
#' @param iCodes A 2-length character vector specifying the `iCodes` to use as v1 and v2,
#' respectively (passed as `iCodes` argument to [get_data()]. Alternatively specify as a single string
#' which will be used for both x and y.
#' @param ... Optional further arguments to be passed to [get_data()], e.g. to specify which `uCode`s to plot.
#' @param by_group A string specifying an optional group variable. If specified, the plot will be
#' coloured by this grouping variable.
#' @param alpha Transparency value for points between 0 and 1, passed to ggplot2.
#' @param axes_label A string specifying how to label axes and legend. Either `"iCode"` to use the respective codes
#' of each variable, or else `"iName"` to use the names (as specified in `iMeta`).
#' @param dset_label Logical: if `TRUE` (default), also adds to the axis labels which data set each variable is from.
#' @param point_label Specifies whether and how to label points. If `"uCode"`, points are labelled with their unit codes,
#' else if `"uName"`, points are labelled with their unit names. Set `NULL` to remove labels (default).
#' @param check_overlap Logical: if `TRUE` (default), point labels that overlap are removed - this results in a legible
#' plot but some labels may be missing. Else if `FALSE`, all labels are plotted.
#' @param log_scale A 2-length logical vector specifying whether to use log axes for x and y respectively: if `TRUE`,
#' a log axis will be used. Defaults to not-log.
#'
#' @importFrom rlang .data
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin")
#'
#' # scatter plot of Flights against Population
#' # coloured by GDP per capita
#' # log scale applied to population
#' plot_scatter(coin, dsets = c("uMeta", "Raw"),
#'              iCodes = c("Population", "Flights"),
#'              by_group = "GDPpc_group", log_scale = c(TRUE, FALSE))
#'
#'
plot_scatter <- function(coin, dsets, iCodes, ..., by_group = NULL,
                         alpha = 0.5, axes_label = "iCode", dset_label = TRUE,
                         point_label = NULL, check_overlap = TRUE, log_scale = c(FALSE, FALSE)){

  # PREP --------------------------------------------------------------------

  stopifnot(is.character(dsets),
            is.character(iCodes),
            length(dsets) %in% c(1,2),
            length(iCodes) %in% c(1,2),
            axes_label %in% c("iCode", "iName"),
            is.logical(log_scale),
            length(log_scale) == 2)

  if(length(dsets) == 1){
    dsets <- rep(dsets, 2)
  }
  if(length(iCodes) == 1){
    iCodes <- rep(iCodes, 2)
  }

  if(!is.null(point_label)){
    stopifnot(is.character(point_label),
              length(point_label) == 1)
    if(point_label %nin% c("uCode", "uName")){
      stop("point_label must be either NULL, 'uCode', or 'uName'.")
    }
  }


  # GET DATA ----------------------------------------------------------------

  if(!is.null(by_group)){
    also_get <- by_group
  } else {
    also_get <- NULL
  }

  x1 <- get_data(coin, dset = dsets[1], iCodes = iCodes[1], also_get = also_get, ...)
  x2 <- get_data(coin, dset = dsets[2], iCodes = iCodes[2], also_get = also_get, ...)

  x12 <- merge(x1, x2, by = c("uCode", also_get), all = FALSE)

  # if we have the same indicator plotted against itself, have to rename
  iCodes_orig <- iCodes
  if(iCodes[1] == iCodes[2]){
    iCodes[1] <- names(x12)[2]
    iCodes[2] <- names(x12)[3]
  }

  if(is.null(point_label) || (point_label == "uCode") ){
    x12$plbs <- x12$uCode
  } else {
    x12$plbs <- codes2names(coin, x12$uCode)
  }

  # PLOT --------------------------------------------------------------------

  # setup: whether to plot by group or not
  if(!is.null(by_group)){
    plt <- ggplot2::ggplot(x12, ggplot2::aes(x = .data[[iCodes[1]]],
                                             y = .data[[iCodes[2]]],
                                             label = .data$plbs,
                                             colour = .data[[by_group]]))
  } else {
    plt <- ggplot2::ggplot(x12, ggplot2::aes(x = .data[[iCodes[1]]],
                                             y = .data[[iCodes[2]]],
                                             label = .data$plbs))
  }

  # main plot
  plt <-  plt +
    ggplot2::geom_point(alpha = alpha) +
    ggplot2::theme_minimal()

  # LABELS ------------------------------------------------------------------

  # names
  if(axes_label == "iName"){
    lbs <- codes2names(coin, c(iCodes_orig, by_group))
  } else {
    lbs <- c(iCodes_orig, by_group)
  }
  # dset
  if(dset_label){
    lbs[1] <- paste0(lbs[1], " (", dsets[1], ")")
    lbs[2] <- paste0(lbs[2], " (", dsets[2], ")")
  }
  if(is.null(by_group)){
    plt <- plt + ggplot2::labs(
      x = lbs[1],
      y = lbs[2]
    )
  } else {
    plt <- plt + ggplot2::labs(
      x = lbs[1],
      y = lbs[2],
      colour = lbs[3]
    )
  }


  # AXES --------------------------------------------------------------------

  if(log_scale[1]){
    plt <- plt + ggplot2::scale_x_log10()
  }
  if(log_scale[2]){
    plt <- plt + ggplot2::scale_y_log10()
  }


  # POINT LABELS ------------------------------------------------------------

  if(!is.null(point_label)){

    plt <- plt + ggplot2::geom_text(size = 3,
                                    vjust = 0, nudge_y = 5,
                                    check_overlap = check_overlap)
  }


  plt

}
