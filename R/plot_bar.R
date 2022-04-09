
#' Bar chart
#'
#' Plot bar charts of single indicators, possibly coloured according to a grouping variable.
#'
#' @param coin A coin object.
#' @param dset Data set from which to extract the variable to plot. Passed to [get_data()].
#' @param iCode Code of variable or indicator to plot. Passed to [get_data()].
#' @param ... Further arguments to pass to [get_data()], e.g. for filtering units.
#' @param uLabel How to label units: either `"iCode"`, or `"iName"`.
#' @param axes_label How to label the y axis and group legend: either `"uCode"` or `"uName"`.
#' @param by_group Optional group variable to use to colour bars.
#' @param dset_label Logical: whether to include the data set in the y axis label.
#' @param log_scale Logical: if `TRUE` uses a log scale for the y axis.
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @examples
#' #
plot_bar <- function(coin, dset, iCode, ..., uLabel = "uCode", axes_label = "iCode",
                     by_group = NULL, dset_label = FALSE, log_scale = FALSE){

  # PREP --------------------------------------------------------------------

  stopifnot(is.character(dset),
            is.character(iCode),
            axes_label %in% c("iCode", "iName"))

  if(!is.null(uLabel)){
    stopifnot(is.character(uLabel),
              length(uLabel) == 1)
    if(uLabel %nin% c("uCode", "uName")){
      stop("uLabel must be either NULL, 'uCode', or 'uName'.")
    }
  }


  # GET DATA ----------------------------------------------------------------

  if(!is.null(by_group)){
    also_get <- by_group
  } else {
    also_get <- NULL
  }

  iData <- get_data(coin, dset = dset, iCodes = iCode, also_get = also_get, ...)

  if(is.null(uLabel) || (uLabel == "uCode") ){
    iData$plbs <- iData$uCode
  } else {
    iData$plbs <- codes2names(coin, iData$uCode)
  }

  # PLOT --------------------------------------------------------------------

  # setup: whether to plot by group or not
  if(!is.null(by_group)){
    plt <- ggplot2::ggplot(iData, ggplot2::aes(x = reorder(.data[["uCode"]], -.data[[iCode]]),
                                             y = .data[[iCode]],
                                             label = plbs,
                                             fill = .data[[by_group]]))
  } else {
    plt <- ggplot2::ggplot(iData, ggplot2::aes(x = reorder(.data[["uCode"]], -.data[[iCode]]),
                                             y = .data[[iCode]],
                                             label = plbs))
  }

  # main plot
  plt <-  plt +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal()


  # LABELS ------------------------------------------------------------------

  # names
  if(axes_label == "iName"){
    lbs <- codes2names(coin, c(iCode, by_group))
  } else {
    lbs <- c(iCode, by_group)
  }
  # dset
  if(dset_label){
    lbs[1] <- paste0(lbs[1], " (", dset, ")")
  }
  if(is.null(by_group)){
    plt <- plt + ggplot2::labs(
      x = ggplot2::element_blank(),
      y = lbs[1]
    )
  } else {
    plt <- plt + ggplot2::labs(
      x = ggplot2::element_blank(),
      y = lbs[1],
      fill = lbs[2]
    )
  }


  # AXES --------------------------------------------------------------------

  if(log_scale){
    plt <- plt + ggplot2::scale_y_log10()
  }


  plt

}
