
#' Bar chart
#'
#' Plot bar charts of single indicators. Bar charts can be coloured by an optional grouping variable `by_group`, or if
#' `iCode` points to an aggregate, setting `stack_children = TRUE` will plot `iCode` coloured by its underlying scores.
#'
#' This function uses ggplot2 to generate plots, so the plot can be further manipulated using ggplot2 commands.
#' See `vignette("visualisation`) for more details on plotting.
#'
#' @param coin A coin object.
#' @param dset Data set from which to extract the variable to plot. Passed to [get_data()].
#' @param iCode Code of variable or indicator to plot. Passed to [get_data()].
#' @param ... Further arguments to pass to [get_data()], e.g. for filtering units.
#' @param uLabel How to label units: either `"uCode"`, or `"uName"`.
#' @param axes_label How to label the y axis and group legend: either `"iCode"` or `"iName"`.
#' @param by_group Optional group variable to use to colour bars. Cannot be used if `stack_children = TRUE`.
#' @param dset_label Logical: whether to include the data set in the y axis label.
#' @param log_scale Logical: if `TRUE` uses a log scale for the y axis.
#' @param stack_children Logical: if `TRUE` and `iCode` refers to an aggregate, will plot `iCode` with each bar split into
#' its underlying component values (the underlying indicators/aggregates used to create `iCode`). To use this, you must
#' have aggregated your data and `dset` must point to a data set where the underlying (child) scores of `iCode` are available.
#' @param bar_colours Optional vector of colour codes for colouring bars.
#'
#' @importFrom stats reorder
#' @importFrom rlang .data
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # bar plot of CO2 by GDP per capita group
#' plot_bar(coin, dset = "Raw", iCode = "CO2",
#'          by_group = "GDPpc_group", axes_label = "iName")
plot_bar <- function(coin, dset, iCode, ..., uLabel = "uCode", axes_label = "iCode",
                     by_group = NULL, dset_label = FALSE, log_scale = FALSE, stack_children = FALSE,
                     bar_colours = NULL){

  # PREP --------------------------------------------------------------------

  stopifnot(is.character(dset),
            is.character(iCode),
            length(iCode) == 1,
            axes_label %in% c("iCode", "iName"),
            is.logical(dset_label),
            is.logical(log_scale),
            is.logical(stack_children))

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

  # uLABELS -----------------------------------------------------------------

  if(is.null(uLabel) || (uLabel == "uCode") ){
    iData$plbs <- iData$uCode
  } else {
    iData$plbs <- codes2names(coin, iData$uCode)
  }

  # GET children -------------------------------------------------------------
  # if stack_children = TRUE, we need to get iCode plus underlying codes

  if(stack_children){

    if(!is.null(by_group)){
      stop("Cannot have stack_children = TRUE and plotting by group (by_group). Disable one of these two options.")
    }

    # get iMeta
    iMeta <- coin$Meta$Ind
    # get child codes
    iCodes_ch <- iMeta$iCode[iMeta$Parent == iCode]
    # remove NAs
    iCodes_ch <- iCodes_ch[!is.na(iCodes_ch)]
    # check
    if(length(iCodes_ch) == 0){
      stop("No child codes found for selected iCode: if stack_children = TRUE, you must select an iCode in Level 2
           or above (it must be an aggregate).")
    }

    # get data
    iData_ch <- get_data(coin, dset = dset, iCodes = iCodes_ch, also_get = also_get, ...)

    # merge onto iData
    iData <- merge(iData, iData_ch, by = "uCode")

    # scale children to add up to parent score
    iData$scale_fac <- iData[[iCode]]/rowSums(iData[iCodes_ch])
    iData[iCodes_ch] <- sapply(iData[iCodes_ch], `*`, iData$scale_fac)

    # make long for plotting, and rename some things
    iData <- lengthen(iData, cols = iCodes_ch)
    names(iData)[names(iData) == "name"] <- "Component"
    names(iData)[names(iData) == iCode] <- paste0(iCode, "2")
    names(iData)[names(iData) == "Value"] <- iCode

  }

  # PLOT --------------------------------------------------------------------

  # setup: whether to plot by group or not
  if(!is.null(by_group)){
    plt <- ggplot2::ggplot(iData, ggplot2::aes(x = stats::reorder(.data[["plbs"]], -.data[[iCode]]),
                                             y = .data[[iCode]],
                                             label = .data[["plbs"]],
                                             fill = .data[[by_group]]))
  } else if(stack_children){
    plt <- ggplot2::ggplot(iData, ggplot2::aes(x = stats::reorder(.data[["plbs"]], -.data[[iCode]]),
                                               y = .data[[iCode]],
                                               label = .data[["plbs"]],
                                               fill = .data[["Component"]]))
  } else {
    plt <- ggplot2::ggplot(iData, ggplot2::aes(x = stats::reorder(.data[["plbs"]], -.data[[iCode]]),
                                             y = .data[[iCode]],
                                             label = .data[["plbs"]]))
  }

  if(stack_children){

    # main plot
    plt <-  plt +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::theme_minimal()

  } else {

    # main plot
    plt <-  plt +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::theme_minimal()

  }

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


  # COLOURS -----------------------------------------------------------------

  if(!is.null(bar_colours)){
    plt <- plt + ggplot2::scale_fill_manual(values = bar_colours)
  }

  # AXES --------------------------------------------------------------------

  if(log_scale){
    plt <- plt + ggplot2::scale_y_log10()
  }

  plt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::theme(text=ggplot2::element_text(family="sans"))

}
