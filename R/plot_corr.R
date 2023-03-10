#' Static heatmaps of correlation matrices
#'
#' Generates heatmaps of correlation matrices using ggplot2, which can be tailored according to the grouping and structure
#' of the index. This enables correlating any set of indicators against any other,
#' and supports calling named aggregation groups of indicators. The `withparent` argument generates tables of correlations only with
#' parents of each indicator. Also supports discrete colour maps using `flagcolours`, different types of correlation, and groups
#' plots by higher aggregation levels.
#'
#' This function calls [get_corr()].
#'
#' Note that this function can only call correlations within the same data set (i.e. only one data set in `.$Data`).
#'
#' This function uses ggplot2 to generate plots, so the plot can be further manipulated using ggplot2 commands.
#' See `vignette("visualisation")` for more details on plotting.
#'
#' This function replaces the now-defunct `plotCorr()` from COINr < v1.0.
#'
#' @param coin The coin object
#' @param dset The target data set.
#' @param iCodes An optional list of character vectors where the first entry specifies the indicator/aggregate
#' codes to correlate against the second entry (also a specification of indicator/aggregate codes)
#' @param Levels The aggregation levels to take the two groups of indicators from. See [get_data()] for details.
#' @param ... Optional further arguments to pass to [get_data()].
#' @param cortype The type of correlation to calculate, either `"pearson"`, `"spearman"`, or `"kendall"` (see [stats::cor()]).
#' @param withparent If `aglev[1] != aglev[2]`, and equal `TRUE` will only plot correlations of each row with its parent.
#' If `"family"`, plots the lowest aggregation level in `Levels` against all its parent levels.
#' If `FALSE` plots the full correlation matrix (default).
#' @param grouplev The aggregation level to group correlations by if `aglev[1] == aglev[2]`. By default, groups correlations into the
#' aggregation level above. Set to 0 to disable grouping and plot the full matrix.
#' @param box_level The aggregation level to draw boxes around if `aglev[1] == aglev[2]`.
#' @param showvals If `TRUE`, shows correlation values. If `FALSE`, no values shown.
#' @param flagcolours If `TRUE`, uses discrete colour map with thresholds defined by `flagthresh`. If `FALSE` uses continuous colour map.
#' @param flagthresh A 3-length vector of thresholds for highlighting correlations, if `flagcolours = TRUE`.
#' `flagthresh[1]` is the negative threshold (default -0.4). Below this value, values will be flagged red.
#' `flagthresh[2]` is the "weak" threshold (default 0.3). Values between `flagthresh[1]` and `flagthresh[2]` are coloured grey.
#' `flagthresh[3]` is the "high" threshold (default 0.9). Anything between `flagthresh[2]` and `flagthresh[3]` is flagged "OK",
#' and anything above `flagthresh[3]` is flagged "high".
#' @param pval The significance level for plotting correlations. Correlations with \eqn{p < pval} will be shown,
#' otherwise they will be plotted as the colour specified by `insig_colour`. Set to 0 to disable this.
#' @param insig_colour The colour to plot insignificant correlations. Defaults to a light grey.
#' @param text_colour The colour of the correlation value text (default white).
#' @param discrete_colours An optional 4-length character vector of colour codes or names to define the discrete
#' colour map if `flagcolours = TRUE` (from high to low correlation categories). Defaults to a green/blue/grey/purple.
#' @param box_colour The line colour of grouping boxes, default black.
#' @param order_as Optional list for ordering the plotting of variables. If specified, this must be a list of length 2, where each entry of the list is
#' a character vector of the iCodes plotted on the x and y axes of the plot. The plot will then follow the order of these character vectors. Note this must
#' be used with care because the `grouplev` and `boxlev` arguments will not follow the reordering. Hence this argument is probably best used for plots
#' with no grouping, or for simply re-ordering within groups.
#' @param use_directions Logical: if `TRUE` the extracted data is adjusted using directions found inside the coin (i.e. the "Direction"
#' column input in `iMeta`: any indicators with negative direction will have their values multiplied by -1 which will reverse the
#' direction of correlation). This should only be set to `TRUE` if the data set has *not* yet been normalised. For example, this can be
#' useful to set to `TRUE` to analyse correlations in the raw data, but would make no sense to analyse correlations in the normalised
#' data because that already has the direction adjusted! So you would reverse direction twice. In other words, use this at your
#' discretion.
#'
#' @importFrom ggplot2 ggplot aes geom_tile
#' @importFrom rlang .data
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)
#'
#' # plot correlations between indicators in Sust group, using Normalised dset
#' plot_corr(coin, dset = "Normalised", iCodes = list("Sust"),
#'           grouplev = 2, flagcolours = TRUE)
#'
#' @return A plot object generated with ggplot2, which can be edited further with ggplot2 commands.
#'
#' @export
plot_corr <- function(coin, dset, iCodes = NULL, Levels = 1, ..., cortype = "pearson",
                     withparent = FALSE, grouplev = NULL, box_level = NULL, showvals = TRUE, flagcolours = FALSE,
                     flagthresh = NULL, pval = 0.05, insig_colour = "#F0F0F0",
                     text_colour = NULL, discrete_colours = NULL, box_colour = NULL, order_as = NULL, use_directions = FALSE){


  # NOTE SET grouplev default to level + 1
  # grouplev <- Levels[1] + 1

  # CHECKS ------------------------------------------------------------------

  if (length(iCodes) == 1){
    iCodes = rep(iCodes, 2)
  }
  if (length(Levels) == 1){
    Levels = rep(Levels, 2)
  }
  if (Levels[2] > Levels [1]){
    Levels <- rev(Levels)
    iCodes <- rev(iCodes)
  }

  crtable <- get_corr(coin, dset = dset, iCodes = iCodes, Levels = Levels,
                      ... = ..., cortype = cortype, pval = pval, withparent = withparent,
                      grouplev = grouplev, make_long = TRUE, use_directions = use_directions)

  # round values for plotting
  crtable$Correlation <- round(crtable$Correlation,2)

  # remove diags, otherwise plot looks annoying
  crtable <- crtable[as.character(crtable$Var1) != as.character(crtable$Var2),]

  # get index structure
  lin <- coin$Meta$Lineage

  ##- PLOT -----------------------------------------

  # get orders (otherwise ggplot reorders)
  ord1 <- unique(crtable$Var1)
  ord2 <- unique(crtable$Var2)

  # sometimes these orderings come out not sorted according to higher aggregation levels
  # Here we sort them according to the order in IndMeta (which is already sorted)
  # Order first set (unless family plot in which case no, cos messes up)
  if(withparent != "family"){
    c1 <- unlist(lin[Levels[1]])
    ord1 <- unique(c1[c1 %in% ord1])
  }
  # Order second set
  c2 <- unlist(lin[Levels[2]])
  ord2 <- unique(c2[c2 %in% ord2])
  if(withparent ==  "family"){
    ord2 <-rev(ord2)
  }

  # if we are correlating a set with itself, we make sure the orders match
  if(setequal(ord1,ord2)){
    # reversing agrees with the "classical" view of a correlation matrix
    ord2 <- rev(ord1)
  }

  # for discrete colour map
  if(is.null(flagthresh)){
    hithresh <- 0.9
    weakthresh <- 0.3
    negthresh <- -0.4
  } else {
    stopifnot(is.numeric(flagthresh),
              length(flagthresh) == 3)
    hithresh <- flagthresh[3]
    weakthresh <- flagthresh[2]
    negthresh <- flagthresh[1]
  }

  if(!is.null(order_as)){
    # custom ordering
    stopifnot(is.list(order_as),
              length(order_as) == 2,
              is.character(order_as[[1]]),
              is.character(order_as[[2]]))
    if(length(order_as[[1]]) != length(ord1)){
      stop("Error in length of order_as[[1]]: expected length = ", length(ord1))
    }
    if(length(order_as[[2]]) != length(ord2)){
      stop("Error in length of order_as[[2]]: expected length = ", length(ord2))
    }
    if(!setequal(order_as[[1]], ord1)){
      stop("Expected iCodes not found in order_as[[1]] - expected codes are: ", paste(ord1, collapse = ", "))
    }
    if(!setequal(order_as[[2]], ord2)){
      stop("Expected iCodes not found in order_as[[2]] - expected codes are: ", paste(ord2, collapse = ", "))
    }

    ord1 <- order_as[[1]]
    ord2 <- order_as[[2]]
  }

  if (flagcolours){

    # make new col with flags for each correlation
    crtable$Flag <- ifelse(crtable$Correlation >= hithresh, yes = "High", no = "OK")
    crtable$Flag[(crtable$Correlation <= weakthresh)] <- "Weak"
    crtable$Flag[(crtable$Correlation <= negthresh)] <- "Negative"

    # make factors
    # note: moved from inside ggplot call below to hopefully help ggplotly tooltip
    crtable$Var1 <- factor(crtable$Var1, levels = ord1)
    crtable$Var2 <- factor(crtable$Var2, levels = ord2)

    # heatmap plot
    plt <- ggplot2::ggplot(data = crtable,
                           ggplot2::aes(x = .data$Var1,
                                        y = .data$Var2,
                                        fill = .data$Flag,
                                        label = .data$Correlation)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_x_discrete(expand=c(0,0)) +
      ggplot2::scale_y_discrete(expand=c(0,0))

    if(is.null(discrete_colours)){
      discrete_colours <- c("#80d67b", "#b8e8b5", "#e2e6e1", "#d098bd")
    } else {
      stopifnot(is.character(discrete_colours),
                length(discrete_colours)==4)
    }

    plt <- plt + ggplot2::scale_fill_manual(
      breaks = c("High", "OK", "Weak", "Negative"),
      values = discrete_colours,
      na.value = insig_colour
    )
  } else {

    # heatmap plot
    plt <- ggplot2::ggplot(data = crtable,
                           ggplot2::aes(x = .data$Var1,
                                        y = .data$Var2,
                                        fill = .data$Correlation,
                                        label = .data$Correlation)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::labs(x = NULL, y = NULL, fill = "Correlation") +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::scale_x_discrete(expand=c(0,0)) +
      ggplot2::scale_y_discrete(expand=c(0,0))

    plt <- plt + ggplot2::scale_fill_gradient2(mid="#FBFEF9",low="#A63446",high="#0C6291", limits=c(-1,1),
                                               na.value = insig_colour)
  }

  if (showvals){
    if(is.null(text_colour)){
      if(flagcolours){
        text_colour <- "#6a6a6a"
      } else {
        text_colour <- "white"
      }
    }
    plt <- plt + ggplot2::geom_text(colour = text_colour, size = 3, na.rm = TRUE)
  }

  # boxes
  # the relevant function is ggplot2::annotate

  if(is.null(box_colour)){
    box_colour <- "#505050"
  }

  if(withparent=="family"){

    # for family, we always plot boxes

    # isolate cols of things we are correlating. Here all levels above current.
    acls <- unique(lin[min(Levels):ncol(lin)])
    # filter out to current set of indicators
    acls <- acls[unlist(acls[1]) %in% unlist(unique(crtable[2])), ]
    # now we need to iterate over columns, excluding the first one
    for(icol in 2:ncol(acls)){
      # isolate the column of interest
      parents <- unlist(acls[icol])
      # starting and ending indices of the rectangles
      yends <- match(unique(parents), parents)
      yends <- length(ord2) - yends + 1.5
      ystarts <- c(yends, 0.5)
      ystarts <- ystarts[-1]
      xstarts <- rep(icol - 1.5, length(ystarts))
      xends <- xstarts  + 1

      plt <- plt + ggplot2::annotate("rect", xmin=xstarts, xmax=xends, ymin=ystarts, ymax=yends,
                                     fill = NA, color = box_colour)
      # dark grey: #606060
    }

  } else if(!is.null(box_level)) {

    if(box_level < Levels[2]+1){
      stop("box_level must be at least the aggregation level above Levels.")
    }

    # isolate cols of things we are correlating, plus box level
    acls <- unique(lin[c(Levels[1], box_level)])
    # filter out to current set of indicators
    acls <- acls[unlist(acls[1]) %in% unlist(unique(crtable[1])), ]
    # we need four vectors for annotate: xmin, xmax, ymin and ymax
    # actually xmin=ymin and xmax=ymax
    parents <- unlist(acls[2])
    # starting indices of the rectangles
    starts <- match(unique(parents), parents)
    # ends are the same, but shifted one along and with the last index included
    ends <- c(starts, length(ord1)+1)
    # remove the first element
    ends <- ends[-1]
    # now we mess around to get the correct positions. Tile boundaries are
    # at half intervals. But also due to the fact that the y axis is reversed
    # we have to subtract from the length.
    xstarts <- starts - 0.5
    xends <- ends - 0.5
    if(Levels[1]==Levels[2]){
      yends <- length(ord1) - xends + 1
      ystarts <- length(ord1) - xstarts + 1
    } else {

      # isolate cols of things we are correlating, plus box level
      acls <- unique(lin[c(Levels[2], box_level)])
      # filter out to current set of indicators
      acls <- acls[unlist(acls[1]) %in% unlist(unique(crtable[2])), ]
      # get parent codes
      parents <- unlist(acls[2])
      # starting indices of the rectangles
      ystarts <- match(unique(parents), parents) - 0.5
      # ends are the same, but shifted one along and with the last index included
      yends <- c(ystarts, length(ord2) + 0.5)
      # remove the first element
      yends <- yends[-1]
    }

    # add the rectangles to the plot
    plt <- plt + ggplot2::annotate("rect", xmin=xstarts, xmax=xends, ymin=ystarts, ymax=yends,
                                   fill = NA, color = box_colour)

  }

  plt  +
    ggplot2::theme(text=ggplot2::element_text(family="sans"))

}
