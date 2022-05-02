#' Framework plots
#'
#' Plots the hierarchical indicator framework. If `type = "sunburst"` (default), the framework is plotted as a
#' sunburst plot. If `type = "stack"` it is plotted as a linear stack. In both cases, the size of each component
#' is reflected by its weight and the weight of its parent, i.e. its "effective weight" in the framework.
#'
#' The colouring of the plot is defined to some extent by the `colour_level` argument. This should be specified
#' as an integer between 1 and the highest level in the framework (i.e. the maximum of the `iMeta$Level` column).
#' Levels higher than and including `colour_level` are coloured with individual colours from the standard colour
#' palette. Any levels *below* `colour_level` are coloured with the same colours as their parents, to emphasise
#' that they belong to the same group, and also to avoid repeating the colour palette. Levels below `colour_level`
#' can be additionally differentiated by setting `transparency = TRUE` which will apply increasing transparency
#' to lower levels.
#'
#' This function returns a ggplot2 class object. If you want more control over the appearance of the plot, pass
#' return the output of this function to a variable, and manipulate this further with ggplot2 commands to e.g.
#' change colour palette, individual colours, add titles, etc.
#'
#' @param coin A coin class object
#' @param type Either `"sunburst"` or `"stack"`.
#' @param colour_level The framework level, as an integer, to colour from. See details.
#' @param text_colour Colour of label text - default `"white"`.
#' @param text_size Text size of labels, default 2.5
#' @param transparency If `TRUE`, levels below `colour_level` are differentiated with some transparency.
#'
#' @importFrom rlang .data
#'
#' @return A ggplot2 plot object
#' @export
plot_framework <- function(coin, type = "sunburst", colour_level = NULL,
                           text_colour = NULL, text_size = NULL, transparency = TRUE){

  # CHECKS ------------------------------------------------------------------

  check_coin_input(coin)
  stopifnot(type %in% c("sunburst", "stack"))

  # get iMeta
  iMeta <- coin$Meta$Ind[!is.na(coin$Meta$Ind$Level), ]
  maxlev <- coin$Meta$maxlev

  # DEFAULTS ----------------------------------------------------------------

  text_colour <- set_default(text_colour, "white")
  text_size <- set_default(text_size, 2.5)
  colour_level <- set_default(colour_level, maxlev - 1)
  stopifnot(colour_level %in% 1:maxlev)

  # COLOURS -----------------------------------------------------------------

  # check if EffWeight present, if not, get
  if(is.null(iMeta$EffWeight)){
    coin <- get_eff_weights(coin, out2 = "coin")
    # get iMeta
    iMeta <- coin$Meta$Ind[!is.na(coin$Meta$Ind$Parent), ]
  }
  # get lineage
  lin <- coin$Meta$Lineage

  # add colouring col
  # this is fiddly
  iMeta$colourcol <- "a"

  for(lev in 1:maxlev){
    # get codes
    codes <- iMeta$iCode[iMeta$Level == lev]
    if(lev <= colour_level){
      # get groups at colour_level
      iMeta$colourcol[match(codes, iMeta$iCode)] <-
        lin[[colour_level]][match(codes, lin[[lev]])]
    } else {
      iMeta$colourcol[match(codes, iMeta$iCode)] <- codes
    }
  }

  if(type == "sunburst"){
    # some special treatment to get rid of the center circle
    iMeta$EffWeight[iMeta$Level == maxlev] <- 0
    iMeta$colourcol[iMeta$Level == maxlev] <- iMeta$colourcol[iMeta$Level == (maxlev - 1)][1]
  }

  # have to make colourcol into a factor column with an ordering of factors
  # that I specify, otherwise ordering is wrong
  fac_order <- unique(Reduce(c,rev(lin)))
  # reorder factors
  iMeta$colourcol <- factor(iMeta$colourcol, fac_order)

  # transparency if needed
  trans <- c(0.8,0.6,rep(0.4, 100))
  iMeta$Alf <- 1
  # Only levels below colour_level are given transparency
  iMeta$Alf[iMeta$Level < colour_level] <- trans[colour_level - iMeta$Level[iMeta$Level < colour_level]]

  # finally, I have to reverse the levels otherwise plot is inside out
  iMeta$Level <- maxlev - iMeta$Level + 1

  # PLOT --------------------------------------------------------------------

  # basic
  plt <- ggplot2::ggplot(iMeta, ggplot2::aes(x = .data$Level,
                                             y = .data$EffWeight,
                                             fill = .data$colourcol,
                                             label = .data$iCode))

  # bars
  if(transparency){
    plt <- plt + ggplot2::geom_bar(stat = "identity", color='white', alpha = iMeta$Alf)
  } else {
    plt <- plt + ggplot2::geom_bar(stat = "identity", color='white')
  }

  # text
  plt <- plt + ggplot2::geom_text(size = text_size, check_overlap = TRUE, position = ggplot2::position_stack(vjust = 0.5),
                     colour = text_colour)

  # alter to sunburst if needed
  if(type == "sunburst"){
    plt <- plt + ggplot2::coord_polar('y')
  }

  # styling
  plt <- plt + ggplot2::theme_minimal() +
    ggplot2::ylab("") + ggplot2::xlab("") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          strip.background = ggplot2::element_blank(),
          axis.text= ggplot2::element_blank(),
          axis.ticks= ggplot2::element_blank(),
          legend.position="none"
    )

  plt


}
