#' Reweight indicators
#'
#' Interactive gadget which lets you adjust weights and see the effects. Weights can be saved with new names to the COIN object.
#'
#' @param COIN COIN object
#'
#' @importFrom plotly plot_ly plotlyOutput layout add_trace renderPlotly add_segments
#' @importFrom reactable renderReactable reactableOutput
#'
#' @return An updated COIN object with additional sets of weights.
#'
#' @export

rew8r <- function(COIN){

  stop("Sorry, rew8r is out of action for a bit until I get some updates sorted. Back soon.")

  # NOTE I need to make this compatible with the new weight format. Easiest may be to simply copy
  # GII version over which also has other small updates...

  # aggregate names for dropdown lists
  agnames <- paste0("Level ", 1:COIN$Parameters$Nlevels)

  # get indicator names
  inames <- COIN$Parameters$IndCodes

  # initial weights
  w0 <- COIN$Parameters$Weights$Original
  # initial correlations etc
  crOut <- weights2corr(COIN, w0, aglevs = c(1, COIN$Parameters$Nlevels))$cr

  # initialise data frame for plotting
  dat <- data.frame(
    Indicator = crOut[[1]],
    Weight = w0[[1]],
    Correlation = crOut[[3]] )
  colnames(dat)[3] <- "Correlation"

  ## Create the shiny UI layout
  ui <- fluidPage(

    # the side panel
    sidebarPanel(
      h3("ReW8R v0.2"),
      fluidRow(
        column(6,selectInput("aglev1", "Correlate this:",
                             agnames, selected = "Level 1")),
        column(6,selectInput("aglev2", "with this:",
                             agnames, selected = "Level 2"))
      ),
      fluidRow(
        column(6,checkboxInput("facet", "Separate groups", value = FALSE)),
        column(6,selectInput("cortype", "Correlation type",
                             c("pearson","spearman","kendall")))
      ),
      hr(style = "border-top: 1px solid #000000;"),
      h4("Weighting"),
      selectInput("vseldrop", "Select indicator here or by clicking a point on plot.",
                  c("<Select>",inames)),
      "Indicator weight",
      sliderInput("wi", "Select indicator first",
                  min = 0, max = 1,
                  value = 1, step = 0.1),
      fluidRow(
        column(4,br(),actionButton("butEQw", "Equal weights")),
        column(8,
               selectInput(inputId = "wset", label = "Weight sets", choices = c(names(COIN$Parameters$Weights)))
               )
      ),
      hr(style = "border-top: 1px solid #000000;"),
      fluidRow(
        column(6,numericInput("locorval", "Low corr. threshold:", -0.2, min = -1, max = 1, step = 0.05)),
        column(6,numericInput("hicorval", "High corr. threshold:", 0.9, min = -1, max = 1, step = 0.05))
      ),
      hr(style = "border-top: 1px solid #000000;"),
      h4("Weights output"),
      fluidRow(
        column(8,textInput("weightsname", "Save as", "AltWeights")),
        column(4,br(),actionButton("saveweights", "Save"),)
      ),
      actionButton("closeapp", "Close app", width = "100%")

    ),

    # the main panel (graph, table, etc)
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Correlations",
                           suppressMessages(plotly::plotlyOutput("corrplot")),
                           br(),
                           fluidRow(
                             column(5,
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Ind-Ind",
                                                         h4("Flagged indicators (high corr. within group)"),
                                                         reactable::reactableOutput("collinSP")
                                                ),
                                                tabPanel("Cross-level",
                                                         h4("Flagged indicators"),
                                                         reactable::reactableOutput("locorinds")))),
                             column(7,
                                    h4("Correlation heatmap"),
                                    suppressMessages(plotly::plotlyOutput("corrheat")),
                                    checkboxInput("HMcorrvals", "Show correlation values", value = FALSE)
                             )
                           )
                  ),
                  tabPanel("Results",
                           h4("Scores and ranks"),
                           reactable::reactableOutput("restable"))
      )#tabsetpanel
    )#mainpanel
  )#fluidpage

  ## Create the Shiny Server layout
  server <- function(input, output, session) {

    # The main input to plotly is "dat", which consists of (a) weights, and (b) correlationa
    # The weight is updated by two things: the plotly click which says which variable to target, and the weight slider which says what value to assign
    # So, need to monitor two inputs: plotly click -> variable, and slider -> value
    # THEN, need to rebuild dat. Then plot.
    # Finally, need to make sure that dat doesn't forget previous values.

    # this is the plotly click data
    event.data <- reactive({plotly::event_data(event = "plotly_click", source = "scplot")})

    # the weights (full list for all levs)
    w <- reactiveVal(w0)

    # the table of data. Initialise with data
    dfRes <- reactiveVal(
      COIN$Data$Aggregated[c("UnitName",
                              COIN$Parameters$AggCodes[[length(COIN$Parameters$AggCodes)]],
                              COIN$Parameters$AggCodes[[length(COIN$Parameters$AggCodes)-1]])]
    )

    # the correlations
    #crs <- reactiveVal(cr0)

    # the list of weightIndCodes to output
    wlist <- reactiveVal(NULL)

    # the vector of indicator codes
    icodes <- reactiveVal(inames)

    # First, monitor which variable is active
    # Create reactive value for active var
    acvar <- reactiveVal(NULL)
    # update active variable via plot click
    observeEvent(event.data(),{
      acvar(event.data()$key)})
    # update active variable via dropdown
    observeEvent(input$vseldrop,
                 acvar(input$vseldrop))
    # Make reactive values for the two aggregation levels
    lev1 <- reactiveVal(1)
    lev2 <- reactiveVal(COIN$Parameters$Nlevels)
    # update aggregation level to numeric (used by the functions)
    observeEvent(input$aglev1,
                 lev1(which(agnames==input$aglev1)))
    observeEvent(input$aglev2,
                 lev2(which(agnames==input$aglev2)))

    # modify weight vector
    observeEvent(input$wi,{
      # this is the full list of weights
      wdash <- w()
      # get the right level
      wdash1 <- wdash[[lev1()]]
      # modify the right element
      wdash1[icodes() == acvar()] <- input$wi
      # put it all back
      wdash[[lev1()]] <- wdash1
      w(wdash)
    })

    # this is the main data frame with correlations and weights etc
    dat1 <- reactive({

      out1 <- weights2corr(COIN, w(), aglevs = c(lev1(), lev2()), cortype = input$cortype)

      # the table of results
      dfRes(out1$dat)
      # correlations
      #crs(out1$cr[[3]])

      wts <- w()

      dat <- data.frame(
        Indicator = out1$cr[[1]],
        Parent = out1$cr[[2]],
        Weight = wts[[lev1()]],
        Correlation = out1$cr[[3]] )
      colnames(dat)[4] <- "Correlation"


      return(dat)
    })

    # also update everything when level changes
    observeEvent(lev1(),{

      out1 <- weights2corr(COIN, w(), aglevs = c(lev1(), lev2()), cortype = input$cortype)
      # get new ind codes
      icodes(out1$cr[[1]])
    })

    ## Create correlation scatter plot
    output$corrplot <- plotly::renderPlotly({
      p <- corrweightscat(dat1(), facet = input$facet, acvar = acvar(), linesw = TRUE,
                          locorval = input$locorval, hicorval = input$hicorval) %>%
        suppressWarnings()
      p <- p %>% plotly::layout(title = paste0("Correlation of ", input$aglev1, " with ", input$aglev2))
      p
    })

    ## Create correlation heatmap
    output$corrheat <- plotly::renderPlotly({
      iplotCorr(COIN, aglevs = c(lev1(), lev2()), grouprects = TRUE,
               corthresh = list(clow = input$locorval, chigh = input$hicorval),
               showvals = input$HMcorrvals, cortype = input$cortype, useweights = w()) %>%
        suppressWarnings()
    })

    # collinear indicators within sub-pillar
    output$collinSP <- reactable::renderReactable({
      dfc <- hicorrSP(COIN, hicorval = input$hicorval, cortype = input$cortype)
      if( nrow(dfc)==0){
        dfc <- data.frame(Indicator = "None")
        return(dfc %>% reactable::reactable() )
      } else {
        return(dfc %>%
                 reactable::reactable(defaultPageSize = 7, highlight = TRUE, wrap = F,
                                      defaultSorted = list(Corr = "desc")))
      }
    })

    # low/high correlation indicators
    output$locorinds <- reactable::renderReactable({
      dfc <- dat1()
      dfc$Correlation <- round(dfc$Correlation,3)
      colnames(dfc)[2] <- "In"
      rownames(dfc) <- NULL
      dfclo <- cbind(dfc, "Flag" = "Low")
      dfclo <- dplyr::filter(dfclo, .data$Correlation < input$locorval)
      dfchi <- cbind(dfc, "Flag" = "High")
      dfchi <- dplyr::filter(dfchi, .data$Correlation > input$hicorval)
      dfc <- rbind(dfclo,dfchi)
      if( nrow(dfc)==0){
        dfc <- data.frame(Indicator = "None")
        return(dfc %>% reactable::reactable() )
      } else {
        return(dfc %>%
                 reactable::reactable(defaultPageSize = 7, highlight = TRUE, wrap = F,
                                      defaultSorted = list(Correlation = "asc")))
      }
    })

    # table of unit results
    output$restable <- reactable::renderReactable({
      data.frame(lapply(dfRes(), function(y) if(is.numeric(y)) round(y, 3) else y)) %>%
        reactable::reactable(defaultPageSize = 20, highlight = TRUE, wrap = F,
                             defaultSorted = list(Index = "desc"))
    })

    # update slider
    observeEvent(acvar(),{
      wts <- w()
      wts <- wts[[lev1()]]
      updateSliderInput(session, "wi",
                        label = acvar(),
                        value = wts[icodes()==acvar()])
    })

    # update dropdown menu on active variable
    observeEvent(acvar(),{
      updateSelectInput(session, "vseldrop", selected = acvar())
    })

    # update dropdown menu on change of level
    observeEvent(lev1(),{
      updateSelectInput(session, "vseldrop", choices = icodes() )
    })

    # button to set to equal weights
    observeEvent(input$butEQw,{
      wdash <- w()
      wts <- wdash[[lev1()]]
      # update weights
      wts <- rep(1,length(icodes()))
      wdash[[lev1()]] <- wts
      w(wdash)
      # also update slider
      updateSliderInput(session, "wi",
                        label = acvar(),
                        value = wts[icodes()==acvar()])
    })

    # dropdown to choose weight sets
    observeEvent(input$wset,{
      # change weights
      w(COIN$Parameters$Weights[[input$wset]])
      # also update slider
      updateSliderInput(session, "wi",
                        label = acvar(),
                        value = w()[icodes()==acvar()])
    })

    # button to save weights
    observeEvent(input$saveweights,{

      wnew <- w()

      if(is.null(wlist())){
        # create list and save name of weights
        eval(parse(text=paste0("wlist(list(",input$weightsname," = wnew ))")))
      } else {
        # add to list. Have to make copy because otherwise seems difficult to modify reactive list
        wlist2  <- wlist()
        eval(parse(text=paste0("wlist2$",input$weightsname," = wnew ")))
        wlist(wlist2)
      }
    })

    # end app and return weights
    observeEvent(input$closeapp, {
      COIN$Parameters$Weights <- c(COIN$Parameters$Weights, wlist())
      returnValue <- COIN
      stopApp(returnValue)
    })

  }

  runGadget(ui, server, viewer = browserViewer())

}

#' Recalculate correlations and ranks based on new weights
#'
#' This is a short cut function which takes a new set of indicator weights, and recalculates the COIN results
#' based on these weights. It returns a summary of rankings and the correlations between indicators and index.
#'
#' @param COIN COIN object
#' @param w Full list of weights for each level
#' @param aglevs A 2-length vector with two aggregation levels to correlate against each other
#' @param icodes List of two character vectors of indicator codes, corresponding to the two aggregation levels
#' @param cortype Correlation type. Either "pearson" (default), "kendall" or "spearman".
#' @param withparent Logical: if TRUE, only correlates with the parent, e.g. sub-pillars are only correlated with their parent pillars and not others.
#'
#' @importFrom reshape2 melt
#' @importFrom dplyr inner_join
#'
#' @return A list with .$cr is a vector of correlations between each indicator and the index, and
#' .$dat is a data frame of rankings, with unit code, and index, input and output scores
#'
#' @export

weights2corr <- function(COIN, w, aglevs = NULL, icodes = NULL,
                         cortype = "pearson", withparent = TRUE){

  if(is.null(aglevs)){
    aglevs <- c(1, COIN$Parameters$Nlevels)
  }

  if(is.null(COIN$Method$aggregate)){
    stop("You have not yet aggregated your data. This needs to be done first.")
  }

  # aggregate
  COIN2 <- aggregate(COIN, agtype = COIN$Method$aggregate$agtype,
                     agweights = w,
                     dset = COIN$Method$aggregate$dset,
                     agtype_bylevel = COIN$Method$aggregate$agtype_bylevel,
                     agfunc = COIN$Method$aggregate$agfunc
                     )

  # get data to correlate against each other
  out1 <- getIn(COIN2, dset = "Aggregated", icodes = icodes[[1]], aglev = aglevs[1])
  idata1 <- out1$ind_data_only
  idata2 <- getIn(COIN2, dset = "Aggregated", icodes = icodes[[2]], aglev = aglevs[2])$ind_data_only
  # table of results data
  dfres <- COIN2$Data$Aggregated[c("UnitName",
                                   COIN$Parameters$AggCodes[[length(COIN$Parameters$AggCodes)]],
                                   COIN$Parameters$AggCodes[[length(COIN$Parameters$AggCodes)-1]])]
  dfres <- cbind("Rank"=rank(COIN2$Data$Aggregated$Index*-1, ties.method = "min"), dfres)
  # get correlations
  cr = stats::cor(idata1, idata2, method = cortype, use = "pairwise.complete.obs")

  # get index structure
  agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, dplyr::starts_with("Agg"))

  # select cols corresponding to what is being correlated against what
  agcols <- agcols[aglevs]
  # change correlation to long form
  # WHY because this makes a nice table also with names and what is correlated with what
  crlong <- suppressMessages(reshape2::melt(cr))
  colnames(crlong) <- c(colnames(agcols), "Correlation")

  # only correlate with parents, if asked. This is necessary if using inside the rew8r app because
  # we need a vector output. Also, it is the most relevant information.
  if (withparent & ncol(cr)>1){

    # now do inner join - we are matching correlation rows that agree with the structure of the index
    crtable <- dplyr::inner_join(agcols, crlong, by = colnames(agcols))

  } else {
    crtable <- crlong
  }
  # sometimes this throws duplicates, so remove
  crtable <- unique(crtable)

  # now we want the correlations...
  out <- list(cr = crtable,
              dat = dfres,
              icodes1 = out1$ind_names)

  return(out)

}

#' Scatter plot of correlations against weights
#'
#' Plots correlations on the x axis and weights on the y axis. Allows a selected highlighted point
#' and a line showing low correlation boundary.
#'
#' @param dat Data frame with first col indicator codes, second is weights, third is correlations
#' @param facet Logical: if TRUE creates subplots.
#' @param acvar Active variable to highlight (one of the indicator codes)
#' @param linesw Whether to plot a vertical line showing low correlation boundary
#' @param locorval x value of low correlation line
#' @param hicorval x value of high correlation line
#'
#' @importFrom rlang .data
#'
#' @return A scatter plot, also outputs event data (the clicked indicator)
#'
#' @export

corrweightscat <- function(dat, facet = FALSE, acvar = NULL, linesw = FALSE,
                           locorval = NULL, hicorval = NULL){

  icodes <- dat[[1]]

  # NOTE: deactivated changing markers here because became very messy
  # colours around markers when selected or not
  lincol <- ifelse(icodes %in% acvar, "red", "blue")
  # size of line around marker (set to 0 if not selected)
  linsize <- ifelse(icodes %in% acvar, 0, 0)

  # # symbol when above/below corr threshold
  # symbs <- if(input$linesw==TRUE){c(16,15)}else{c(16,16)}
  # # colour when above/below threshold
  # pcols <- if(input$linesw==TRUE){c("blue", "orange")}else{c("blue", "blue")}

  if(facet){

    # grid lines etc
    ax <- list(
      zeroline = TRUE,
      gridcolor = plotly::toRGB("gray25"),
      gridwidth = 1,
      zerolinecolor = plotly::toRGB("gray25"),
      zerolinewidth = 1.5,
      title = ""
    )

    # Define a function which creates a subplot given some data
    #my_plot <- . %>%
    my_plot <- function(plotdata){
      plotly::plot_ly(plotdata, x = ~Correlation, y = ~Weight, type = "scatter", mode = "markers",
                      text = ~Indicator, key = ~Indicator, source = "scplot",
                      marker = list(size = 10),# line = list(color = lincol, width = linsize)),
                      hovertemplate = paste0(
                        "<b>%{text}</b><br>",
                        "Weight = %{y:.2f} <br>",
                        "Correlation = %{x:.2f} <extra></extra>"
                      )) %>%
        plotly::add_annotations(
          text = ~unique(Parent),
          x = 0.5,
          y = 0.5,
          yref = "paper",
          xref = "paper",
          xanchor = "center",
          yanchor = "center",
          showarrow = FALSE,
          font = list(size = 25),
          opacity = 0.5
        ) %>% plotly::layout(xaxis = ax, yaxis = c(ax, list(range = c(0, 1.25))))
    }

    # now group and generate one plot for each group
    p <- dat %>%
      dplyr::group_by(.data$Parent) %>%
      dplyr::do(p = my_plot(.data))

    # subplots
    p <- p %>% plotly::subplot(nrows = ceiling(length(unique(dat$Parent))/6),
                               shareX = T, shareY = T) %>% suppressMessages()
    # legend on, margins
    p <- p %>% layout(showlegend = FALSE, margin = list(l=50, b =50))
    # big x and y labels
    p <- p %>% plotly::add_annotations(
      text = "Correlation",
      x = 0.5,
      y = -0.2,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "center",
      showarrow = FALSE,
      font = list(size = 13.5)
    )
    p <- p %>% plotly::add_annotations(
      text = "Weight",
      x = -0.05,
      y = 0.5,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "center",
      showarrow = FALSE,
      font = list(size = 13.5),
      textangle = 270
    )

  } else {

    # generate main plot
    p <- plotly::plot_ly(dat, x = ~Correlation, y = ~Weight, color = ~Parent, type = "scatter", mode = "markers",
                         text = ~Indicator, key = ~Indicator, source = "scplot",
                         marker = list(size = 10, line = list(color = lincol, width = linsize)),
                         hovertemplate = paste0(
                           "<b>%{text}</b><br>",
                           "Weight = %{y:.2f} <br>",
                           "Correlation = %{x:.2f} <extra></extra>"
                         )) %>%
      plotly::layout(showlegend = TRUE, yaxis = list(
        range = c(0, 1.25),
        autotick = FALSE,
        dtick = 0.25)
        # xaxis = list(
        #   range = c(-1, 1),
        #   autotick = FALSE,
        #   dtick = 0.2)
      )

    # add low correlation line, if activated
    if(linesw==TRUE){
      p <- p %>% plotly::add_segments(x = locorval, xend = locorval, y = 0, yend = 1.25,
                                      marker = list(color = 'red', opacity=0),
                                      line = list(dash = 'dash'))
      p <- p %>% plotly::add_segments(x = hicorval, xend = hicorval, y = 0, yend = 1.25,
                                      marker = list(color = 'red', opacity=0),
                                      line = list(dash = 'dash')) %>%
        plotly::layout(showlegend = FALSE)
    }
  }

  # return plot object
  p
}


#' Highly correlated indicators in same sub pillar
#'
#' This returns a data frame of any highly correlated indicators within the same sub-pillar.
#'
#' @param COIN Data frame with first col indicator codes, second is weights, third is correlations
#' @param dset The data set to use for correlations.
#' @param hicorval A threshold to flag high correlation. Default 0.9.
#' @param cortype The type of correlation, either "pearson" (default), "spearman" or "kendall"
#'
#' @importFrom reshape2 melt
#'
#' @return A data frame with one entry for every indicator pair that is highly correlated within a sub-pillar.
#' Pairs are only reported once, i.e. only uses the upper triangle of the correlation matrix.
#'
#' @export

hicorrSP <- function(COIN, dset = "Normalised", hicorval = 0.9, cortype = "pearson"){

  out1 <- getIn(COIN, dset = "Normalised")

  corr_ind <- stats::cor(out1$ind_data_only,
                         method = cortype, use = "pairwise.complete.obs") %>% round(2)

  subpill <- dplyr::select(COIN$Input$IndMeta, dplyr::starts_with("Agg"))[[1]]

  samepill <- matrix(FALSE, nrow = nrow(corr_ind), ncol = ncol(corr_ind))

  for(ii in 1:ncol(samepill)){
    pill_ii <- subpill[ii]
    samepill[,ii] <- subpill == pill_ii
  }

  corr_ind[!samepill] <- NA
  diag(corr_ind) <- NA
  corr_ind[lower.tri(corr_ind)] <- NA
  hicorr <- reshape2::melt(corr_ind)
  colnames(hicorr) <- c("Ind1", "Ind2", "Corr")
  hicorr <- cbind(SubPill = rep(subpill,nrow(corr_ind)), hicorr)
  hicorr <- hicorr[!is.na(hicorr$Corr),]
  hicorr <- hicorr[hicorr$Corr>hicorval,]

  return(tibble::as_tibble(hicorr))

}

#' Correlation heatmap
#'
#' Plots an interactive heatmap of a correlation matrix. Currently this only works with the
#' aggregated data set, i.e. you need to have aggregated the data first before using this.
#'
#' @param COIN The COIN object
#' @param aglevs A two length vector specifying which level to plot against which level. E.g. c(2,4) for
#' COIN plots sub-pillars against sub-indexes. If NULL, plots everything against everything.
#' @param insig Logical: if TRUE, all correlation values are plotted; if FALSE (default), does not plot insignificant correlations.
#' @param levs Logical: if TRUE, plots lines showing the division between different levels. Only works if aglevs = NULL.
#' @param grouprects Logical: if TRUE, plots rectangles showing aggregation groups
#' @param mapcols If set to "thresholds", uses a discrete colour scale specified by corthresh. Otherwise if "continuous"
#' uses a continuous colour map.
#' @param corthresh A named list specifying the colour thresholds to use if mapcols = "thresholds". Entries should
#' specify correlation thresholds and can specify any of clow, cmid and chi. Anything below clow will be
#' coloured red. Anything between clow and cmid will be grey. Anything between cmid and chigh will be blue.
#' Anything above chigh will be green. Default is list(clow = -0.4, cmid = 0.4, chigh = 0.85). You can
#' specify a subset of these and the others will revert to defaults.
#' @param showvals Logical: if TRUE, overlays correlation values on each square.
#' @param cortype The type of correlation: either "pearson" (default), "spearman" or "kendall"
#' @param useweights An optional list of weights to use (this is used mainly in the rew8r app)
#'
#' @importFrom reshape2 melt
#' @importFrom rlang .data
#'
#' @examples \dontrun{flags <- iplotCorr(COIN, aglevs = c(2,3))}
#'
#' @return A correlation map
#' @export

iplotCorr <- function(COIN, aglevs = NULL, insig = FALSE, levs = TRUE, grouprects = TRUE,
                     mapcols = "thresholds", corthresh = NULL, showvals = TRUE, cortype = "pearson",
                     useweights = NULL){

  # this expects that the parent is the second entry of aglevs. If not, swap
  if(!is.null(aglevs)){
    if(aglevs[1]>aglevs[2]){
      # swap around
      aglevs <- aglevs[c(2,1)]
    }
  }

  # if weights are specified, reaggregate first
  if(!is.null(useweights)){
    # aggregate
    COIN <- aggregate(COIN, agtype = COIN$Method$aggregate$agtype,
                       agweights = useweights,
                       dset = COIN$Method$aggregate$dset,
                       agtype_bylevel = COIN$Method$aggregate$agtype_bylevel,
                       agfunc = COIN$Method$aggregate$agfunc
    )
  }

  # get the relevant data sets
  out1 <- getIn(COIN, dset = "Aggregated", aglev = aglevs[1])
  out2 <- getIn(COIN, dset = "Aggregated", aglev = aglevs[2])

  # correlation
  corr_ind <- stats::cor(out1$ind_data_only, out2$ind_data_only, method = cortype, use = "pairwise.complete.obs") %>% round(2)
  corr_ind_text <- corr_ind # for annotations later

  # change insignificant correlations to NAs if asked
  if(insig==FALSE){
    corr_ind[abs(corr_ind)<(2/sqrt(nrow(out1$ind_data_only)))] <- NA
  }

  sometext <- matrix("3", nrow(corr_ind), ncol(corr_ind))

  # Rectangles

  # this is the structure of the index. All we need should be here
  aggcols <- COIN$Input$IndMeta %>% dplyr::select(.data$IndCode | dplyr::starts_with("Agg"))

  # this is the spec for one rectangle (coordinates not entered yet)
  # start with this to add to the list
  rect1 <- list(list(type = "rect", layer = "above",# fillcolor = "purple",
                     line = list(color = "purple", width = 2), opacity = 0.5,
                     x0 = 0, x1 = 0, xref = "x",
                     y0 = 0, y1 = 0, yref = "y"))

  # this is the order of indicator codes as in the correlation plot
  icodes <- out1$ind_names

  if(grouprects==TRUE){

    # dont want rects if plotting sth against itself, or if only one col
    if(!is.null(aglevs) & (ncol(corr_ind)>1) & (aglevs[1]!=aglevs[2]) ){

      # just get relevant aggregation cols
      aggcols2 <- unique(aggcols[aglevs])

      # first col is the children
      aggroupsC <- unique(aggcols2[[1]])
      # the second col of agglevs2 should be the parent. Loop over groups in that
      aggroupsP <- unique(aggcols2[[2]])

      # we should have one rectangle for each of the elements in aggroupsP. So one row for each,
      # with each row consisting of four entries for the coordinates of the 2 points to define the rect
      rects2 <- matrix(NA, length(aggroupsP), 4)

      for (gg in 1:length(aggroupsP)){
        rects2[gg,1]<- gg - 1.5
        rects2[gg,2]<- min(which(aggcols2[,2]==aggroupsP[gg])) - 1.5
        rects2[gg,3]<- gg - 0.5
        rects2[gg,4]<- max(which(aggcols2[,2]==aggroupsP[gg])) - 0.5
      }

      # copy rect specs to have one entry for each rectangle
      rectslist = rep(rect1,nrow(rects2))

      # loop now to add coordinates
      for (ii in 1:nrow(rects2)){
        rectslist[[ii]]$x0 <- rects2[ii,1]
        rectslist[[ii]]$y0 <- rects2[ii,2]
        rectslist[[ii]]$x1 <- rects2[ii,3]
        rectslist[[ii]]$y1 <- rects2[ii,4]
      }
    } else if (is.null(aglevs)) {

      # matrix for storing the coordinates of the rectangles in
      rects <- matrix(1,1,2)

      # this is fiddly. We have three things to loop over
      # - two times the level
      # - the groups inside the level

      for (jj in 1:(ncol(aggcols)-1)){

        # if(jj>1){
        #   aggcols1 <- aggcols[-(1:(jj-1))]
        # } else {
        #   aggcols1 <- aggcols
        # }
        # aggcols1 <- unique(aggcols1)

        jcol <- aggcols[[jj]]

        # loop over cols (levels). We don't want the indicator level or index level.
        for(ii in (jj+1):(ncol(aggcols)-1)){

          # the unique names of indicators/aggs in this level
          icol <- aggcols[[ii]]
          aggroups <- unique(icol)

          # getting the positions of rectangle corners
          for(kk in 1:length(aggroups)){

            groupkk <- aggroups[kk]
            jcol_elements <- jcol[(icol==groupkk)]

            rects <- rbind(rects,
                           c(min(which(icodes %in% jcol_elements)) - 1.5,
                             max(which(icodes %in% jcol_elements))- 0.5 ))
          }
        }

      }

      # remove first row
      rects <- rects[-1,]

      # copy rect specs to have one entry for each rectangle
      rectslist = rep(rect1,nrow(rects))

      # loop now to add coordinates
      for (ii in 1:nrow(rects)){
        rectslist[[ii]]$x0 <- rects[ii,1]
        rectslist[[ii]]$y0 <- rects[ii,1]
        rectslist[[ii]]$x1 <- rects[ii,2]
        rectslist[[ii]]$y1 <- rects[ii,2]
      }

    } else {
      # just use this as a list to add to, will be deleted later
      rectslist = rect1
    }


  } else {
    # just use this as a list to add to, will be deleted later
    rectslist = rect1
  }

  # plot level division lines, if asked
  if(levs == TRUE & is.null(aglevs)){

    # the outer limits....
    xlim <- length(out1$ind_names)-0.5
    ylim <- xlim

    # lines showing level divisions
    for (ii in 1:(length(aggcols)-1)){
      # last indicator in col
      lastind <- aggcols[[nrow(aggcols),ii]]
      # this is the position on the chart. -0.5 because that's how the coordinates work here.
      iilast <- which(out1$ind_names==lastind)-0.5
      # add vertical line
      linevert <- list(list(type = "line",
                            line = list(color = "black", width = 2), opacity = 1,
                            x0 = iilast, x1 = iilast, xref = "x",
                            y0 = -0.5, y1 = ylim, yref = "y"))
      # add horizontal line
      linehor <- list(list(type = "line",
                           line = list(color = "black", width = 2), opacity = 1,
                           x0 = -0.5, x1 = xlim, xref = "x",
                           y0 = iilast, y1 = iilast, yref = "y"))
      # add both lines to the big list
      rectslist <- c(rectslist, linevert, linehor)
    }

  }

  if(grouprects==FALSE){
    # need to remove first entry
    rectslist <- rectslist[-1]
  }

  ## HEATMAP COLOURS

  # these are the colours we will use
  cneg <- "#FF5733"
  clow <- "#A6ACAF"
  cgood <- "#D6EAF8"
  ccolin <- "#84BB60"

  # these are the thresholds between colours. Have to be normalised to [0,1] to work
  breaks1 <- c(-1,-0.4,0.4,0.85,1)
  if (is.null(corthresh)){
    breaks <- breaks1
  } else {
    # take elements out of input list if exist, otherwise use default
    breaks <- c(-1, ifelse(is.null(corthresh$clow),breaks1[2],corthresh$clow),
                ifelse(is.null(corthresh$cmid),breaks1[3],corthresh$cmid),
                ifelse(is.null(corthresh$chi),breaks1[4],corthresh$chi), 1)
  }


  breaks <- (breaks - min(breaks))/(max(breaks)-min(breaks))

  # formal definition of colour scale using breaks and colours
  colorScale <- data.frame(z=c(breaks[1],breaks[2],
                               breaks[2],breaks[3],
                               breaks[3],breaks[4],
                               breaks[4],breaks[5]
  ),
  col=c(cneg, cneg,
        clow, clow,
        cgood, cgood,
        ccolin, ccolin
  ))
  colorScale$col <- as.character(colorScale$col)

  # plot
  if(mapcols == "thresholds"){
    fig <- plotly::plot_ly(z = corr_ind,
                           x = names(out2$ind_data_only),
                           xgap = 2,
                           y = names(out1$ind_data_only),
                           ygap =2,
                           type = "heatmap",
                           colorscale= colorScale,
                           zmin = -1, zmax = 1,
                           hovertemplate = "corr(%{x}, %{y}) = %{z}<extra></extra>")

  } else if (mapcols == "continuous"){
    fig <- plotly::plot_ly() %>%
      plotly::add_heatmap(
        x = names(out2$ind_data_only),
        y = names(out1$ind_data_only),
        z = corr_ind,
        zmin = -1, zmid = 0, zmax = 1,
        colors = "RdBu",
        hovertemplate = "corr(%{x}, %{y}) = %{z}<extra></extra>",
        xgap = 2,
        ygap = 2
      )
  }


  fig <- plotly::layout(fig,
                        shapes = rectslist)

  tx <- reshape2::melt(corr_ind_text)

  if(showvals){
    # add values of correlation to each square
    fig <- plotly::add_annotations(fig, x = tx$Var2,
                                   y = tx$Var1,
                                   text = tx$value,
                                   showarrow = FALSE)
  }

  fig
}

#' Weight optimisation
#'
#' This function provides optimised weights to agree with a pre-specified vector of
#' "target importances"
#'
#' @param COIN COIN object
#' @param itarg a vector of (relative) target importances. For example, c(1,2,1) would specify that the second
#' indicator should be twice as "important" as the other two.
#' @param aglev The aggregation level to apply the weight adjustment to.
#' @param cortype The type of correlation to use - can be either "pearson", "spearman" or "kendall".
#' @param optype The optimisation type. Either "balance", which aims to balance correlations
#' according to a vector of "importances" specified by itarg (default), or "infomax" which aims to maximise
#' overall correlations. This latter option is experimental and may not yet work very well.
#' @param toler Tolerance for convergence. Defaults to 0.001 (decrease for more accuracy, increase if convergence problems).
#' @param maxiter Maximum number of iterations. Default 500.
#' @param out2 Where to output the results. If "COIN" (default for COIN input), appends to updated COIN,
#' creating a new list of weights in .$Parameters$Weights. Otherwise if "list" outputs to a list (default).
#'
#' @importFrom stats optim
#'
#' @return An updated object with optimised weights
#'
#' @export

weightOpt <- function(COIN, itarg, aglev, cortype = "pearson", optype = "balance",
                      toler = NULL, maxiter = NULL, out2 = NULL){

  # if equal influence requested
  if(is.character(itarg)){
    if(itarg == "equal"){
      itarg <- rep(1, sum(COIN$Parameters$Weights$Original$AgLevel == aglev))
    } else {
      stop("itarg not recognised - should be either numeric vector or \"equal\" ")
    }
  }

  if(length(itarg) != sum(COIN$Parameters$Weights$Original$AgLevel == aglev) ){
    stop("itarg is not the correct length for the specified aglev")
  }

  itarg <- itarg/sum(itarg)

  # we need to define an objective function. The idea here is to make a function, which when you
  # put in a set of weights, gives you the correlations

  objfunc <- function(w){

    # get full list of weights
    wlist <- COIN$Parameters$Weights$Original
    # modify appropriate level to current vector of weights
    wlist$Weight[wlist$AgLevel == aglev] <- w
    # re-aggregate using these weights, get correlations
    crs <- weights2corr(COIN, wlist, aglevs = c(aglev, COIN$Parameters$Nlevels),
                        cortype = cortype)$cr$Correlation

    if (optype == "balance"){
      # normalise so they sum to 1
      crs <- crs/sum(crs)
      # the output is the sum of the squared differences (note, could also log this)
      sqdiff <- sum((itarg - crs)^2)/length(crs)
    } else if (optype == "infomax"){
      # the output is the sum of the squared differences (note, could also log this)
      sqdiff <- log(sum(1 - crs))/length(crs)
    }
    message("iterating... squared difference = ", sqdiff)
    return(sqdiff)

  }

  # defaults for tolerance and max iterations
  if(is.null(toler)){toler <- 0.001}
  if(is.null(maxiter)){maxiter <- 500}

  # run optimisation
  optOut <- stats::optim(par = itarg, fn = objfunc, control = list(
    reltol = toler,
    maxit = maxiter
  ))

  if(optOut$convergence == 0){
    message("Optimisation successful!")
  } else {
    message("Optimisation did not converge for some reason...
         You can try increasing the number of iterations and/or the tolerance.")
  }

  # get full list of weights
  wopt <- COIN$Parameters$Weights$Original
  # modify appropriate level to optimised vector of weights
  wopt$Weight[wopt$AgLevel == aglev] <- optOut$par

  if(is.null(out2)){out2 <- "list"}

  if (out2 == "COIN"){
    wname <- paste0("OptimsedLev", aglev)
    COIN$Parameters$Weights[[wname]] <- wopt
    COIN$Analysis$Weights[[wname]] <- list(OptResults = optOut,
                                           CorrResults = data.frame(
                                             Desired = itarg,
                                             Obtained = crs <- weights2corr(COIN, wopt, aglevs = c(aglev, COIN$Parameters$Nlevels),
                                                                            cortype = cortype)$cr$Correlation,
                                             OptWeight = optOut$par
                                           ))
    return(COIN)
  } else {
    return(list(WeightsOpt = wopt,
                OptResults = optOut,
                CorrResults = data.frame(
                  Desired = itarg,
                  Obtained = crs <- weights2corr(COIN, wopt, aglevs = c(aglev, COIN$Parameters$Nlevels),
                                                 cortype = cortype)$cr$Correlation,
                  OptWeight = optOut$par
                )))
  }

}

