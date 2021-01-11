#' Reweight indicators
#'
#' Interactive gadget which lets you adjust weights of indicators and see the effects on the results and
#' the correlations. Weights can be saved with new names to the COIN object.
#'
#' @importFrom stats var
#'
#' @param COINobj COIN object
#'
#' @return An updated COIN object with additional sets of weights (if saved from the app).
#'
#' @export

coin_rew8r <- function(COINobj){

  if (!exists("Aggregated",COINobj$Data)){
    stop("No aggregated data set found. You have to aggregate your index first using coin_aggregate, otherwise I don't know
         what method you want to use for aggregation.")
  }

  # get indicator names
  inames <- COINobj$Parameters$IndCodes

  # aggregation level names to show in table
  agnames <- c(COINobj$Parameters$AggCodes[[COINobj$Parameters$Nlevels-1]],
               COINobj$Parameters$AggCodes[[COINobj$Parameters$Nlevels-2]])

  # initial weights
  w0 <- COINobj$Parameters$Weights$IndWeight
  # initial correlations
  cr0 <- weights2corr(COINobj, w0, agnames)$cr



  ## Create the shiny UI layout
  ui <- fluidPage(
    # the side panel
    sidebarPanel(
      h3("ReW8R v0.1"),
      selectInput("vseldrop", "Select indicator here or by clicking a point on plot.",
                  c("<Select>",inames)),
      "Indicator weight",
      sliderInput("wi", "Select indicator first",
                  min = 0, max = 1,
                  value = 1, step = 0.1),
      actionButton("butEQw", "Equal weights"),
      actionButton("butObjw", "Original weights"),
      hr(style = "border-top: 1px solid #000000;"),
      fluidRow(
        column(6,numericInput("locorval", "Low correlation threshold:", 0.2, min = -1, max = 1, step = 0.05)),
        column(6,br(),checkboxInput("locorsw", "Enable", value = FALSE))),
      hr(style = "border-top: 1px solid #000000;"),
      h4("Weights output"),
      textInput("weightsname", "Save as", "AltWeights"),
      actionButton("saveweights", "Save"),
      hr(style = "border-top: 1px solid #000000;"),
      actionButton("closeapp", "Close app", width = "100%")

    ),
    # the main panel (graph, table, etc)
    mainPanel(
      plotly::plotlyOutput("corrplot"),
      br(),
      fluidRow(
        column(5,
               h4("Summary statistics"),
               textOutput("crmean"),
               textOutput("crvar"),
               hr(),
               h4("Low-correlated indicators"),
               reactable::reactableOutput("locorinds")),
        column(7,
               h4("Scores and ranks"),
               reactable::reactableOutput("restable"))
      )
    )
  )

  ## Create the Shiny Server layout
  server <- function(input, output, session) {

    # The main input to plotly is "dat", which consists of (a) weights, and (b) correlationa
    # The weight is updated by two things: the plotly click which says which variable to target, and the weight slider which says what value to assign
    # So, need to monitor two inputs: plotly click -> variable, and slider -> value
    # THEN, need to rebuild dat. Then plot.
    # Finally, need to make sure that dat doesn't forget previous values.

    # this is the plotly click data
    event.data <- reactive({plotly::event_data(event = "plotly_click", source = "scplot")})

    # the weights
    w <- reactiveVal(w0)

    # the table of data. Initialise with data
    dfRes <- reactiveVal(COINobj$Data$Aggregated[c("UnitCode", agnames)])

    # the correlations
    crs <- reactiveVal(cr0)

    # the list of weights to output
    wlist <- reactiveVal(NULL)

    # First, monitor which variable is active
    # Create reactive value for active var
    acvar <- reactiveVal(NULL)
    # update active variable via plot click
    observeEvent(event.data(),{
      acvar(event.data()$key)})
    # update active variable via dropdown
    observeEvent(input$vseldrop,
                 acvar(input$vseldrop))

    # modify weight vector
    observeEvent(input$wi,{
      wdash <- w()
      wdash[inames == acvar()] <- input$wi
      w(wdash)
    })

    # update results df, and correlations when weights change
    observeEvent(w(),{
      out1 <- weights2corr(COINobj, w(), agnames)
      dfRes(out1$dat)
      crs(out1$cr)
    })

    ## Create correlation scatter plot
    output$corrplot <- plotly::renderPlotly({

      #out <- weights2corr(X, w())

      dat <- data.frame(
        Indicator = inames,
        Weight = w(),
        Correlation = crs() )

      # colours around markers when selected or not
      lincol <- ifelse(inames %in% acvar(), "red", "blue")
      # size of line around marker (set to 0 if not selected)
      linsize <- ifelse(inames %in% acvar(), 3, 0)

      # symbol when above/below corr threshold
      symbs <- if(input$locorsw==TRUE){c(16,15)}else{c(16,16)}
      # colour when above/below threshold
      pcols <- if(input$locorsw==TRUE){c("blue", "orange")}else{c("blue", "blue")}

      # generate main plot
      p <- plot_ly(dat, x = ~Correlation, y = ~Weight, type = "scatter", mode = "markers",
                   text = ~Indicator, key = ~Indicator, source = "scplot",
                   marker = list(size = 10, line = list(color = lincol, width = linsize)),
                   hovertemplate = paste0(
                     "<b>%{text}</b><br>",
                     "Weight = %{y:.2f} <br>",
                     "Correlation = %{x:.2f} <extra></extra>"
                   )) %>%
        layout(showlegend = FALSE, yaxis = list(
          range = c(0, 1.25),
          autotick = FALSE,
          dtick = 0.25),
          xaxis = list(
            range = c(-1, 1),
            autotick = FALSE,
            dtick = 0.2))

      # add low correlation line, if activated
      if(input$locorsw==TRUE){
        p <- p %>% plotly::add_segments(x = input$locorval, xend = input$locorval, y = 0, yend = 1.25,
                                marker = list(color = 'red', opacity=0),
                                line = list(dash = 'dash')) %>%
          layout(showlegend = FALSE)
      }

      # return plot object
      p

    })

    output$crmean <- renderText({
      paste0("Mean correlation = ", round(mean(crs()),3))
    })
    output$crvar <- renderText({
      paste0("Variance of correlation = ", round(stats::var(crs()),3))
    })

    # low correlation indicators
    output$locorinds <- reactable::renderReactable({
      dfc <- data.frame(
        Indicator = inames,
        Weight = w(),
        Correlation = round(crs(),3) )

      dfc <- dfc[dfc$Correlation < input$locorval,]
      rownames(dfc) <- NULL
      if( nrow(dfc)==0 | input$locorsw==FALSE){
        dfc <- data.frame(Indicator = "None")
        return(dfc %>% reactable::reactable() )
      } else {
        return(dfc %>%
                 reactable::reactable(defaultPageSize = 5, highlight = TRUE, wrap = F,
                           defaultSorted = list(Correlation = "asc")))
      }
    })

    # table of unit results
    output$restable <- reactable::renderReactable({
      data.frame(lapply(dfRes(), function(y) if(is.numeric(y)) round(y, 3) else y)) %>%
        reactable::reactable(defaultPageSize = 10, highlight = TRUE, wrap = F,
                  defaultSorted = list(Index = "desc"))
    })

    # update slider
    observeEvent(acvar(),{
      updateSliderInput(session, "wi",
                        label = acvar(),
                        value = w()[inames==acvar()])
    })

    # update dropdown menu
    observeEvent(acvar(),{
      updateSelectInput(session, "vseldrop", selected = acvar())
    })

    # button to set to equal weights
    observeEvent(input$butEQw,{
      # update weights
      w( rep(1,length(inames)) )
      # also update slider
      updateSliderInput(session, "wi",
                        label = acvar(),
                        value = w()[inames==acvar()])
    })

    # button to reset to original Obj weights
    observeEvent(input$butObjw,{
      # reset weights
      w(w0)
      # also update slider
      updateSliderInput(session, "wi",
                        label = acvar(),
                        value = w()[inames==acvar()])
    })

    # button to save weights
    observeEvent(input$saveweights,{

      wnew <- COINobj$Parameters$Weights
      wnew$IndWeight <- w()

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
      COINobj$Parameters$WeightsAdj <- wlist()
      returnValue <- COINobj
      stopApp(returnValue)
    })

  }

  runGadget(ui, server, viewer = browserViewer())

}

#' Recalculate correlations and ranks based on new weights
#'
#' This is a short cut function which takes a new set of indicator weights, and recalculates the aggregated results
#' based on these weights. It returns a summary of rankings and the correlations between indicators and index.
#'
#' @param COINobj COINobj object
#' @param w a vector of indicator weights
#' @param agnames Names of aggregation groups to be included in results table
#'
#' @return A list with .$cr is a vector of correlations between each indicator and the index, and
#' .$dat is a data frame of rankings, with unit code, and index, input and output scores
#'
#' @export

weights2corr <- function(COINobj, w, agnames){

  w <- w/sum(w) # normalise first to sum to 1
  COINobj$Parameters$Weights$IndWeight <- w # assign ind weights to object

  # aggregate
  COINobj2 <- coin_aggregate(COINobj, agtype="arith_mean")

  # get main table of results: these are the last two aggregation levels
  dfres <- COINobj2$Data$Aggregated[c("UnitCode", agnames)]

  # get table of just indicator data
  ind_data_only <- COINobj$Data$Normalised[COINobj$Parameters$IndCodes]

  # now we want the correlations...
  out <- list(cr = cor(ind_data_only, dfres$Index, method = "pearson", use = "na.or.complete"),
              dat = dfres)

}
