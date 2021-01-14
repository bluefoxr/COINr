#' Indicator visualisation dashboard
#'
#' Generates an interactive visualisation of one or two indicators at a time. Requires Shiny and an active R session.
#'
#' @param COINobj The COIN object, or a data frame of indicator data.
#' @param inames A set of indicator codes to include. Defaults to all indicators.
#' @param dset The data set to plot.
#'
#' @import shiny
#' @importFrom plotly plot_ly plotlyOutput layout add_trace renderPlotly
#' @importFrom reactable reactable renderReactable
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return Interactive visualisation
#'
#' @export

coin_indicatordash <- function(COINobj, inames = NULL, dset = "Raw"){

  # first, get the indicator data from input object
  out <- coin_aux_objcheck(COINobj, dset, inames)
  ind_data_only <- out$ind_data_only
  # this is used to label scatter plot. Will need to be generalised.
  code_yr <- out$ind_data$UnitName

  rfac = 0.1 # parameter to adjust the fixed range when matching axes

  ###--------- Define the UI ---------------

  ui <- fluidPage(
    sidebarPanel(
      h2("Indicator Viewer"),
      hr(),
      h4("Indicator 1"),
      selectInput("dset1", "Data set", choices= names(COINobj$Data) ),
      selectInput("vr1", "Indicator", choices=colnames(ind_data_only)),
      h4("Indicator 2"),
      selectInput("dset2", "Data set", choices= names(COINobj$Data) ),
      selectInput("vr2", "Indicator", choices=colnames(ind_data_only)),
      textOutput("info"),
      checkboxInput(inputId = "axmatch", label = "Plot indicators on the same axis range", value = FALSE),
      hr(),
      h4("Treated indicators"),
      reactable::reactableOutput("treatinfoall")
    ),

    mainPanel(
      fluidRow(
        column(6,
               plotly::plotlyOutput("histo", height = "250px"),
               plotly::plotlyOutput("violin", height = "300px"),
               tableOutput("treatinfo1")
        ),
        column(6,
               plotly::plotlyOutput("histo2", height = "250px"),
               plotly::plotlyOutput("violin2", height = "300px"),
               tableOutput("treatinfo2")
               )
      ),
      br(),
      fluidRow(
        column(6,
               plotly::plotlyOutput("scatter")
               ),
        column(6,
               plotly::plotlyOutput("histo12")
        )
      )
    )
  )

  ###------ Define the server code -----------

  server <- function(input, output, session) {

    ## ---- First initialise and modify some values ---- ##

    # initialise reactive values: the data sets
    idata1 <- reactiveVal(ind_data_only)
    idata2 <- reactiveVal(ind_data_only)

    # get reactive values: the indicator names (with data set added, for plots)
    iname1 <- reactive(paste0(input$vr1," - ", input$dset1))
    iname2 <- reactive(paste0(input$vr2," - ", input$dset2))

    # get reactive values: the selected indicator codes (for internal reference)
    isel1 <- reactive({
     if (exists(input$vr1,idata1())){
       return(input$vr1)
     } else {
       return(colnames(idata1())[1])
     }
    })
    isel2 <- reactive({
      if (exists(input$vr2,idata2())){
        return(input$vr2)
      } else {
        return(colnames(idata2())[1])
      }
    })

    # get reactive values: matching axes (only used in plots if requested)
    axlims <- reactive({
      # get ranges of both selected indicators
      r1 <- range(idata1()[isel1()], na.rm = TRUE)
      r2 <- range(idata2()[isel2()], na.rm = TRUE)
      # build new range which includes both
      r12 <- c( min(r1[1],r2[1], na.rm = TRUE), max(r1[2],r2[2], na.rm = TRUE) )
      # add a little bit on each end
      ran12 <- r12[2]-r12[1]
      r12[1] <- r12[1]-rfac*ran12
      r12[2] <- r12[2]+rfac*ran12
      return(r12)
    })

    # update data set 1 to selected one
    observeEvent(input$dset1,{
      idata1(coin_aux_objcheck(COINobj, input$dset1, inames)$ind_data_only)
    })

    # update data set 2 to selected one
    observeEvent(input$dset2,{
      idata2(coin_aux_objcheck(COINobj, input$dset2, inames)$ind_data_only)
    })

    ## ---- Plots ---- ##

    # Violin plot v1
    output$violin <- plotly::renderPlotly({
      fig <- plot_ly(data = idata1(), y = ~get(isel1()), type = 'violin',
          box = list(visible = T),
          meanline = list(visible = T),
          x0 = iname1(),
          points = 'all',
          pointpos = -1.5,
          jitter = 0.1,
          hoveron = "violins+points+kde"
        )

      # match axes with other variable if requested
      if (input$axmatch==TRUE){
        fig <- fig %>% plotly::layout( yaxis = list(title = "", zeroline = F, range = axlims()) )
      } else {
        fig <- fig %>% plotly::layout( yaxis = list(title = "", zeroline = F) )
      }
      fig
    })

    # Histogram v1
    output$histo <- plotly::renderPlotly({

      fig <- plot_ly(data = idata1(), x = ~get(isel1()), type = "histogram")

      # match axes with other variable if requested
      if (input$axmatch==TRUE){
        fig <- fig %>% plotly::layout(bargap=0.1, xaxis = list(title = iname1(), range = axlims()))
      } else {
        fig <- fig %>% plotly::layout(bargap=0.1, xaxis = list(title = iname1()))
      }
      fig

    })

    # Violin plot v2
    output$violin2 <- plotly::renderPlotly({

      fig <- plotly::plot_ly(data = idata2(), y = ~get(input$vr2), type = 'violin',
                     box = list(visible = T),
                     meanline = list(visible = T),
                     x0 = iname2(),
                     points = 'all',
                     pointpos = -1.5,
                     jitter = 0.1,
                     hoveron = "violins+points+kde"
      )

      # match axes with other variable if requested
      if (input$axmatch==TRUE){
        fig <- fig %>% plotly::layout( yaxis = list(title = "", zeroline = F, range = axlims()) )
      } else {
        fig <- fig %>% plotly::layout( yaxis = list(title = "", zeroline = F) )
      }
      fig
    })

    # Histogram v2
    output$histo2 <- plotly::renderPlotly({
      fig <- plot_ly(data = idata2(), x = ~get(isel2()), type = "histogram")

      # match axes with other variable if requested
      if (input$axmatch==TRUE){
        fig <- fig %>% plotly::layout(bargap=0.1, xaxis = list(title = iname2(), range = axlims()))
      } else {
        fig <- fig %>% plotly::layout(bargap=0.1, xaxis = list(title = iname2()))
      }
      fig
    })

    # scatter plot between v1 and v2
    output$scatter <- plotly::renderPlotly({

      # build data frame first, since the variables may come from different dfs
      df <- data.frame(v1 = dplyr::pull(idata1(),isel1()),
                       v2 = dplyr::pull(idata2(),input$vr2))

      sc <- plotly::plot_ly(data = df, type = 'scatter', mode = 'markers') %>%
        plotly::add_trace(
          x = ~v1,
          y = ~v2,
          text = code_yr,
          hoverinfo = 'text',
          marker = list(size = 15),
          showlegend = F
        ) %>%
        plotly::layout(xaxis = list(title = iname1()),
               yaxis = list(title = iname2()))
      sc
    })

    # Overlaid histogram of v1 and v2 together
    output$histo12 <- plotly::renderPlotly({

      # build data frame first, since the variables may come from different dfs
      df <- data.frame(v1 = dplyr::pull(idata1(),isel1()),
                       v2 = dplyr::pull(idata2(),input$vr2))

      fig <- plotly::plot_ly(df, alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = ~v1, name = iname1())
      fig <- fig %>% plotly::add_histogram(x = ~v2, name = iname2())
      fig <- fig %>% plotly::layout(barmode = "overlay",
                                    xaxis = list(title = ""))
      fig
    })

    ## ---- Text ouputs ---- ##

    # data treatment summary, if treated data is selected
    output$treatinfo1 <- renderTable({

      if(input$dset1=="Treated"){
        tbl <- COINobj$Analysis$Treatment$Summary
        # get row of indicator, remove indcode
        treatinfo <- tbl[tbl$IndCode == isel1(),-1]
        # add stats
        treatinfo <- cbind(treatinfo,
                           Skew = round( moments::skewness(idata1()[[isel1()]], na.rm = T), 3),
                           Kurtosis = round( moments::kurtosis(idata1()[[isel1()]], na.rm = T), 3))
      } else {
        # no treated data set selected, so just return skew and kurtosis
        treatinfo <- data.frame(
                           Skew = round( moments::skewness(idata1()[[isel1()]], na.rm = T), 3),
                           Kurtosis = round( moments::kurtosis(idata1()[[isel1()]], na.rm = T), 3))
      }
      return(treatinfo)
    })

    # data treatment summary, if treated data is selected
    output$treatinfo2 <- renderTable({

      if(input$dset2=="Treated"){
        tbl <- COINobj$Analysis$Treatment$Summary
        # get row of indicator, remove indcode
        treatinfo <- tbl[tbl$IndCode == isel2(),-1]
        # add stats
        treatinfo <- cbind(treatinfo,
                           Skew = round( moments::skewness(idata2()[[isel2()]], na.rm = T), 3),
                           Kurtosis = round( moments::kurtosis(idata2()[[isel2()]], na.rm = T), 3))
      } else {
        # no treated data set selected, so just return skew and kurtosis
        treatinfo <- data.frame(
          Skew = round( moments::skewness(idata2()[[isel2()]], na.rm = T), 3),
          Kurtosis = round( moments::kurtosis(idata2()[[isel2()]], na.rm = T), 3))
      }
      return(treatinfo)
    })

    # data treatment summary, if treated data is selected
    output$treatinfoall <- reactable::renderReactable({

      # show info of indicator 1 if a treated variable is selected, otherwise i2, otherwise message
      if(input$dset1=="Treated" | input$dset2=="Treated"){
        treatinfoall <- COINobj$Analysis$Treatment$Summary
        treatinfoall <- treatinfoall[treatinfoall$Treatment !="None",]
      } else {
        treatinfoall <- data.frame(IndCode = "Select a treated data set (if it exists) to view.")
      }
      return(treatinfoall %>%
               reactable::reactable(defaultPageSize = 5, highlight = TRUE, wrap = FALSE,
                                    rownames = FALSE, resizable = TRUE))
    })

    ## ---- Update dropdown menus ---- ##

    # Update dropdown menu of indicator selection based on data set 1
    observeEvent(input$dset1,{
      updateSelectInput(session = session, inputId = "vr1",
                        choices = coin_aux_objcheck(COINobj,input$dset1,inames)$ind_names)
    })

    # Update dropdown menu of indicator selection based on data set 2
    observeEvent(input$dset2,{
      updateSelectInput(session = session, inputId = "vr2",
                        choices = coin_aux_objcheck(COINobj,input$dset2,inames)$ind_names)
    })

  }

  # Return a Shiny app object
  shinyApp(ui = ui, server = server)

}