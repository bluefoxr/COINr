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
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return Interactive visualisation
#'
#' @export
#'

coin_indicatordash <- function(COINobj, inames = NULL, dset = "Raw"){

  # first, get the indicator data from input object
  out <- coin_aux_objcheck(COINobj, dset, inames)
  ind_data_only <- out$ind_data_only
  # this is used to label scatter plot. Will need to be generalised.
  code_yr <- out$ind_data$UnitName

  ###--------- Define the UI ---------------

  ui <- fluidPage(
    sidebarPanel(
      h2("Indicator Viewer"),
      hr(),
      h4("Indicator 1"),
      selectInput("dset1", "Data set", choices= names(COINobj$Data) ),
      selectInput("vr1", "Indicator", choices=colnames(ind_data_only)),
      #sliderInput("v1bins","Histogram bins",min = 5, max = 30, step = 1, value = 5),
      hr(),
      h4("Indicator 2"),
      selectInput("dset2", "Data set", choices= names(COINobj$Data) ),
      selectInput("vr2", "Indicator 2", choices=colnames(ind_data_only)),
      textOutput("info")
      #sliderInput("v2bins","Histogram bins",min = 5, max = 30, step = 1, value = 5)
    ),

    mainPanel(
      fluidRow(
        column(6,
               plotly::plotlyOutput("histo", height = "250px"),
               plotly::plotlyOutput("violin", height = "300px"),
               textOutput("sk"),
               textOutput("k")
        ),
        column(6,
               plotly::plotlyOutput("histo2", height = "250px"),
               plotly::plotlyOutput("violin2", height = "300px"),
               textOutput("sk2"),
               textOutput("k2")
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

    # initialise reactive values: the data sets
    idata1 <- reactiveVal(ind_data_only)
    idata2 <- reactiveVal(ind_data_only)

    # get reactive values: the indicator names
    iname1 <- reactive(paste0(input$vr1," - ", input$dset1))
    iname2 <- reactive(paste0(input$vr2," - ", input$dset2))

    # get reactive values: the selected indicators
    isel1 <- reactive({
     if (exists(input$vr1,idata1())){
       return(input$vr1)
     } else {
       return(colnames(idata1())[1])
     }
    })
    isel2 <- reactive(input$vr2)

    # update data set 1 to selected one
    observeEvent(input$dset1,{
      idata1(coin_aux_objcheck(COINobj, input$dset1, inames)$ind_data_only)
    })

    # update data set 2 to selected one
    observeEvent(input$dset2,{
      idata2(coin_aux_objcheck(COINobj, input$dset2, inames)$ind_data_only)
    })

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
        ) %>%

        plotly::layout( yaxis = list(title = "", zeroline = F) )

      fig
    })

    # Histogram v1
    output$histo <- plotly::renderPlotly({

      plot_ly(data = idata1(), x = ~get(isel1()), type = "histogram") %>%
        plotly::layout(bargap=0.1, xaxis = list(title = iname1()))
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
      ) %>%
        plotly::layout( yaxis = list(title = "", zeroline = F) )
      fig
      })

    # Histogram v2
    output$histo2 <- plotly::renderPlotly({
      plotly::plot_ly(data = idata2(), x = ~get(input$vr2), type = "histogram") %>%
        plotly::layout(bargap=0.1, xaxis = list(title = iname2()))
    })

    # scatter plot
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

    output$histo12 <- plotly::renderPlotly({

      # build data frame first, since the variables may come from different dfs
      df <- data.frame(v1 = dplyr::pull(idata1(),isel1()),
                       v2 = dplyr::pull(idata2(),input$vr2))

      fig <- plotly::plot_ly(df, alpha = 0.6)
      fig <- fig %>% plotly::add_histogram(x = ~v1, name = iname1())
      fig <- fig %>% plotly::add_histogram(x = ~v2, name = iname2())
      fig <- fig %>% plotly::layout(barmode = "overlay")
      fig
    })



    output$sk <- renderText({
      paste0("Skew = ",
             moments::skewness(idata1()[[isel1()]], na.rm = T) %>%
               round(3))
    })
    output$k <- renderText({
      paste0("Kurtosis = ",
             moments::kurtosis(idata1()[[isel1()]], na.rm = T) %>%
               round(3))
    })
    output$sk2 <- renderText({
      paste0("Skew = ",
             moments::skewness(idata2()[[input$vr2]], na.rm = T) %>%
               round(3))
    })
    output$k2 <- renderText({
      paste0("Kurtosis = ",
             moments::kurtosis(idata2()[[input$vr2]], na.rm = T) %>%
               round(3))
    })

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
