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

coin_indicatordash <- function(COINobj, inames = NULL, dset = "raw"){

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
      fluidRow(
        plotly::plotlyOutput("scatter")
      )
    )
  )

  ###------ Define the server code -----------

  server <- function(input, output, session) {

    # Violin plot v1
    output$violin <- plotly::renderPlotly({

      fig <- plot_ly(data = ind_data_only, y = ~get(input$vr1), type = 'violin',
          box = list(visible = T),
          meanline = list(visible = T),
          x0 = input$vr1,
          points = 'all',
          pointpos = -1.5,
          jitter = 0.1,
          hoveron = "violins+points+kde"
        ) %>%

        plotly::layout( yaxis = list(title = "", zeroline = F) )

      fig

      #ggplot(ind_data_only, aes_string(x = input$vr1)) +
        #geom_dotplot(binaxis = "x", stackdir = "center", dotsize=1, stackratio=0.5, alpha = 0.3) + theme_light()
    })

    # Histogram v1
    output$histo <- plotly::renderPlotly({

      plot_ly(data = ind_data_only, x = ~get(input$vr1), type = "histogram") %>%
        plotly::layout(bargap=0.1, xaxis = list(title = input$vr1))

    })

    # Violin plot v2
    output$violin2 <- plotly::renderPlotly({

      fig <- plotly::plot_ly(data = ind_data_only, y = ~get(input$vr2), type = 'violin',
                     box = list(visible = T),
                     meanline = list(visible = T),
                     x0 = input$vr2,
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

      plotly::plot_ly(data = ind_data_only, x = ~get(input$vr2), type = "histogram") %>%
        plotly::layout(bargap=0.1, xaxis = list(title = input$vr2))

    })

    # scatter plot
    output$scatter <- plotly::renderPlotly({

      sc <- plotly::plot_ly(data = ind_data_only, type = 'scatter', mode = 'markers') %>%
        plotly::add_trace(
          x = ~get(input$vr1),
          y = ~get(input$vr2),
          text = code_yr,
          hoverinfo = 'text',
          marker = list(size = 15),
          showlegend = F
        ) %>%
        plotly::layout(xaxis = list(title = input$vr1),
               yaxis = list(title = input$vr2))

      sc

    })

    output$sk <- renderText({
      paste0("Skew = ",
             moments::skewness(ind_data_only[[input$vr1]], na.rm = T) %>%
               round(3))
    })
    output$k <- renderText({
      paste0("Kurtosis = ",
             moments::kurtosis(ind_data_only[[input$vr1]], na.rm = T) %>%
               round(3))
    })
    output$sk2 <- renderText({
      paste0("Skew = ",
             moments::skewness(ind_data_only[[input$vr2]], na.rm = T) %>%
               round(3))
    })
    output$k2 <- renderText({
      paste0("Kurtosis = ",
             moments::kurtosis(ind_data_only[[input$vr2]], na.rm = T) %>%
               round(3))
    })

  }

  observeEvent(input$dset1,
    updateSelectInput(session = session, inputId = "vr1",
                      choices = eval(parse(text=paste0(
                        "coin_aux_objcheck(COINobj, ",dset,")$ind_names"))))
  )

  # Return a Shiny app object
  shinyApp(ui = ui, server = server, options = list(height = 500))

}
