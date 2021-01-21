#' Results visualisation dashboard
#'
#' Generates an interactive visualisation of results. Requires Shiny and an active R session.
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

coin_resultsdash <- function(COINobj, inames = NULL, dset = "Aggregated"){

  # first, get the indicator data from input object
  out <- coin_aux_objcheck(COINobj, dset, inames)
  ind_data_only <- out$ind_data_only
  # this is used to label scatter plot. Will need to be generalised.
  code_yr <- out$ind_data$UnitName

  rfac = 0.1 # parameter to adjust the fixed range when matching axes

  ###--------- Define the UI ---------------

  ui <- fluidPage(
    sidebarPanel(
      h2("Results Viewer"),
      hr(),
      h4("Indicator 1"),
      selectInput("dset1", "Data set", choices= rev(names(COINobj$Data)) ),
      selectInput("vr1", "Indicator", choices=rev(colnames(ind_data_only))),
      plotly::plotlyOutput("radar")
    ),

    mainPanel(
      fluidRow(
        plotly::plotlyOutput("map")
      ),
      fluidRow(
        plotly::plotlyOutput("bar")
      ),
      fluidRow(
        reactable::reactableOutput("table")
      )
    )
  )

  ###------ Define the server code -----------

  server <- function(input, output, session) {

    ## ---- First initialise and modify some values ---- ##

    # initialise reactive values: the data sets
    idata1 <- reactiveVal(ind_data_only)

    # get reactive values: the indicator names (with data set added, for plots)
    iname1 <- reactive(paste0(input$vr1," - ", input$dset1))

    # get reactive values: the selected indicator codes (for internal reference)
    isel1 <- reactive({
      if (exists(input$vr1,idata1())){
        return(input$vr1)
      } else {
        return(colnames(idata1())[1])
      }
    })

    # update data set 1 to selected one
    observeEvent(input$dset1,{
      idata1(coin_aux_objcheck(COINobj, input$dset1, inames)$ind_data_only)
    })

    ## ---- Plots and tables ---- ##

    # Choropleth map OR bar chart
    output$map <- plotly::renderPlotly({
      iplot_map(COINobj, input$dset1, isel1())
    })

    # Bar chart
    output$bar <- plotly::renderPlotly({
      iplot_bar(COINobj, input$dset1, isel1())
    })

    # Results table
    output$table <- reactable::renderReactable({
      iplot_table(COINobj, input$dset1)
    })

    # Radar plot
    output$radar <- plotly::renderPlotly({
      iplot_radar(ASEM, dset = "Aggregated", usel = event.data()$key, levsel = 2)
    })

    event.data <- reactive({plotly::event_data(event = "plotly_click", source = "mapclick")})

    ## ---- Text outputs ---- ##

    # data treatment summary, if treated data is selected
    output$treatinfoall <- reactable::renderReactable({

      return(idata1() %>%
               reactable::reactable(defaultPageSize = 5, highlight = TRUE, wrap = FALSE,
                                    rownames = FALSE, resizable = TRUE))
    })

    ## ---- Update dropdown menus ---- ##

    # Update dropdown menu of indicator selection based on data set 1
    observeEvent(input$dset1,{
      updateSelectInput(session = session, inputId = "vr1",
                        choices = rev(coin_aux_objcheck(COINobj,input$dset1,inames)$ind_names))
    })

  }

  # Return a Shiny app object
  shinyApp(ui = ui, server = server)

}

#' Choropleth map
#'
#' Generates an interactive choropleth map of specified indicator data. Only works on national
#' level data (i.e. one point per country), with countries labeled by ISO alpha-3 codes. See e.g.
#' https://www.iso.org/iso-3166-country-codes.html. This function is simply a wrapper for
#' the Plotly choropleth map function.
#'
#' @param COINobj The COIN object, or a data frame of indicator data.
#' @param dset The data set to plot.
#' @param isel The selected indicator code or aggregate
#'
#' @importFrom plotly plot_ly layout colorbar
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return Interactive map
#'
#' @export

iplot_map <- function(COINobj, dset = "Raw", isel){

  out1 <- coin_aux_objcheck(COINobj, dset = dset, inames = isel)

  # Set up appearance
  g <- list(
    showframe = FALSE,
    showcoastlines = TRUE,
    projection = list(type = 'Mercator'),
    showcountries = F,
    bgcolor = plotly::toRGB("white", alpha = 0),
    showframe = F,
    showland = T,
    landcolor = plotly::toRGB("grey90"),
    countrycolor = plotly::toRGB("white"),
    coastlinecolor = plotly::toRGB("white")
  )

  # Colourscale options:
  # Greys,YlGnBu,Greens,YlOrRd,Bluered,RdBu,Reds,Blues,Picnic,Rainbow,Portland,Jet,
  # Hot,Blackbody,Earth,Electric,Viridis,Cividis

  fig <- plotly::plot_ly(data = out1$ind_data, z = ~get(isel), locations = out1$UnitCodes,
                         type = "choropleth", text=~UnitName, colorscale="Jet",
                         source = 'mapclick', key = out1$UnitCodes)
  fig <- fig %>% plotly::colorbar(title = isel)
  fig <- fig %>% plotly::layout(geo = g)

  fig
}

#' Bar chart
#'
#' Generates an interactive bar chart. This function is simply a wrapper for
#' the Plotly bar chart function.
#'
#' @param COINobj The COIN object, or a data frame of indicator data.
#' @param dset The data set to plot.
#' @param isel The selected indicator code or aggregate
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return Interactive bar chart
#'
#' @export

iplot_bar <- function(COINobj, dset = "Raw", isel){

  out1 <- coin_aux_objcheck(COINobj, dset = dset, inames = isel)

  # Build data frame, then sort it
  df1 <- data.frame(UnitCode = out1$ind_data$UnitCode,
                    Indicator = out1$ind_data[isel])
  colnames(df1)[2] <- "Indicator"
  df1 <- df1[order(df1[2], decreasing = TRUE),]

  fig <- plotly::plot_ly(data = df1, x = ~UnitCode, y = ~Indicator,
                         source = 'barclick', key = ~UnitCode, type = "bar")
  fig <- fig %>% layout(yaxis = list(title = isel),
                 xaxis = list(title = "",
                              categoryorder = "array", categoryarray = df1$Indicator))
  fig
}


#' Results table
#'
#' Generates an interactive table of data. For use in e.g. Shiny or html documents.
#'
#' @param COINobj The COIN object, or a data frame of indicator data.
#' @param dset The data set to use in the table
#' @param isel The selected indicator codes (default all in dset)
#'
#' @importFrom reactable reactable
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return Interactive table
#'
#' @export

iplot_table <- function(COINobj, dset = "Raw", isel = NULL){

  out1 <- coin_aux_objcheck(COINobj, dset = dset, inames = isel)

  # Colour map for conditional formatting
  orange_pal <- function(x){
    if (!is.na(x)){
      rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)
    } else {
      "#e9e9e9" #grey
    }
  }

  # get data and reverse so that index is first

  tabledata <- cbind(UnitName = out1$ind_data$UnitName, rev( out1$ind_data[setdiff(names(out1$ind_data), "UnitName")] )) %>%
   lapply(function(y) if(is.numeric(y)) round(y, 1) else y) %>%
    data.frame()

  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")


  # to do: make a function which loops over columns of the data frame
  # for each col, it adds to a list using the coldef function.
  # hopefully should be able to subst whole list into the reactable function.
  # basically what I did here below, but then with looping over cols.
  # lapply probably a good bet.

  coldefs <- list(Index = reactable::colDef(
    style = function(value) {
      normalized <- (value - min(tabledata$Index)) / (max(tabledata$Index) - min(tabledata$Index))
      color <- orange_pal(normalized)
      list(background = color)
    }
  ))

  # coldefs2 <- list(1)
  # for (ii in 1:ncol(out1$ind_data_only)){
  #   #coldefs2[out1$ind_names[ii]] = 1
  #   coldefs2[out1$ind_names[ii]] = list( style = reactable::colDef(
  #     style = function(value) {
  #       normalized <- (value - min(out1$ind_data_only[ii], na.rm = T)) / (max(out1$ind_data_only[ii], na.rm = T) - min(out1$ind_data_only[ii], na.rm = T))
  #       color <- orange_pal(normalized)
  #       list(background = color)
  #     }
  #   ))
  # }
  # coldefs2 <- coldefs2[-1]

  reactable::reactable(tabledata,
            defaultSorted = "Index", defaultSortOrder = "desc",
            resizable = TRUE, bordered = TRUE,
            #columns = coldefs2,
            onClick = reactable::JS("
  function(rowInfo, colInfo, state) {
    window.alert('clicked row ' + rowInfo.index + ', column ' + colInfo.id + ', on page ' + state.page)

    // Send the click event to Shiny, which will be available at input$clicked
    // (Note that the row index starts at 0 in JavaScript, so we add 1)
    if (window.Shiny) {
      Shiny.onInputChange('clicked', { column: colInfo.id, index: rowInfo.index + 1 })
    }
  }
")

            # ),
            # Code_country = colDef(
            #   style = sticky_style,
            #   headerStyle = sticky_style
            # )


  )
}

#' Radar chart
#'
#' Generates an interactive radar chart for a specified unit.
#'
#' @param COINobj The COIN object, or a data frame of indicator data.
#' @param dset The data set to use in the table
#' @param levsel The selected aggregation level to take indicator data from,
#' where 1 is the base indicator level, and 2, 3 etc are higher aggregation levels
#'
#' @importFrom reactable reactable
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return Interactive table
#'
#' @export

iplot_radar <- function(COINobj, dset = "Raw", levsel = NULL, usel = NULL){

  if(levsel==1){
    levcodes <- ASEM$Parameters$IndCodes
  } else {
    levcodes <- ASEM$Parameters$AggCodes[[levsel-1]]
  }

  out1 <- coin_aux_objcheck(COINobj, dset = dset, inames = levcodes)

  uRow <- out1$ind_data_only[out1$UnitCodes == usel,]

  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    r = as.numeric(uRow),
    theta = colnames(uRow),
    fill = 'toself'
  )
  return(fig)
  browser()
}
