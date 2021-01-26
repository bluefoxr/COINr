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

coin_resultsdash <- function(COINobj, dset = "Aggregated"){

  # first, get the indicator data from input object
  out <- getIn(COINobj, dset)
  ind_data_only <- out$ind_data_only
  ind_data <- out$ind_data

  rfac = 0.1 # parameter to adjust the fixed range when matching axes

  ###--------- Define the UI ---------------

  ui <- fluidPage(
    titlePanel("Results dashboard"),

    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(
        selectInput("dset1", "Data set", choices= rev(names(COINobj$Data)) ),
        selectInput("vr1", "Indicator", choices=rev(colnames(ind_data_only))),
        hr(),
        h4("Unit comparison"),
        "Click a unit in the bar or map chart to plot data. Double click to clear.",
        plotly::plotlyOutput("radar"),
        column(6,
               numericInput("aglev", "Aggregation level", 1, min = 1, max = COINobj$Parameters$Nlevels-1, step = 1)
        ),
        column(6,
               selectInput("aggroup", "Aggregation group", choices= COINobj$Parameters$AggCodes[[1]] )
        )
      ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Indicator",
                             plotly::plotlyOutput("barmap",height = "600px"),
                             column(9,
                                    tableOutput("unitinfo")),
                             column(3,
                                    checkboxInput("barmapsel", "Toggle map/bar chart", value = FALSE)),
                    ),
                    tabPanel("Table", reactable::reactableOutput("table"))
        )

      )
    )
  )

  ###------ Define the server code -----------

  server <- function(input, output, session) {

    ## ---- First initialise and modify some values ---- ##

    # initialise reactive values: the data sets
    idata1 <- reactiveVal(ind_data_only)
    idata1_full <- reactiveVal(ind_data)
    # the selected unit
    usel <- reactiveVal(NULL)
    isel1 <- reactiveVal(NULL)

    # get reactive values: the indicator names (with data set added, for plots)
    iname1 <- reactive(paste0(input$vr1," - ", input$dset1))

    # update selected indicator (table click)
    observeEvent(input$vr1,{
      if (exists(input$vr1,idata1())){
        isel1(input$vr1)
      } else {
        isel1(colnames(idata1())[1])
      }
    })

    # update selected indicator (table click)
    observeEvent(input$clicked,{
      isel1(input$clicked$column)
    })

    # update data set 1 to selected one
    observeEvent(input$dset1,{
      outsel <- getIn(COINobj, input$dset1)
      idata1(outsel$ind_data_only)
      idata1_full(outsel$ind_data)
    })

    ## ---- Plots and tables ---- ##

    # Choropleth map OR bar chart
    output$barmap <- plotly::renderPlotly({
      if (input$barmapsel==TRUE){
        iplot_map(COINobj, input$dset1, isel1())
      } else {
        iplot_bar(COINobj, input$dset1, isel1(), usels())
      }
    })

    # Results table
    output$table <- reactable::renderReactable({
      iplot_table(COINobj, input$dset1)
    })

    # Radar plot
    output$radar <- plotly::renderPlotly({
      if(is.null(usels())){NULL} else {
      iplot_radar(COINobj, dset = "Aggregated", usel = usels(),
                  levsel = input$aglev, isel = input$aggroup) }
    })

    # Unit data on selected indicator
    output$unitinfo <- renderTable({
      if(is.null(usels())){data.frame(Info = "Select a unit")}
      else {
        data.frame(
          UnitName = idata1_full()$UnitName[idata1_full()$UnitCode %in% usels()],
          UnitCode = usels(),
          IndicatorCode = isel1(),
          IndicatorName = COINobj$Parameters$Code2Name$AggName[COINobj$Parameters$Code2Name$AggCode==isel1()],
          Score = idata1_full()[idata1_full()$UnitCode %in% usels(), isel1()] %>% as.numeric(),
          Rank = rank(-1*idata1_full()[,isel1()])[idata1_full()$UnitCode %in% usels()] %>%
            as.numeric() %>% round()
        )}
    })

    # get click data
    eventmap <- reactive({plotly::event_data(event = "plotly_click", source = "mapclick")})
    eventbar <- reactive({plotly::event_data(event = "plotly_click", source = "barclick")})
    eventmap2 <- reactive({plotly::event_data(event = "plotly_doubleclick", source = "mapclick")})
    eventbar2 <- reactive({plotly::event_data(event = "plotly_doubleclick", source = "barclick")})

    # consolidate click data
    observeEvent(eventmap(),{
      usel(eventmap()$key)
    })
    observeEvent(eventbar(),{
      usel(eventbar()$key)
    })
    observeEvent(input$clicked,{
      usel(idata1_full()[input$clicked$index,"UnitCode"] )
    })

    # keep track of which cars have been hovered on
    usels <- reactiveVal(NULL)

    # Collect all the selected units
    observeEvent(usel(), {
      usel_new <- usel()
      usel_old_new <- c(usels(), usel())
      usels(unique(usel_old_new))
    })

    # clear the set of units when a double-click occurs
    observeEvent(eventmap2(), {
      usels(NULL)
    })
    observeEvent(eventbar2(), {
      usels(NULL)
    })

    ## ---- Update dropdown menus ---- ##

    # Update dropdown menu of indicator selection based on data set 1
    observeEvent(input$dset1,{
      updateSelectInput(session = session, inputId = "vr1",
                        choices = rev(getIn(COINobj,input$dset1)$ind_names))
    })
    # Update selected indicator based on table click
    observeEvent(input$clicked$column,{
      updateSelectInput(session = session, inputId = "vr1", selected = input$clicked$column)
    })
    # Update dropdown menu of aggregation groups based on selected agg level
    observeEvent(input$aglev,{
      newchoices <- COINobj$Parameters$AggCodes[[input$aglev]]
      updateSelectInput(session = session, inputId = "aggroup",
                        choices = newchoices,
                        selected = newchoices[1])
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
#' @examples \dontrun{coin_indicatordash(COINobj, dset = "Raw")}
#'
#' @return Interactive map
#'
#' @export

iplot_map <- function(COINobj, dset = "Raw", isel){

  out1 <- getIn(COINobj, dset = dset, inames = isel)

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

  indname <- COINobj$Parameters$Code2Name$AggName[COINobj$Parameters$Code2Name$AggCode == isel]

  # Colourscale options:
  # Greys,YlGnBu,Greens,YlOrRd,Bluered,RdBu,Reds,Blues,Picnic,Rainbow,Portland,Jet,
  # Hot,Blackbody,Earth,Electric,Viridis,Cividis

  fig <- plotly::plot_ly(data = out1$ind_data, z = ~get(isel), locations = out1$UnitCodes,
                         type = "choropleth", text=~UnitName, colorscale="Greens",
                         source = 'mapclick', key = out1$UnitCodes, showscale = T,
                         reversescale= TRUE)
  fig <- fig %>% plotly::colorbar(title = isel)
  fig <- fig %>% plotly::layout(title = indname, geo = g)

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

iplot_bar <- function(COINobj, dset = "Raw", isel, usel){

  out1 <- getIn(COINobj, dset = dset, inames = isel)

  # Build data frame, then sort it
  df1 <- data.frame(UnitCode = out1$ind_data$UnitCode,
                    Indicator = out1$ind_data[isel])
  colnames(df1)[2] <- "Indicator"
  df1 <- df1[order(df1[2], decreasing = TRUE),]

  # colour bars based on selected units
  barcolours <- rep('#58508d', nrow(df1))
  barcolours[df1$UnitCode %in% usel] <- '#ffa600'

  # get indicator name
  indname <- COINobj$Parameters$Code2Name$AggName[COINobj$Parameters$Code2Name$AggCode == isel]

  fig <- plotly::plot_ly(data = df1, x = ~UnitCode, y = ~Indicator,
                         source = 'barclick', key = ~UnitCode, type = "bar",
                         marker = list(color = barcolours))
  fig <- fig %>% layout(title = list(text = indname, y = 0.95),
                        yaxis = list(title = isel),
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

  out1 <- getIn(COINobj, dset = dset, inames = isel)

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
            defaultSorted = colnames(tabledata)[2], defaultSortOrder = "desc",
            resizable = TRUE, bordered = TRUE, highlight = TRUE, searchable = TRUE,
            defaultPageSize = 15,
            #columns = coldefs2,
            onClick = reactable::JS("
  function(rowInfo, colInfo, state) {

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
#' @param usel Character vector of unit code(s) to plot data from
#' @param levsel The selected aggregation level to take indicator data from,
#' where 1 is the base indicator level, and 2, 3 etc are higher aggregation levels
#' @param isel The indicator or aggregation code(s) to plot
#'
#' @importFrom plotly plot_ly add_trace
#'
#' @examples \dontrun{
#' iplot_radar(ASEM, dset = "Aggregated", usel = c("AUT", "CHN"), levsel = 1, isel = "Physical")
#' }
#'
#' @return Interactive table
#'
#' @export

iplot_radar <- function(COINobj, dset = "Raw", usel = NULL, levsel = NULL, isel = NULL){

  # get indicator data
  out1 <- getIn(COINobj, dset = dset, inames = isel, aglev = levsel)

  # data to plot on radar chart (vals of each indicator/aggregate)
  uRow <- out1$ind_data_only[out1$UnitCodes %in% usel,]

  # build plot
  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    mode = "markers",
    fill = 'toself'
  )

  # Add each unit trace by looping over units
  for (ii in 1:length(usel)){
    fig <- fig %>%
      plotly::add_trace(
        r = as.numeric(uRow[ii,]),
        theta = colnames(uRow),
        name = COINobj$Input$IndData$UnitName[COINobj$Input$IndData$UnitCode == usel[ii]]
      )
  }

  # add title
  fig <- fig %>% plotly::layout(title = list(text = isel,
                                             y = 0.95, x = 0.03)) # title position

  return(fig)
}
