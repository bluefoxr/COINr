#' Results visualisation dashboard
#'
#' Generates an interactive visualisation of results. Requires Shiny and an active R session.
#'
#' @param COINobj The COIN object, or a data frame of indicator data.
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

        # Using two tabs here
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

    ## --------- Indicator Selection ----------------

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

    ## -------------- Unit selection -------

    # get click data
    eventmap <- reactive({plotly::event_data(event = "plotly_click", source = "mapclick", priority = "event")})
    eventbar <- reactive({plotly::event_data(event = "plotly_click", source = "barclick", priority = "event")})
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

    # initialise character vector of selected units
    usels <- reactiveVal(NULL)

    # Collect all the selected units
    observeEvent(usel(), {
      usel_new <- usel()
      # if already selected and clicked again, remove
      if (usel_new %in% usels()){
        usel_old_new <- usels()
        usel_old_new <- usel_old_new[usel_old_new != usel_new]
      } else {
        # otherwise, add to list
        usel_old_new <- c(usels(), usel())
      }
      # set selection to NULL if empty - avoids error on plot
      if(length(usel_old_new)==0){usel_old_new<-NULL}
      usels(unique(usel_old_new))
    })

    # clear the set of units when a double-click occurs
    observeEvent(eventmap2(), {
      usels(NULL)
    })
    observeEvent(eventbar2(), {
      usels(NULL)
    })

    ## ----- Plots and tables ----- ##

    # Choropleth map OR bar chart
    output$barmap <- plotly::renderPlotly({
      if (input$barmapsel==TRUE){
        iplotMap(COINobj, input$dset1, isel1())
      } else {
        iplotBar(COINobj, input$dset1, isel1(), usels())
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

iplotMap <- function(COINobj, dset = "Raw", isel){

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
#' the Plotly bar chart function, but accesses COIN object to get the relevant indicator.
#' Also has click event data for Shiny.
#'
#' @param COINobj The COIN object, or a data frame of indicator data.
#' @param dset The data set to plot.
#' @param isel The selected indicator code or aggregate (does not support multiple indicators)
#' @param usel A character vector of unit codes to highlight on the bar chart
#' @param aglev The aggregation level to collect the indicator data from
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples \dontrun{iplotBar(ASEM, dset = "Raw", isel = "Flights")}
#'
#' @return Interactive bar chart.
#'
#' @export

iplotBar <- function(COINobj, dset = "Raw", isel = NULL, usel = NULL, aglev = 1){

  out1 <- getIn(COINobj, dset = dset, inames = isel, aglev = aglev)

  ind_data_only <- out1$ind_data_only
  indname <- out1$IndNames
  ind_code <- out1$IndCodes
  browser()

  if(length(ind_code)>1){stop("This function only supports plotting single indicators. You may need to use the aglev argument if you are calling an aggregation group.")}

  if((out1$otype=="COINobj") & (aglev == 1)){
    # look for units
    if(exists("IndUnit",COINobj$Input$IndMeta)){
      # find unit for indicator
      indunit <- COINobj$Input$IndMeta$IndUnit[COINobj$Input$IndMeta$IndCode == ind_code]
    } else {
      # if not, NULL
      indunit <- ""
    }
  } else {
    # if not COINobj, no units
    indunit <- ""
  }

  # Build data frame, then sort it
  # now build df for plotly
  df1 <- data.frame(UnitCode = out1$UnitCodes,
                    Indicator = out1$ind_data[ind_code])
  colnames(df1)[2] <- "Indicator"
  df1 <- df1[order(df1[2], decreasing = TRUE),]

  # colour bars based on selected units
  barcolours <- rep('#58508d', nrow(df1))
  barcolours[df1$UnitCode %in% usel] <- '#ffa600'

  if(is.na(out1$UnitCodes[1])){
    # if there are no unitcodes, e.g. for just a df, we don't plot them
    fig <- plotly::plot_ly(data = df1, y = ~Indicator,
                           source = 'barclick', key = ~UnitCode, type = "bar",
                           marker = list(color = barcolours))
  } else {
    # otherwise, plot unit codes on x axis
    fig <- plotly::plot_ly(data = df1, x = ~UnitCode, y = ~Indicator,
                           source = 'barclick', key = ~UnitCode, type = "bar",
                           marker = list(color = barcolours))
  }

  fig <- fig %>% layout(title = list(text = indname, y = 0.95),
                        yaxis = list(title = indunit),
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
#' @examples \dontrun{iplot_table(COINobj, dset = "Aggregated", isel = NULL)}
#'
#' @return Interactive table
#'
#' @export

iplot_table <- function(COINobj, dset = "Raw", isel = NULL){

  out1 <- getIn(COINobj, dset = dset, inames = isel)

  # get data and reverse so that index is first, also unit names are first

  tabledata <- cbind(UnitName = out1$ind_data$UnitName, rev( out1$ind_data[setdiff(names(out1$ind_data), "UnitName")] )) %>%
   lapply(function(y) if(is.numeric(y)) round(y, 1) else y) %>%
    data.frame()

  # these are the attributes needed to get the left col to stick when scrolling
  # e.g. like "freeze panes" in Excel.
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")

  # now have to do sth a bit complicated to get conditional formatting

  # Colour map for conditional formatting: a function that takes a value between 0 and 1
  # and maps it to a colour according to a scale
  orange_pal <- function(x){
    if (!is.na(x)){
      grDevices::rgb(grDevices::colorRamp(c("#eefff4", "#358554"))(x), maxColorValue = 255)
    } else {
      "#e9e9e9" #grey
    }
  }

  # function which returns background colour based on cell value (using colour map)
  # also takes column name as an input, which allows to get max and min
  stylefunc <- function(value, index, name) {
    normalized <- (value - min(tabledata[name], na.rm = T)) /
      (max(tabledata[name], na.rm = T) - min(tabledata[name], na.rm = T))
    color <- orange_pal(normalized)
    list(background = color)
  }

  # list giving column formatting (using style function) for single column
  coldefs <- list(
    reactable::colDef(style = stylefunc)
  )

  # get names of numerical cols
  inumcols <- unlist(lapply(tabledata, is.numeric)) # indices of numerical cols
  numcols <- colnames(tabledata)[inumcols] # names of numerical cols
  # replicate list to required length
  coldefs <- rep(coldefs,length(numcols))
  # name elements of list according to cols
  names(coldefs) <- numcols

  # join lists: sticky first col plus cond formatting numerical cols
  coldefs <- c(coldefs,
               list(UnitName = reactable::colDef(style = sticky_style, headerStyle = sticky_style)))

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
"), columns = coldefs)
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
