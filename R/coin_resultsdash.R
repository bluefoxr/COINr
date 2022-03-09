#' Results visualisation dashboard
#'
#' Generates an interactive visualisation of results. Requires Shiny and an interactive R session.
#'
#' This function provides a fast way to present and explore results in a COIN. It plots interactive bar charts of any indicator
#' or aggregate, and maps if the unit codes are ISO alpha-3 country codes. It also includes an interactive results table, and the
#' possibility to quickly compare units on a radar chart.
#'
#' @param COIN The COIN object
#' @param dset The initial data set to explore.
#'
#' @import shiny
#' @importFrom plotly plot_ly plotlyOutput layout add_trace renderPlotly
#' @importFrom reactable reactable renderReactable
#'
#' @examples
#' # To be run in an interactive R session...
#' if(interactive()){
#' # build ASEM up to results
#' ASEM <- build_ASEM()
#' # launch results dashboard
#' resultsDash(ASEM)
#' }
#'
#' @return Interactive app for exploring results (if running an interactive R session). Otherwise will simply generate a message.
#'
#' @seealso
#' * [indDash()] shiny dashboard for exploring indicator distributions
#' * [rew8r()] shiny dashboard for altering weights and visualising correlations
#'
#' @export

resultsDash <- function(COIN, dset = "Aggregated"){

  if(interactive()){

    if(!is.COIN(COIN)){
      stop("This function only supports COINs as inputs.")
    }

    # first, get the indicator data from input object
    out <- getIn(COIN, dset)
    ind_data_only <- out$ind_data_only
    ind_data <- out$ind_data

    ###--------- Define the UI ---------------

    ui <- fluidPage(
      titlePanel("Results dashboard"),

      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
          selectInput("dset1", "Data set", choices= rev(names(COIN$Data)) ),
          selectInput("vr1", "Indicator", choices=rev(colnames(ind_data_only))),
          hr(),
          h4("Unit comparison"),
          "Click a unit in the bar or map chart to plot data. Double click to clear.",
          plotly::plotlyOutput("radar"),
          column(6,
                 numericInput("aglev", "Aggregation level", 1, min = 1, max = COIN$Parameters$Nlevels-1, step = 1)
          ),
          column(6,
                 selectInput("aggroup", "Aggregation group", choices= COIN$Parameters$AggCodes[[1]] )
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
                                      checkboxInput("barmapsel", "Toggle map/bar chart", value = FALSE),
                                      checkboxInput("stack_kids", "Show component scores (bar only)", value = FALSE)),
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
        outsel <- getIn(COIN, input$dset1)
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
          iplotMap(COIN, input$dset1, isel1())
        } else {

          # need to find aglev
          if (isel1() %in% COIN$Input$IndMeta$IndCode){
            aglev1 <- 1
          } else {
            aglev1 <- COIN$Input$AggMeta$AgLevel[COIN$Input$AggMeta$Code == isel1()]
          }

          if(input$stack_kids & aglev1 > 1){
            iplotBar(COIN, dset = input$dset1, isel = isel1(), usel = usels(), aglev = aglev1, stack_children = TRUE)
          } else {
            iplotBar(COIN, dset = input$dset1, isel = isel1(), usel = usels(), aglev = aglev1)
          }

        }
      })

      # Results table
      output$table <- reactable::renderReactable({
        iplotTable(COIN, input$dset1)
      })

      # Radar plot
      output$radar <- plotly::renderPlotly({
        if(is.null(usels())){NULL} else {
          iplotRadar(COIN, dset = "Aggregated", usel = usels(),
                     aglev = input$aglev, isel = input$aggroup) }
      })

      # Unit data on selected indicator
      output$unitinfo <- renderTable({
        if(is.null(usels())){data.frame(Info = "Select a unit")}
        else {
          data.frame(
            UnitName = idata1_full()$UnitName[idata1_full()$UnitCode %in% usels()],
            UnitCode = idata1_full()$UnitCode[idata1_full()$UnitCode %in% usels()],
            IndicatorCode = isel1(),
            IndicatorName = COIN$Parameters$Code2Name$AggName[COIN$Parameters$Code2Name$AggCode==isel1()],
            Score = idata1()[idata1_full()$UnitCode %in% usels(), isel1()] %>% dplyr::pull(1),
            Rank = rank(-1*idata1_full()[,isel1()])[idata1_full()$UnitCode %in% usels()] %>%
              as.numeric() %>% round()
          )}
      })

      ## ---- Update dropdown menus ---- ##

      # Update dropdown menu of indicator selection based on data set 1
      observeEvent(input$dset1,{
        updateSelectInput(session = session, inputId = "vr1",
                          choices = rev(getIn(COIN,input$dset1)$ind_names))
      })
      # Update selected indicator based on table click
      observeEvent(input$clicked$column,{
        updateSelectInput(session = session, inputId = "vr1", selected = input$clicked$column)
      })
      # Update dropdown menu of aggregation groups based on selected agg level
      observeEvent(input$aglev,{
        newchoices <- COIN$Parameters$AggCodes[[input$aglev]]
        updateSelectInput(session = session, inputId = "aggroup",
                          choices = newchoices,
                          selected = newchoices[1])
      })

    }

    # Return a Shiny app object
    shinyApp(ui = ui, server = server)

  } else {
    message("resultsDash() only works in interactive mode. Open an interactive R session to view the app.")
  }

}

#' Choropleth map
#'
#' Generates an interactive choropleth map of specified indicator data. Only works on national
#' level data (i.e. one point per country), with countries labelled by [ISO alpha-3 codes](https://www.iso.org/iso-3166-country-codes.html).
#' This function is simply a quick wrapper for the **plotly** choropleth map function.
#'
#' @param COIN The COIN object, or a data frame of indicator data.
#' @param dset The data set to plot.
#' @param isel The selected indicator code or aggregate
#'
#' @importFrom plotly plot_ly layout colorbar
#'
#' @examples
#' # assemble ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # map CO2 indicator
#' iplotMap(ASEM, dset = "Raw", isel = "CO2")
#'
#' @return Interactive map generated by plotly.
#'
#' @seealso
#' * [iplotBar()] bar chart of indicator or aggregate
#' * [resultsDash()] interactive dashboard of indicator data
#'
#' @export

iplotMap <- function(COIN, dset = "Raw", isel){

  out1 <- getIn(COIN, dset = dset, icodes = isel)

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

  indname <- COIN$Parameters$Code2Name$AggName[COIN$Parameters$Code2Name$AggCode == isel]

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
#' the **plotly** bar chart function, but accesses the COIN object to get the relevant indicator.
#' Also has click event data for Shiny. Allows construction of stacked bar charts which show underlying components (for aggregated
#' data only), and plots of only specified groups.
#'
#' @param COIN The COIN object, or a data frame of indicator data.
#' @param dset The data set to plot.
#' @param isel The selected indicator code or aggregate (does not support multiple indicators)
#' @param usel A character vector of unit codes to highlight on the bar chart (optional)
#' @param aglev The aggregation level to collect the indicator data from (this needs to be specified)
#' @param stack_children If `TRUE`, produces a stacked bar chart with any children of `isel.` In this case, `usel` is ignored.
#' This only works if `dset = "Aggregated"` and `aglev > 1`.
#' @param from_group Filters the bar chart to a specified group using a group column that is present in the specified
#' data set. Specified as `list(group_variable = selected_group)`.
#' @param xlabs Tick labels to display on x axis: either `"UnitCode"` or `"UnitName"`.
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples
#' # assemble ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # plot Flights indicator
#' iplotBar(ASEM, dset = "Raw", isel = "Flights", aglev = 1)
#'
#' @seealso
#' * [iplotMap()] bar chart of indicator or aggregate
#' * [resultsDash()] interactive dashboard of indicator data
#'
#' @return Interactive bar chart generated by plotly.
#'
#' @export

iplotBar <- function(COIN, dset = "Raw", isel = NULL, usel = NULL, aglev = NULL,
                     stack_children = FALSE, from_group = NULL, xlabs = "UnitCode"){

  stopifnot(xlabs %in% c("UnitCode", "UnitName"))

  # for a COIN we need to know the aggregation level to make things a bit easier later on
  if(is.null(aglev) & (class(COIN) == "COIN")){
    stop("aglev must be specified if a COIN is input.")
  }

  out1 <- getIn(COIN, dset = dset, icodes = isel, aglev = aglev)

  ind_data_only <- out1$ind_data_only
  indname <- out1$IndNames
  ind_code <- out1$IndCodes

  if(length(ind_code)>1){stop("This function only supports plotting single indicators. You may need to use the aglev argument if you are calling an aggregation group.")}

  # look for units
  if((out1$otype=="COINobj")){
    # look for units
    if(exists("IndUnit",COIN$Input$IndMeta) & (aglev == 1)){
      # find unit for indicator
      indunit <- COIN$Input$IndMeta$IndUnit[COIN$Input$IndMeta$IndCode == ind_code]
    } else {
      # if not, NULL
      indunit <- ""
    }
  } else {
    # if not COIN, no units
    indunit <- ""
  }

  if(stack_children){

    if(aglev == 1 | !(dset %in% c("Aggregated", "PreAggregated"))){
      stop("Stacked bar chart only works with aglev > 1 and dset = 'Aggregated'.")
    }

    # if stack_children is TRUE and we are above the indicator level, stacked bar chart with children
    # First we need to find who the children are
    # Get index structure
    aggcols <- dplyr::select(COIN$Input$IndMeta,
                             dplyr::starts_with(c("IndCode", "Agg")))
    aggcols <- aggcols[c(aglev-1, aglev)]

    # get children
    children <- unique(aggcols[aggcols[2] == ind_code, 1])
    children <- children[[1]]

    # get scores for children
    out_children <- getIn(COIN, dset = dset, icodes = children, aglev = aglev-1)

    #df1 <- data.frame(UnitCode = out_children$UnitCodes, Indicator = ind_data_only, out_children$ind_data_only)

    df1 <- merge(out_children$ind_data, out1$ind_data)

    # filter to a group, if asked
    if(!is.null(from_group)){

      # checks
      stopifnot(is.list(from_group),
                length(from_group)==1,
                !is.null(names(from_group)),
                is.character(from_group[[1]]),
                names(from_group)[1] %in% colnames(df1))

      # filter to group
      df1 <- df1[ df1[names(from_group)[1]] == from_group[[1]], ]

      # alter title
      bartitle <- paste0(indname, "<br>(", from_group[[1]], ")" )
    } else {
      bartitle <- indname
    }


    #colnames(df1)[2] <- "Indicator"
    df1 <- df1[order(df1[[ind_code]], decreasing = TRUE),]

    fig <- plotly::plot_ly(data = df1, x = ~get(xlabs), y = ~get(children[1]),
                           source = 'barclick', key = ~UnitCode, type = "bar",
                           name = children[1])


    for (iii in 2:length(children)){

      fig <- plotly::add_trace(fig, y = df1[[children[iii]]], name = children[iii])

    }

    fig <- fig %>% plotly::layout(title = list(text = bartitle, y = 0.95),
                          yaxis = list(title = indunit, showticklabels = FALSE),
                          xaxis = list(title = "",
                                       categoryorder = "array", categoryarray = df1[ind_code]),
                          barmode = "stack")

  } else {

    # Build data frame, then sort it
    # now build df for plotly
    df1 <- out1$ind_data

    # filter to a group, if asked
    if(!is.null(from_group)){

      # checks
      stopifnot(is.list(from_group),
                length(from_group)==1,
                !is.null(names(from_group)),
                is.character(from_group[[1]]),
                names(from_group)[1] %in% colnames(df1))

      # filter to group
      df1 <- df1[ df1[names(from_group)[1]] == from_group[[1]], ]
    }

    # colnames(df1)[2] <- "Indicator"
    df1 <- df1[order(df1[[ind_code]], decreasing = TRUE),]

    # colour bars based on selected units
    barcolours <- rep('#58508d', nrow(df1))
    barcolours[df1$UnitCode %in% usel] <- '#ffa600'

    if(is.na(out1$UnitCodes[1])){
      # if there are no unitcodes, e.g. for just a df, we don't plot them
      fig <- plotly::plot_ly(data = df1, y = ~get(ind_code),
                             source = 'barclick', key = ~UnitCode, type = "bar",
                             marker = list(color = barcolours))
    } else {
      # otherwise, plot unit codes on x axis
      fig <- plotly::plot_ly(data = df1, x = ~get(xlabs), y = ~get(ind_code),
                             source = 'barclick', key = ~UnitCode, type = "bar",
                             marker = list(color = barcolours))
    }


    fig <- fig %>% layout(title = list(text = indname, y = 0.95),
                          yaxis = list(title = indunit),
                          xaxis = list(title = "",
                                       categoryorder = "array", categoryarray = df1[ind_code]))

  }


  fig
}


#' Results table
#'
#' Generates an interactive table of data. For use in e.g. Shiny or HTML documents.
#'
#' This function is a wrapper for the **reactable** package and offers a fast way to make interactive tables. It also applies
#' conditional formatting (colouring by cell value), and sorts by the first column by default. Like other **COINr** functions,
#' it can target subsets of indicators.
#'
#' @param COIN The COIN object, or a data frame of indicator data.
#' @param dset The data set to use in the table
#' @param isel The selected indicator codes (default all in `dset`)
#' @param aglev The aggregation level to select data from
#' @param nround The number of decimal places to round scores to (default 1).
#' @param extra_cols If `FALSE` (default), excludes group columns and similar. If `TRUE`, includes them.
#'
#' @importFrom reactable reactable
#'
#' @examples
#' # assemble ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # table of indicators in "Poltical" pillar
#' iplotTable(ASEM, dset = "Raw", isel = "Political", aglev = 1)
#'
#' @seealso
#' * [colourTable()] Conditionally-formatted table for any data frame
#' * [resultsDash()] interactive dashboard of indicator data
#' * [getResults()] results summary tables
#'
#' @return An interactive table generated by reactable.
#'
#' @export

iplotTable <- function(COIN, dset = "Raw", isel = NULL, aglev = NULL, nround = 1,
                       extra_cols = FALSE){

  # reverse order of selected indicators (if any): necessary because later we reverse
  if(!is.null(isel)){
    isel <- rev(isel)
  }

  # get indicator data
  out1 <- getIn(COIN, dset = dset, icodes = isel, aglev = aglev)

  # get data and reverse so that index is first, also unit names are first

  if(extra_cols == TRUE){
    tabledata <- cbind(UnitName = out1$ind_data$UnitName, rev( out1$ind_data[setdiff(names(out1$ind_data), "UnitName")] )) %>%
      roundDF(nround)
  } else {
    tabledata <- cbind(UnitName = out1$ind_data$UnitName, rev( out1$ind_data_only )) %>%
      roundDF(nround)
  }

  colourTable(tabledata)

}


#' Conditionally formatted table
#'
#' Given a data frame, generates a conditionally-formatted html table using reactable. This function is used by
#' [iplotTable()]. It is a quick wrapper for [reactable::reactable].
#'
#' @param df A data frame to be displayed as a table.
#' @param freeze1 If `TRUE` (default), freezes the first column. This may be for example the unit name or code.
#' @param sortcol A column name to sort the table by. Defaults to first numerical column. Set to `"none"` to disable.
#' @param sortorder Either `"desc"` for sorted column to be sorted from high to low (default) or `"asc"` for the opposite.
#' @param searchable If `TRUE`, includes a search box
#' @param pagesize The number of rows to display on each page.
#' @param cell_colours A character vector of at least two colour codes (e.g. Hex codes) to use for the colour palette. Should be in
#' order of low to high values. Defaults to a simple green palette of `c("#eefff4", "#358554")`. See [grDevices::colorRamp()] for more info.
#' @param reverse_colours If `TRUE`, reverses the colour map - useful for rank tables where lowest numbers mean high scores.
#'
#' @importFrom reactable reactable
#'
#' @examples
#' # some random data
#' df <-  as.data.frame(matrix(runif(12), 3, 4))
#' # a names column
#' df <- cbind(Rnames = letters[1:3], df)
#' # round it
#' df <- roundDF(df)
#' # make a table
#' colourTable(df)
#'
#' @seealso
#' * [iplotTable()] Interactive table of indicator data (from a COIN)
#'
#' @return An interactive table generated by reactable.
#'
#' @export

colourTable <- function(df, freeze1 = TRUE, sortcol = NULL, sortorder = "desc", searchable = TRUE,
                        pagesize = 10, cell_colours = NULL, reverse_colours = FALSE){

  # these are the attributes needed to get the left col to stick when scrolling
  # e.g. like "freeze panes" in Excel.
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")

  # now have to do sth a bit complicated to get conditional formatting

  if(is.null(cell_colours)){
    cell_colours <- c("#eefff4", "#358554")
  }
  if(reverse_colours){
    cell_colours <- rev(cell_colours)
  }

  # Colour map for conditional formatting: a function that takes a value between 0 and 1
  # and maps it to a colour according to a scale
  orange_pal <- function(x){
    if (!is.na(x)){
      grDevices::rgb(grDevices::colorRamp(cell_colours)(x), maxColorValue = 255)
    } else {
      "#e9e9e9" #grey
    }
  }

  # function which returns background colour based on cell value (using colour map)
  # also takes column name as an input, which allows to get max and min
  stylefunc <- function(value, index, name) {
    normalized <- (value - min(df[name], na.rm = T)) /
      (max(df[name], na.rm = T) - min(df[name], na.rm = T))
    color <- orange_pal(normalized)
    list(background = color)
  }

  # list giving column formatting (using style function) for single column
  coldefs <- list(
    reactable::colDef(style = stylefunc)
  )

  # get names of numerical cols
  inumcols <- unlist(lapply(df, is.numeric)) # indices of numerical cols
  numcols <- colnames(df)[inumcols] # names of numerical cols
  # replicate list to required length
  coldefs <- rep(coldefs,length(numcols))
  # name elements of list according to cols
  names(coldefs) <- numcols

  # sticky first column
  if (freeze1){
    # name of first col
    freezecol <- colnames(df)[1]
    # make sticky
    coldefs[[freezecol]] <- reactable::colDef(style = sticky_style, headerStyle = sticky_style)
  }

  if(is.null(sortcol)){
    sortcol <- numcols[1]
  } else if (sortcol == "none"){
    sortcol <- NULL
  }

  reactable::reactable(df,
                       defaultSorted = sortcol, defaultSortOrder = sortorder,
                       resizable = TRUE, bordered = TRUE, highlight = TRUE, searchable = searchable,
                       defaultPageSize = pagesize,
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
#' Generates an interactive radar chart for a specified unit or set of units.
#'
#' This function uses **plotly** to generate a radar chart for showing one or more units, compared using a specified set of indicators.
#' Optionally, you can add mean/median or group mean/median as an extra trace. The point being to show how a particular unit compares to
#' its peers.
#'
#' @param COIN The COIN object, or a data frame of indicator data.
#' @param dset The data set to use in the table
#' @param usel Character vector of unit code(s) to plot data from
#' @param isel The indicator or aggregation code(s) to plot
#' @param aglev The selected aggregation level to take indicator data from,
#' where 1 is the base indicator level, and 2, 3 etc are higher aggregation levels
#' @param addstat Adds the statistic of the scores in each dimension as a separate trace. If `"mean"` adds the overall
#' mean for each dimension/indicator. If `"median"` adds the overall median. If `"groupmean"` or `"groupmedian"`, adds the
#' group mean or median respectively of the first unit specified in `usel`, using the group specified by `statgroup`. Default `"none"`, i.e. no extra trace.
#' Using a group mean or median won't make sense unless all of selected units are from the same group.
#' @param statgroup A grouping variable (must be present in `dset`) if `addstat = "groupmean"` or `"groupmedian"`
#' @param statgroup_name An optional name to display for `statgroup.` In the legend this will appear as e.g. `"statgroup_name group mean"`.
#' Defaults to `statgroup`.
#'
#' @importFrom plotly plot_ly add_trace
#'
#' @examples
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # radar chart of Austria vs. China in Political indicators
#' iplotRadar(ASEM, dset = "Aggregated", usel = c("AUT", "CHN"), isel = "Political", aglev = 1)
#'
#' @return Interactive radar chart generated using plotly.
#'
#' @seealso
#' * [resultsDash()] Interactive results dashboard.
#'
#' @export

iplotRadar <- function(COIN, dset = "Raw", usel = NULL, aglev = NULL, isel = NULL, addstat = "none",
                       statgroup = NULL, statgroup_name = NULL){

  if(is.null(usel)){
    stop("You need to select a unit using usel.")
  }

  # get indicator data
  out1 <- getIn(COIN, dset = dset, icodes = isel, aglev = aglev)

  # data to plot on radar chart (vals of each indicator/aggregate)
  uRow <- out1$ind_data_only[out1$ind_data$UnitCode %in% usel,]
  uNames <- out1$ind_data$UnitName[out1$ind_data$UnitCode %in% usel]

  if(addstat == "mean"){

    uRow <- rbind(colMeans(out1$ind_data_only, na.rm = TRUE),
                  uRow)
    uNames <- c("<b>Mean<b>", uNames)

  } else if (addstat == "median"){

    uRow <- rbind(apply(out1$ind_data_only, 2, stats::median, na.rm = T),
                  uRow)
    uNames <- c("<b>Median<b>", uNames)

  } else if ((addstat == "groupmean") | (addstat == "groupmedian")){

    # trap some errors first
    if(is.null(statgroup)){
      stop("You didn't specify which grouping variable to use. Use statgroup argument.")
    }
    if(!(statgroup %in% colnames(out1$ind_data))){
      stop("Specified statgroup not found in the selected data set - please check.")
    }

    # first need to find which group usel is in. Use the first unit in usel as reference.
    group_usel <- out1$ind_data[out1$ind_data$UnitCode == usel[1] ,statgroup]
    group_usel <- group_usel[[1]]

    # get df with just the indicators, and rows belonging to specified group
    groupdata <- out1$ind_data[out1$ind_data[[statgroup]] == group_usel, out1$IndCodes]

    # get group name - use statgroup_name or if NULL use statgroup (will start with "Group_" though)
    if(is.null(statgroup_name)){
      statgroup_name = statgroup
    }

    if (addstat == "groupmean"){

      uRow <- rbind(apply(groupdata, 2, mean, na.rm = T),
                    uRow)
      uNames <- c(paste0("<b>", statgroup_name, " group mean (", group_usel, ")<b>"),
                  uNames)

    } else if (addstat == "groupmedian"){

      uRow <- rbind(apply(groupdata, 2, stats::median, na.rm = T),
                    uRow)
      uNames <- c(paste0("<b>", statgroup_name, " group median<b>"),
                  uNames)

    }

  }

  # build plot
  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    mode = "markers",
    fill = 'toself'
  )

  # Add each unit trace by looping over units
  for (ii in 1:length(uNames)){
    fig <- fig %>%
      plotly::add_trace(
        r = as.numeric(uRow[ii,]),
        theta = colnames(uRow),
        name = uNames[ii]
        #name = COIN$Input$IndData$UnitName[COIN$Input$IndData$UnitCode == usel[ii]]
      )
  }

  # add title
  fig <- fig %>% plotly::layout(title = list(text = isel,
                                             y = 0.95, x = 0.03)) # title position

  return(fig)
}
