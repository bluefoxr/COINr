#' Interactive comparison of COINs
#'
#' Compares two different COIN objects in terms of scores and ranks.
#'
#' @param COINbase A named list of COIN versions, or a single COIN version
#' @param ... Further versions of the COIN.
#'
#' @import shiny
#' @importFrom plotly plot_ly plotlyOutput layout add_trace renderPlotly
#' @importFrom reactable reactable renderReactable
#'
#' @examples \dontrun{
#' compareCOINs(COIN1, COIN2, COIN3)
#' compareCOINs(list(COIN_nominal = COIN1, COIN_alt1 = COIN2))
#' }
#'
#' @return App
#'
#' @export

compareCOINs <- function(COINbase, ... = NULL){

  if ("COIN object" %in% class(COINbase)){
    # if the first obj is a COIN object, then inputs are a series of objs
    # So, build list out of inputs (using names of objects as list names)
    COINs <- tibble::lst(COINbase, ...)
  } else if ("list" %in% class(COINbase)){
    # its a list of objects
    COINs <- COINbase
  } else {
    stop("Something has gone wrong. Input not recognised.")
  }

  COIN1init <- COINs[[1]]
  COIN2init <- COINs[[2]]

  ui <- fluidPage(

    titlePanel("COIN Comparison"),
    tabsetPanel(type = "tabs",
                tabPanel("Overview",

                         sidebarPanel(width = 3,
                                      "This tab compares multiple COINs in terms of
                                      index ranks. The baseline COIN (selectable below) is used as the
                                      reference to calculate rank changes. The Table type allows you
                                      to view either ranks, rank differences (with baseline) and absolute
                                      rank differences. Table is sortable and searchable.",
                                      selectInput("COINbase", "Baseline", choices= names(COINs), selected = names(COINs)[1]),
                                      selectInput("tabtype", "Table type", choices= c("Ranks", "Diffs", "AbsDiffs"),
                                                  selected = "Ranks")
                         ),
                         mainPanel(width = 9,
                                   reactable::reactableOutput("overalltable")
                         )# mainpanel

                ), # tabpanel
                tabPanel("Pairwise",
                         sidebarPanel(width = 3,
                                      selectInput("COIN1", "Version 1", choices= names(COINs), selected = names(COINs)[1]),
                                      selectInput("COIN2", "Version 2", choices= names(COINs), selected = names(COINs)[2] ),
                                      selectInput("v1", "Comparison indicator", choices= rev(getIn(COIN1init, "Aggregated")$ind_names) )
                         ),
                         mainPanel(width = 9,
                                   column(5,
                                          fluidRow(
                                            plotly::plotlyOutput("scatter")
                                          )
                                   ),
                                   column(7,
                                          reactable::reactableOutput("ranktable1"),
                                          tableOutput("rankstats")
                                   )
                         )# mainpanel
                ) # tabpanel

    ) # tabsetpanel
  ) # fluidpage

  ###------ Define the server code -----------

  server <- function(input, output, session) {

    # get first selected COIN version
    COIN1 <- reactive({ COINs[[input$COIN1]] })
    # get second selected COIN version
    COIN2 <- reactive({ COINs[[input$COIN2]] })

    # First selected variable
    v1 <- reactive({ COIN1()$Data$Aggregated[input$v1] })

    # Second selected variable
    v2 <- reactive({ COIN2()$Data$Aggregated[input$v1] })

    # Build data frame of both selected variables, plus names, for plotting
    df <- reactive({
      data.frame(UnitName = COIN1$Data$Aggregated$UnitName,
                 vr1 = v1(),
                 vr2 = v2())
    })

    # overall comparison table
    output$overalltable <- renderReactable({

      # index of COIN to select as baseline
      ibase <- which(names(COINs)==input$COINbase)

      # get table using function
      compTableMulti(COINs, tabtype = input$tabtype, ibase = ibase) %>%
        reactable(defaultPageSize = 20, highlight = TRUE, wrap = F, showSortable = TRUE,
                  resizable = TRUE, outlined = TRUE, searchable = TRUE, pagination = FALSE,
                  height = 800)
    })

    # scatter plot between v1 and v2
    output$scatter <- plotly::renderPlotly({

      df1 <- data.frame(UnitName = COIN1()$Data$Aggregated$UnitName,
                        vr1 = rank(-1*v1()),
                        vr2 = rank(-1*v2()))

      sc <- plotly::plot_ly(data = df1, type = 'scatter', mode = 'markers') %>%
        plotly::add_trace(
          x = ~get(colnames(df1)[2]),
          y = ~get(colnames(df1)[3]),
          text = df1$UnitName,
          hoverinfo = 'text',
          marker = list(size = 10,
                        opacity = 0.5,
                        color = 'rgb(17, 157, 255)'),
          showlegend = F
        ) %>%
        plotly::layout(title = input$v1,
                       xaxis = list(title = input$COIN1, autorange = "reversed"),
                       yaxis = list(title = input$COIN2, autorange = "reversed"))
      sc
    })

    # rank table
    output$ranktable1 <- renderReactable({
      compTable(COIN1(),COIN2(), dset = "Aggregated", isel = input$v1,
                COINnames = c(input$COIN1, input$COIN2)) %>%
        reactable(defaultPageSize = 20, highlight = TRUE, wrap = F,
                  #defaultSorted = list(Index = "desc"),
                  searchable = TRUE
        )
    })

    # rank stats
    output$rankstats <- renderTable({
      rkch <- compTable(COIN1(),COIN2(), dset = "Aggregated", isel = input$v1)$AbsRankChange
      data.frame(Min = min(rkch, na.rm = T),
                 Max = max(rkch, na.rm = T),
                 Mean = mean(rkch, na.rm = T),
                 Median = stats::median(rkch, na.rm = T))
    })

  }

  # Return a Shiny app object
  shinyApp(ui = ui, server = server)
}


#' Rank comparison table between 2 COINs
#'
#' Takes two COINs, and generates a rank comparison between specified indicator/aggregate
#'
#' @param COIN1 First COIN obj
#' @param COIN2 Second COIN obj
#' @param dset The data set of interest
#' @param isel The indicator/col of interest
#' @param COINnames An optional character vector of the names of COIN1 and COIN2, to be used in the table headers.
#' @param sort_by If "RankCOIN1", sorts by the indicator values of COIN1, if "RankCOIN2", sorts by COIN2,
#' if "RankChange", sorts by rank change, and if "AbsRankChange" sorts by absolute rank change.
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return A data frame with ranks and rank changes
#'
#' @export
#'
compTable <- function(COIN1, COIN2, dset = "Raw", isel, COINnames = NULL, sort_by = "AbsRankChange"){

  tab1 <- COIN1$Data[[dset]][c("UnitCode", "UnitName", isel)]
  tab2 <- COIN2$Data[[dset]][c("UnitCode", "UnitName", isel)]

  # join the two tables
  df1 <- merge(tab1, tab2, by = c("UnitCode", "UnitName"))
  # convert scores to ranks
  df1 <- rankDF(df1)
  # add diff and abs diff
  df1 <- cbind(df1,
               df1[3] - df1[4],
               abs(df1[3] - df1[4]))
  # tidy up
  colnames(df1) <- c("UnitCode", "UnitName", "RankCOIN1", "RankCOIN2", "RankChange", "AbsRankChange")

  # sort
  if(sort_by == "RankCOIN1" | sort_by == "RankCOIN2"){
    df1 <- df1[order(df1[[sort_by]]),]
  } else {
    df1 <- df1[order(-df1[[sort_by]]),]
  }


  if (!is.null(COINnames)){
    colnames(df)[3:4] <- paste0("Rank:",COINnames)
  }

  df1
}

#' Rank tables between multiple COINs
#'
#' Takes multiple COINs, and generates a rank comparison for a single indicator or aggregate.
#'
#' @param COINs A list of COINs
#' @param dset The data set to extract the indicator from (must be present in each COIN). Default "Aggregated".
#' @param isel Code of the indicator or aggregate to extract from each COIN (must be present in the specified
#' data set of each COIN). Default "Index".
#' @param tabtype The type of table to generate - "Ranks", "Diffs", or "AbsDiffs".
#' @param ibase The index of the COIN to use as a base comparison
#' @param sort_by If TRUE, sorts by the base COIN (ibase), if "change" (default).
#' @param extra_cols A character vector of any extra columns to include from the COIN referenced by ibase. For example,
#' this could include group columns.
#'
#' @importFrom purrr modify_if
#'
#' @return Rank comparison table as a data frame
#'
#' @export

compTableMulti <- function(COINs, dset = "Aggregated", isel = "Index", tabtype = "Ranks", ibase = 1,
                           sort_table = TRUE, extra_cols = NULL){

  # change order of list: put ibase first
  COINs <- COINs[c(ibase, setdiff(1:length(COINs), ibase))]

  # names
  if(is.null(names(COINs))){
    names(COINs) <- paste0("COIN_", 1:length(COINs))
  }

  # get scores of baseline COIN
  tab1 <- COINs[[1]]$Data[[dset]][c("UnitCode", "UnitName", extra_cols, isel)]
  colnames(tab1)[ncol(tab1)] <- names(COINs)[1]

  # now loop over COINs to get the other columns
  for (ii in 2:length(COINs)){

    # get indicator data for iith COIN
    tabi <- COINs[[ii]]$Data[[dset]][c("UnitCode", isel)]

    # join the two tables
    tab1 <- merge(tab1, tabi, by = "UnitCode")

    # rename col
    colnames(tab1)[ncol(tab1)] <- names(COINs)[ii]
  }

  # convert to ranks
  tab1 <- rankDF(tab1)

  # if tabtype is not "Ranks", have to do a further step
  if  (tabtype == "Diffs"){

    # calculate rank differences
    tab1 <- purrr::modify_if(tab1,
                             .p = is.numeric,
                             .f = ~{tab1[names(COINs)[1]] - .x}
    )

  } else if (tabtype == "AbsDiffs"){

    # calculate abs rank differences
    tab1 <- purrr::modify_if(tab1,
                             .p = is.numeric,
                             .f = ~{abs(tab1[names(COINs)[1]] - .x)}
                             )
  }

  # sort
  if(sort_table){
    tab1 <- tab1[order(tab1[[names(COINs)[1]]]),]
  }

  return(tab1)
}
