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
#'
#' @examples \dontrun{coin_indicatordash(COINobj, inames = NULL, dset = "raw")}
#'
#' @return A data frame with ranks and rank changes
#'
#' @export
#'
compTable <- function(COIN1, COIN2, dset = "Raw", isel, COINnames = NULL){

  #NOTE: rewrite this to bypass getIn, and to do ranks here. Also ensure that isel is a string.
  stop("This function is not working at the moment - will fix soon.")

  out1 <- getIn(COIN1, dset = dset)
  out2 <- getIn(COIN2, dset = dset)

  df <- data.frame(Country = out1$UnitNames,
                   out1$ind_ranks[isel],
                   out2$ind_ranks[isel],
                   out1$ind_ranks[isel] - out2$ind_ranks[isel],
                   abs(out1$ind_ranks[isel] - out2$ind_ranks[isel]))
  colnames(df) <- c("Country", "RankCOIN1", "RankCOIN2", "RankChange", "AbsRankChange")
  if (!is.null(COINnames)){
    colnames(df)[2:3] <- paste0("Rank:",COINnames)
  }

  # outtab <- dat$ind_ranks %>% rev() %>%
  #   data.frame(Country = dat$UnitNames) %>%
  #   reactable(defaultPageSize = 10, highlight = TRUE, wrap = F,
  #             defaultSorted = list(Index = "desc"))
  df
}

#' Rank tables between multiple COINs
#'
#' Takes multiple COINs, and generates a rank comparison at the index level. Note, at the moment this
#' only works if your index is called "Index". This will be fixed at some point.
#'
#' @param COINs A list of COINs
#' @param tabtype The type of table to generate
#' @param ibase The index of the COIN to use as a base comparison
#'
#' @return Table
#'
#' @export
#'
compTableMulti <- function(COINs, tabtype = "Ranks", ibase = 1){

  stop("This function is not working at the moment - will fix soon.")
  # NOTE the issue is that ranks are calculated by calling getIn, which doesn't calculate ranks.
  # Need to calculate ranks inside this function. Also, need to perform a join on the tables so that
  # units are correctly matched between different versions.

  # change order of list: put ibase first
  COINs <- COINs[c(ibase, setdiff(1:length(COINs), ibase))]

  # get base ranks
  out_base <- getIn(COINs[[1]], dset = "Aggregated", icodes = "Index")
  ranks_base <- out_base$ind_data_only
  UnitNames <- out_base$UnitNames

  # prep a matrix
  rankmatrix <- matrix(data = NA, nrow = length(ranks_base), ncol = length(COINs))

  # populate matrix
  if (tabtype == "Ranks"){
    rankmatrix[,1] <- ranks_base
    for (ii in 2: length(COINs)){
      rankmatrix[,ii] <- getIn(COINs[[ii]], dset = "Aggregated",
                                    icodes = "Index")$ind_ranks[[1]]
    }
  } else if (tabtype == "Diffs"){
    rankmatrix[,1] <- 0
    for (ii in 2: length(COINs)){
      rankmatrix[,ii] <- ranks_base - getIn(COINs[[ii]], dset = "Aggregated",
                                                 icodes = "Index")$ind_ranks[[1]] %>% abs()
    }
  } else if (tabtype == "AbsDiffs"){
    rankmatrix[,1] <- 0
    for (ii in 2: length(COINs)){
      rankmatrix[,ii] <- abs(ranks_base - getIn(COINs[[ii]], dset = "Aggregated",
                                                     icodes = "Index")$ind_ranks[[1]])
    }
  }

  # change to df and rename cols
  df <- cbind(UnitNames, as.data.frame(rankmatrix))
  colnames(df)[2] <- names(COINs)[1]
  colnames(df)[3:ncol(df)] <- names(COINs)[-1]

  return(df)
}
