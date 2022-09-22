test_that("plot_scatter", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin")

  # scatter plot of Flights against Population
  # coloured by GDP per capita
  # log scale applied to population
  plt <- plot_scatter(coin, dsets = c("uMeta", "Raw"),
               iCodes = c("Population", "Flights"),
               by_group = "GDPpc_group", log_scale = c(TRUE, FALSE))
  # check class
  expect_s3_class(plt, "ggplot")

  # get data
  iData <- coin$Log$new_coin$iData[c("uCode", "GDPpc_group", "Population", "Flights")]
  iData <- iData[order(iData$uCode), ]

  # compare dfs
  iData_plt <- plt$data[names(iData)]
  iData_plt <- iData_plt[match(iData_plt$uCode, iData$uCode) ,]
  iData_plt <- iData_plt[order(iData_plt$uCode), ]
  row.names(iData) <- row.names(iData_plt) <- NULL
  expect_equal(iData_plt, iData)

  # try another with extra options
  plt <- plot_scatter(coin, dsets = "Raw", iCodes = c("CO2", "Poverty"),
                      log_scale = c(FALSE, FALSE), alpha = 0.5, axes_label = "iName",
                      dset_label = FALSE, point_label = "uCode", nudge_y = 2)
  expect_s3_class(plt, "ggplot")

})
