test_that("plot_bar", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # bar plot of CO2 by GDP per capita group
  plt <- plot_bar(coin, dset = "Raw", iCode = "CO2",
           by_group = "GDPpc_group", axes_label = "iName")
  expect_s3_class(plt, "ggplot")

  iData <- get_data(coin, "Raw")
  expect_setequal(plt$data$CO2, iData$CO2)

  # stack
  plt <- plot_bar(coin, dset = "Aggregated", iCode = "Conn", stack_children = TRUE,
                  axes_label = "iName")
  expect_s3_class(plt, "ggplot")
  iData <- get_data(coin, "Aggregated")
  expect_setequal(unique(plt$data$Component),
                  na.omit(coin$Meta$Ind$iCode[coin$Meta$Ind$Parent == "Conn"]))

  # no group
  plt <- plot_bar(coin, dset = "Raw", iCode = "CO2")
  expect_s3_class(plt, "ggplot")

})
