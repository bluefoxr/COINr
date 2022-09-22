test_that("plot_corr", {
  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # hard to properly test here, I'm just going to test a few different options and
  # make sure we don't get any errors

  # plot correlations between indicators in Sust group, using Normalised dset
  plt <- plot_corr(coin, dset = "Normalised", iCodes = list("Sust"),
            grouplev = 2, flagcolours = TRUE)
  expect_s3_class(plt, "ggplot")

  # test some further options
  plt <- plot_corr(coin, dset = "Aggregated",
                   iCodes = list(c("LPI", "Flights", "CO2"), c("Physical", "P2P")), Levels = c(1,2),
                   cortype = "kendall")
  expect_s3_class(plt, "ggplot")

  # yet more options
  # withparent
  plt <- plot_corr(coin, dset = "Aggregated", iCodes = list("Physical"), Levels = c(1,2), withparent = TRUE)
  expect_s3_class(plt, "ggplot")
  # boxes
  plt <- plot_corr(coin, dset = "Normalised", iCodes = list("Sust"), Levels = 1, box_level = 2)
  expect_s3_class(plt, "ggplot")
  # grouping
  plt <- plot_corr(coin, dset = "Normalised", iCodes = list("Sust"), Levels = 1, box_level = 2, grouplev = 2)
  expect_s3_class(plt, "ggplot")
  # family
  plt <- plot_corr(coin, dset = "Aggregated", iCodes = list("Conn"), Levels = 1, withparent = "family", flagcolours = TRUE)
  expect_s3_class(plt, "ggplot")
})
