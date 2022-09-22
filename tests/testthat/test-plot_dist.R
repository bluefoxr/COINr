test_that("plot_dist", {

  # coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # get codes for P2P group
  pcodes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Parent == "P2P"]
  pcodes <- pcodes[!is.na(pcodes)]

  # test all plot types
  for(ptype in c("Box", "Dot", "Violin", "Violindot", "Histogram")){

    # make plot
    plt <- plot_dist(coin, dset = "Raw", iCodes = "P2P", Level = 1, type = ptype)
    # check class
    expect_s3_class(plt, "ggplot")
    # check correct indicators
    expect_setequal(pcodes, unique(plt$data$ind))
    # normalised
    plt <- plot_dist(coin, dset = "Raw", iCodes = "P2P", Level = 1, type = ptype, normalise = TRUE)
    expect_true(all(plt$data$values >= 0 & plt$data$values <= 100))

  }


})

test_that("plot_dist", {

  # # build example coin
  coin <- build_example_coin(up_to = "new_coin")

  # dot plot of LPI
  plt <- plot_dot(coin, dset = "Raw", iCode = "LPI")
  # check class
  expect_s3_class(plt, "ggplot")

  # check data
  iData <- get_dset(coin, dset = "Raw")
  # get LPI data
  x <- iData$LPI
  # check plot data equal to expected
  expect_setequal(plt$data$x, x)

  # unit labels
  plt <- plot_dot(coin, dset = "Raw", iCode = "LPI", usel = c("IND", "JPN"))
  # test lables and point positions
  expect_setequal(plt$layers[[3]]$aes_params$label, c("IND", "JPN"))
  expect_setequal(plt$layers[[2]]$data$x, iData$LPI[iData$uCode %in% c("IND", "JPN")])

  # remaining features
  plt <- plot_dot(coin, dset = "Raw", iCode = "Flights", usel = c("GBR", "AUS"),
                  marker_type = 16, add_stat = "median", stat_label = "test1",
                  show_ticks = FALSE, plabel = "iName+unit", vert_adjust = 0.5)
  # pretty much just check obj here
  expect_s3_class(plt, "ggplot")

})
