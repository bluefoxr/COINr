test_that("get_data", {

  coin <- build_example_coin(up_to = "new_coin", quietly = T)

  # correct col selection
  X <- get_data(coin, dset = "Raw", iCodes = "LPI")
  expect_setequal(names(X), c("uCode", "LPI"))
  # no meta
  X <- get_data(coin, dset = "Raw", iCodes = "LPI", also_get = "none")
  expect_setequal(names(X), c("LPI"))
  # named meta
  X <- get_data(coin, dset = "Raw", iCodes = "LPI", also_get = c("uName", "Pop_group"))
  expect_setequal(names(X), c("uCode", "uName", "Pop_group", "LPI"))

  # row selection
  X <- get_data(coin, dset = "Raw", uCodes = c("AUT", "AUS"))
  expect_setequal(X$uCode, c("AUT", "AUS"))
  # group targeted by unit
  X <- get_data(coin, dset = "Raw", uCodes = "AUT", use_group = "GDP_group")
  expect_equal(unique(X$GDP_group), "L")
  expect_equal(nrow(X), sum(ASEM_iData$GDP_group == "L"))

})
