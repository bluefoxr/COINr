test_that("directionalise", {

  iData <- ASEM_iData[c("LPI", "Flights", "CO2")]
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  iData_ <- directionalise(iData, coin)

  iData2 <- iData
  iData2$CO2 <- -iData2$CO2

  expect_equal(iData_, iData2)

})
