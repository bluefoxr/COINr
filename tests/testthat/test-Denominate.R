test_that("Denominate.df", {

  # Get a sample of indicator data (note must be indicators plus a "UnitCode" column)
  iData <- ASEM_iData[c("uCode", "Goods", "Flights", "LPI")]

  # Also get some denominator data
  denoms <- ASEM_iData[c("uCode", "GDP", "Population")]

  # specify how to denominate
  denomby <- data.frame(iCode = c("Goods", "Flights"),
                        Denominator = c("GDP", "Population"),
                        ScaleFactor = c(1, 1000))

  # Denominate one by the other
  xd <- Denominate(iData, denoms, denomby)

  # Now repeat manually...
  x_m <- merge(iData, denoms, by = "uCode", sort = FALSE)

  # check denom
  expect_equal(xd$Goods, x_m$Goods/x_m$GDP)
  # check scale fac
  expect_equal(xd$Flights, x_m$Flights/x_m$Population*1000)
  # check no denom
  expect_equal(xd$LPI, x_m$LPI)

})

test_that("Denom.coin", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # denominate (here, we only need to say which dset to use, takes
  # specs and denominators from within the coin)
  coin <- Denominate(coin, dset = "Raw")
  xd <- get_dset(coin, "Denominated")

  # Now test...

  # get raw dset
  xraw <- get_dset(coin, "Raw")

  # get specs
  iMeta <- coin$Meta$Ind
  iMeta <- iMeta[!is.na(iMeta$Denominator), ]

  # get denoms
  denoms <- coin$Meta$Unit

  # since the previous test demonstrates that df method works, we can use that
  xd2 <- Denominate(xraw, denoms = denoms, denomby = iMeta)

  expect_equal(xd, xd2)

})
