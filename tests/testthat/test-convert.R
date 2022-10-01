test_that("COIN_to_coin", {

  # in order to run this test we need the COINr6 package
  if(requireNamespace("COINr6", quietly=TRUE)){

    # build COIN
    COIN <- COINr::ASEM_COIN

    # convert to coin
    coin <- COIN_to_coin(COIN, recover_dsets = TRUE)
    expect_s3_class(coin, "coin")
    expect_setequal(names(COIN$Data), names(coin$Data))

    # check raw data agrees at least
    iData1 <- as.data.frame(COIN$Data$Raw)
    iData2 <- as.data.frame(coin$Data$Raw)
    names(iData1)[names(iData1) == "UnitCode"] <- "uCode"
    iData1 <- iData1[names(iData2)]
    iData1 <- iData1[match(iData2$uCode, iData1$uCode) , ]
    row.names(iData1) <- NULL
    row.names(iData2) <- NULL

    expect_mapequal(iData1, iData2)

  }

})
