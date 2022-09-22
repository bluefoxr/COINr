test_that("stats", {

  # stats of ASEM data
  # data with only indicator cols (all numeric)
  iData_ <- ASEM_iData[na.omit(ASEM_iMeta$iCode[ASEM_iMeta$Level == 1])]
  df1 <- get_stats(iData_, nsignif = 4)

  # not going to test all stats, just a selection
  expect_equal(nrow(df1), ncol(iData_))
  expect_setequal(df1$iCode, names(iData_))
  # check min (note, have to round)
  expect_equal(df1$Min[df1$iCode == "LPI"], signif(min(iData_$LPI, na.rm = TRUE), 4))
  # std dev
  expect_equal(df1$Std[df1$iCode == "LPI"], signif(sd(iData_$LPI, na.rm = TRUE), 4))
  # num avail
  expect_equal(df1$N.Avail[df1$iCode == "LPI"], signif(sum(!is.na(iData_$LPI)), 4))
  # n unique
  expect_equal(df1$N.Unique[df1$iCode == "LPI"], signif(length(unique(iData_$LPI)), 4))

  # last, do skew/kurt flags as these are important
  expect_equal(df1$Skew[df1$iCode == "Flights"], signif(skew(iData_$Flights, na.rm = TRUE), 4))
  expect_equal(df1$Kurt[df1$iCode == "Flights"], signif(kurt(iData_$Flights, na.rm = TRUE), 4))

  # flights should be OUT
  expect_true( (abs(df1$Skew[df1$iCode == "Flights"]) > 2) & (df1$Kurt[df1$iCode == "Flights"] > 3.5) )
  expect_equal(df1$Flag.SkewKurt[df1$iCode == "Flights"], "OUT")


  ## COIN METHOD ##
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  df2 <- get_stats(coin, dset = "Raw", nsignif = 4, out2 = "df")
  expect_equal(df2, df1)
  coin <- get_stats(coin, dset = "Raw", nsignif = 4, out2 = "coin")
  expect_equal(coin$Analysis$Raw$Stats, df1)
})
