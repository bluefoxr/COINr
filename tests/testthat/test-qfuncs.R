test_that("qNormalise", {

  # normalise ASEM data
  iData <- ASEM_iData[c("uCode", na.omit(ASEM_iMeta$iCode[ASEM_iMeta$Level == 1]))]
  directions <- na.omit(ASEM_iMeta[ASEM_iMeta$Level == 1, c("iCode", "Direction")])

  # df method
  dfnorm <- qNormalise(iData[names(iData) != "uCode"], directions = directions)
  expect_equal(nrow(dfnorm), nrow(iData))
  expect_equal(ncol(dfnorm), ncol(iData) - 1)
  # expect data on [0, 100] interval
  expect_true(all(sapply(dfnorm, max, na.rm = TRUE) == 100))
  expect_true(all(sapply(dfnorm, min, na.rm = TRUE) == 0))

  # coin method
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  coin <- qNormalise(coin, dset = "Raw")
  # extract data
  dfnorm_coin <- get_data(coin, dset = "Normalised")

  # to compare we need to order rows
  dfnorm <- cbind(uCode = iData$uCode, dfnorm)
  dfnorm <- dfnorm[match(dfnorm_coin$uCode, dfnorm$uCode) ,]

  # check
  expect_equal(dfnorm_coin, dfnorm, ignore_attr = TRUE)

  # purse method
  purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)
  purse <- qNormalise(purse, dset = "Raw", global = FALSE)

  # should see same dsets
  for (ii in 1:nrow(purse)){

    # get raw
    dfraw <- get_dset(purse, dset = "Raw", Time = purse$Time[ii], also_get = "none")
    # get normalised
    dfnor <- get_dset(purse, dset = "Normalised", Time = purse$Time[ii], also_get = "none")
    # manual normalisation
    dfnor2 <- qNormalise(dfraw, directions = directions)
    # check
    expect_equal(dfnor, dfnor2, ignore_attr = TRUE)

  }
})

test_that("qTreat",{

  # DF METHOD

  # select three indicators
  df1 <- ASEM_iData[c("Flights", "Goods", "Services")]

  # treat data frame, changing winmax and skew/kurtosis limits
  l_treat <- qTreat(df1, winmax = 1, skew_thresh = 1.5, kurt_thresh = 3)

  expect_setequal(names(l_treat), c("x_treat", "Dets_Table", "Treated_Points"))
  expect_equal(nrow(l_treat$x_treat), nrow(df1))
  expect_equal(ncol(l_treat$x_treat), ncol(df1))

  # mainly just test that corresponds to main Treat() function, which is tested itself
  l_treat2 <- Treat(df1, global_specs = list(f1_para = list(winmax = 1,
                                                            skew_thresh = 1.5,
                                                            kurt_thresh = 3),
                                             f_pass_para = list(skew_thresh = 1.5,
                                                                kurt_thresh = 3)))
  expect_equal(l_treat, l_treat2)

  # COIN METHOD
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  # quick treat
  coin1 <- qTreat(coin, dset = "Raw", winmax = 3)
  # full Treat
  coin2 <- Treat(coin, dset = "Raw", global_specs = list(f1_para = list(winmax = 3)))
  expect_equal(coin1$Data, coin2$Data)

  # PURSE METHOD
  purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)
  purse1 <- qTreat(purse, dset = "Raw", winmax = 2)
  purse2 <- Treat(purse, dset = "Raw", global_specs = list(f1_para = list(winmax = 3)))
  for(ii in 1:nrow(purse)){

    expect_equal(purse1$coin[[ii]]$Data$Treated, purse1$coin[[ii]]$Data$Treated)

  }

})
