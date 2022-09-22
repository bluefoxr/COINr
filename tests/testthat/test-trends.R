test_that("approx_df", {

  # a time vector
  tt <- 2011:2020

  # two random vectors with some missing values
  y1 <- runif(10)
  y2 <- runif(10)
  y1[2] <- y1[5] <- NA
  y2[3] <- y2[5] <- NA
  # make into df
  Y <- data.frame(y1, y2)

  # interpolate for time = 2012 (row 2)
  Y_int <- approx_df(Y, tt, 2012)

  # second col should be unchanged
  expect_identical(Y_int$Y$y2, Y$y2)
  # all vals from the first col should be unchanged except the interpolated one
  expect_identical(Y_int$Y$y1[-2], Y$y1[-2])
  # interpolated val should be half way between surrounding two vals
  expect_equal(Y_int$Y$y1[2], mean(c(y1[1], y1[3])))

})

test_that("CAGR", {

  # random points over 10 years
  x <- 2011:2020
  y <- runif(10)

  z <- CAGR(y, x)

  expect_type(z, "double")
  expect_equal(z, (y[10]/y[1])^(1/9) - 1)

})

test_that("prc_change", {

  # random points over 2 years
  x <- 2010:2011
  y <- runif(2)

  z <- prc_change(y, x)

  expect_type(z, "double")
  expect_equal(z, (y[2]-y[1])/y[1]*100)

})

test_that("get_trends", {

  # build example purse
  purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)

  # get trends with no interpolation or anything
  trd <- get_trends(purse, dset = "Raw")$Trends

  # we will manually extract some time series and check they are equivalent
  dat1 <- get_data(purse, dset = "Raw", iCodes = "Flights", uCodes = "AUS")
  expect_equal(trd$CAGR[trd$uCode == "AUS" & trd$iCode == "Flights"],
               CAGR(dat1$Flights, dat1$Time))
  dat1 <- get_data(purse, dset = "Raw", iCodes = "MatCon", uCodes = "LTU")
  expect_equal(trd$CAGR[trd$uCode == "LTU" & trd$iCode == "MatCon"],
               CAGR(dat1$MatCon, dat1$Time))
  dat1 <- get_data(purse, dset = "Raw", iCodes = "Renew", uCodes = "JPN")
  expect_equal(trd$CAGR[trd$uCode == "JPN" & trd$iCode == "Renew"],
               CAGR(dat1$Renew, dat1$Time))

  # similar test but with use_latest 3 time points
  trd <- get_trends(purse, dset = "Raw", use_latest = 3)$Trends

  # manual calc
  dat1 <- get_data(purse, dset = "Raw", iCodes = "LPI", uCodes = "AUT", Time = 2020:2022)
  expect_equal(trd$CAGR[trd$uCode == "AUT" & trd$iCode == "LPI"],
               CAGR(dat1$LPI, dat1$Time))
  dat1 <- get_data(purse, dset = "Raw", iCodes = "Renew", uCodes = "JPN", Time = 2020:2022)
  expect_equal(trd$CAGR[trd$uCode == "JPN" & trd$iCode == "Renew"],
               CAGR(dat1$Renew, dat1$Time))

})

