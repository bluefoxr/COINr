test_that("norm_num", {

  # numeric vector
  x <- 1:10

  # normalise with minmax
  xn <- Normalise(x, f_n = "n_minmax", f_n_para = list(l_u = c(2, 7)))
  expect_equal(min(xn), 2)
  expect_equal(max(xn), 7)

  # normalise with zscore
  xn <- Normalise(x, f_n = "n_zscore", f_n_para = list(m_sd = c(5, 2)))
  expect_equal(mean(xn), 5)
  expect_equal(sd(xn), 2)

  # normalise with dist to target
  xn <- Normalise(x, f_n = "n_dist2targ", f_n_para = list(targ = 8, cap_max = TRUE))
  expect_equal(min(xn), 0)
  expect_equal(max(xn), 1)
  expect_equal(xn[x >= 8], rep(1, sum(x >= 8)))

  # if these pass then I think the concept of the function is OK, don't need to test every normalisation func

})

test_that("norm_df", {

  # some test data
  X <- data.frame(ID = LETTERS[1:10], v1 = runif(10), v2 = runif(10))

  # test default normalisation
  Xn <- Normalise(X)

  expect_equal(min(Xn$v1), 0)
  expect_equal(min(Xn$v2), 0)
  expect_equal(max(Xn$v1), 100)
  expect_equal(max(Xn$v2), 100)
  expect_equal(Xn$ID, X$ID)

  # test individual normalisation
  Xn <- Normalise(X,
                  indiv_specs = list(
                    v1 = list(f_n = "n_minmax", f_n_para = list(l_u = c(1, 10))),
                    v2 = list(f_n = "n_borda")
                  ))

  expect_equal(min(Xn$v1), 1)
  expect_equal(max(Xn$v1), 10)
  expect_setequal(Xn$v2, 0:9)

  # test directions
  Xn <- Normalise(X, directions = data.frame(iCode = c("v1", "v2"),
                                             Direction = c(-1, 1)))

  # expect max/min to be reversed for v1 which is backwards
  expect_equal(Xn$v1[X$v1 == min(X$v1)], 100)
  expect_equal(Xn$v1[X$v1 == max(X$v1)], 0)
  # expect opposite for v2
  expect_equal(Xn$v2[X$v2 == min(X$v2)], 0)
  expect_equal(Xn$v2[X$v2 == max(X$v2)], 100)

})

test_that("norm_coin", {

  # given the other tests, we just need to check that the normalised dset agrees with df method
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # normalise using percentile ranks
  coin <- Normalise(coin, dset = "Raw",
                    global_specs = list(f_n = "n_rank"))
  dsetn <- get_dset(coin, dset = "Normalised")

  # now repeat, but via df
  dsetr <- get_dset(coin, dset = "Raw")
  # get also iMeta
  iMeta <- coin$Meta$Ind[coin$Meta$Ind$Type == "Indicator", ]

  # normalise
  dsetn2 <- Normalise(dsetr, global_specs = list(f_n = "n_rank"),
                      directions = data.frame(iCode = iMeta$iCode,
                                              Direction = iMeta$Direction))
  expect_equal(dsetn, dsetn2)

})

test_that("n_funcs", {

  # test data
  x <- runif(10)

  xn <- n_scaled(x, npara = c(1, 10))
  expect_length(xn, length(x))
  expect_equal(xn, (x-1)/(10-1)*100)  # (x-l)/(u-l) * 100

  xn <- n_dist2max(x)
  expect_equal(xn, 1 - (max(x) - x)/(max(x)-min(x))) # 1 - (x_{max} - x)/(x_{max} - x_{min})

  xn <- n_dist2ref(x, iref = 1)
  xn2 <- 1- (x[1] - x)/(x[1] - min(x)) # 1 - (x_{ref} - x)/(x_{ref} - x_{min})
  expect_equal(xn, xn2)
  xn <- n_dist2ref(x, iref = 1, cap_max = TRUE)
  xn2[xn2 > 1] <- 1
  expect_equal(xn, xn2)

  xn <- n_fracmax(x)
  expect_equal(xn, x/max(x))

  # goalposts
  xn <- n_goalposts(x, gposts = c(0.2, 0.8, 10))
  xn2 <- (x - 0.2)/0.6
  xn2[xn2 < 0] <- 0
  xn2[xn2 > 1] <- 1
  xn2 <- xn2 * 10
  expect_equal(xn, xn2)

  # zoom in on some goalpost cases
  expect_equal(n_goalposts(1, c(0, 10, 1)), 0.1) # 1 in range 0-10
  expect_equal(n_goalposts(11, c(0, 10, 1)), 1) # above max
  expect_equal(n_goalposts(-1, c(0, 10, 1)), 0) # below min
  expect_equal(n_goalposts(1, c(0, 10, 1), direction = -1), 0.9) # same but now direction reversed
  expect_equal(n_goalposts(-1, c(0, 10, 1), direction = -1), 1)
  expect_equal(n_goalposts(11, c(0, 10, 1), direction = -1), 0)

})

test_that("dist2targ", {

  x <- c(0, 5, 11)
  y <- n_dist2targ(x, targ = 10, direction = 1, cap_max = FALSE)
  expect_equal(y, c(0, 0.5, 1.1))
  # with cap
  y <- n_dist2targ(x, targ = 10, direction = 1, cap_max = TRUE)
  expect_equal(y, c(0, 0.5, 1))
  # reverse direction
  x <- c(-1, 1, 10)
  y <- n_dist2targ(x, targ = 0, direction = -1, cap_max = FALSE)
  expect_equal(y, c(1.1, 0.9, 0))
  # with cap
  y <- n_dist2targ(x, targ = 0, direction = -1, cap_max = TRUE)
  expect_equal(y, c(1, 0.9, 0))

})

test_that("dist2targ_coin", {

  # test for normalising a coin with dist2targ
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # normalise using dist2targ
  coin <- Normalise(coin, dset = "Raw", global_specs = list(f_n = "n_dist2targ", f_n_para = "use_iMeta"))

  Xr <- get_dset(coin, dset = "Raw")
  Xn <- get_dset(coin, dset = "Normalised")

  # cross-check a couple of indicators

  # LPI (direction = 1)
  targ <- coin$Meta$Ind$Target[coin$Meta$Ind$iCode == "LPI"]
  # manual normalisation
  xn <- (Xr$LPI - min(Xr$LPI, na.rm = TRUE))/(targ - min(Xr$LPI, na.rm = TRUE))
  expect_equal(Xn$LPI, xn)

  # CO2 (direction = -1)
  targ <- coin$Meta$Ind$Target[coin$Meta$Ind$iCode == "CO2"]
  # manual normalisation
  xn <- (max(Xr$CO2, na.rm = TRUE) - Xr$CO2)/(max(Xr$CO2, na.rm = TRUE) - targ)
  expect_equal(Xn$CO2, xn)
})

# test the new functionality of the normalisation parameters
test_that("iMeta_norm_paras", {

  iData <- COINr::ASEM_iData
  iMeta <- COINr::ASEM_iMeta

  # MINMAX #####

  # set two different minmax groups
  iMeta$minmax_lower <- 1
  iMeta$minmax_lower[iMeta$iCode %in% c("LPI", "CO2")] <- 5
  iMeta$minmax_upper <- 100
  iMeta$minmax_upper[iMeta$iCode %in% c("LPI", "CO2")] <- 50

  # build coin
  coin <- new_coin(iData, iMeta, quietly = TRUE)
  # normalise using minmax
  coin <- Normalise(coin, dset = "Raw", global_specs = list(f_n = "n_minmax", f_n_para = "use_iMeta"))

  # check
  expect_equal(min(coin$Data$Normalised$LPI, na.rm = TRUE), 5)
  expect_equal(min(coin$Data$Normalised$CO2, na.rm = TRUE), 5)
  expect_equal(max(coin$Data$Normalised$LPI, na.rm = TRUE), 50)
  expect_equal(max(coin$Data$Normalised$CO2, na.rm = TRUE), 50)
  expect_equal(min(coin$Data$Normalised$Flights, na.rm = TRUE), 1)
  expect_equal(max(coin$Data$Normalised$Flights, na.rm = TRUE), 100)

  # Z-SCORE #####

  # set two different mean/sd groups
  iMeta$zscore_mean <- 10
  iMeta$zscore_mean[iMeta$iCode %in% c("LPI", "CO2")] <- 100
  iMeta$zscore_sd <- 1
  iMeta$zscore_sd[iMeta$iCode %in% c("LPI", "CO2")] <- 10

  # build coin
  coin <- new_coin(iData, iMeta, quietly = TRUE)
  # normalise using minmax
  coin <- Normalise(coin, dset = "Raw", global_specs = list(f_n = "n_zscore", f_n_para = "use_iMeta"))

  # check
  expect_equal(mean(coin$Data$Normalised$LPI, na.rm = TRUE), 100)
  expect_equal(mean(coin$Data$Normalised$CO2, na.rm = TRUE), 100)
  expect_equal(sd(coin$Data$Normalised$LPI, na.rm = TRUE), 10)
  expect_equal(sd(coin$Data$Normalised$CO2, na.rm = TRUE), 10)
  expect_equal(mean(coin$Data$Normalised$Flights, na.rm = TRUE), 10)
  expect_equal(sd(coin$Data$Normalised$Flights, na.rm = TRUE), 1)

  # GOALPOSTS #####

  iData_r <- coin$Data$Raw
  iCodes <- iMeta$iCode[iMeta$Type == "Indicator"]

  iMeta$goalpost_scale <- 1
  iMeta$goalpost_trunc2posts <- TRUE

  for(iCode in iCodes){
    maxx <- max(iData_r[[iCode]], na.rm = TRUE)
    minx <- min(iData_r[[iCode]], na.rm = TRUE)
    rx <- maxx - minx
    # fake goalposts in 5% of range
    iMeta$goalpost_lower[iMeta$iCode == iCode] <- minx + 0.05*rx
    iMeta$goalpost_upper[iMeta$iCode == iCode] <- maxx - 0.05*rx
  }

  # set some exceptions
  iMeta$goalpost_scale[iMeta$iCode == "Flights"] <- 10
  iMeta$goalpost_trunc2posts[iMeta$iCode == "Goods"] <- FALSE

  # build coin
  coin <- new_coin(iData, iMeta, quietly = TRUE)
  # normalise using minmax
  coin <- Normalise(coin, dset = "Raw", global_specs = list(f_n = "n_goalposts", f_n_para = "use_iMeta"))

  # check
  iData_n <- coin$Data$Normalised

  # take LPI
  expect_equal(
    n_goalposts(iData_r$LPI, c(iMeta$goalpost_lower[iMeta$iCode == "LPI"], iMeta$goalpost_upper[iMeta$iCode == "LPI"], 1), direction = 1, trunc2posts = TRUE),
    iData_n$LPI
  )

  # take negative direction indicator
  expect_equal(
    n_goalposts(iData_r$Tariff, c(iMeta$goalpost_lower[iMeta$iCode == "Tariff"], iMeta$goalpost_upper[iMeta$iCode == "Tariff"], 1), direction = -1, trunc2posts = TRUE),
    iData_n$Tariff
  )

})
