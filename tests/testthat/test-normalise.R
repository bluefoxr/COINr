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

  xn <- n_goalposts(x, gposts = c(0.2, 0.8, 10))
  xn2 <- (x - 0.2)/0.6
  xn2[xn2 < 0] <- 0
  xn2[xn2 > 1] <- 1
  xn2 <- xn2 * 10
  expect_equal(xn, xn2)

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
  coin <- Normalise(coin, dset = "Raw", global_specs = list(f_n = "n_dist2targ"))

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
