test_that("eff_wts", {

  # make coin
  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # get effective weights as data frame
  w_eff <- get_eff_weights(coin, out2 = "df")
  expect_s3_class(w_eff, "data.frame")

  # expect that all sum to 1 at each level
  wsums <- tapply(w_eff$EffWeight, w_eff$Level, sum)
  expect_equal(as.numeric(wsums), rep(1, 4))

  # append to coin
  coin <- get_eff_weights(coin, out2 = "coin")
  expect_s3_class(coin, "coin")

})

test_that("opt_weights", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # optimise weights at level 3
  l_opt <- get_opt_weights(coin, itarg = "equal", dset = "Aggregated",
                          Level = 3, out2 = "list")
  # expect normalised results to be equal
  expect_equal(round(l_opt$CorrResultsNorm$Obtained, 2),
               c(0.5, 0.5))

  coin <- get_opt_weights(coin, itarg = "equal", dset = "Aggregated",
                           Level = 3, weights_to = "OptLev3", out2 = "coin")
  expect_s3_class(coin, "coin")

  # new situation with unequal influence (one twice as much as the other)
  l_opt <- get_opt_weights(coin, itarg = c(1,2), dset = "Aggregated",
                           Level = 3, weights_to = "OptLev3", out2 = "list", toler = 0.01)
  expect_equal(round(l_opt$CorrResultsNorm$Obtained, 2),
               c(0.33, 0.67))

})
