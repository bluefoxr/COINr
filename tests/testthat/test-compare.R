test_that("compare_coins", {

  # build full example coin
  coin <- build_example_coin(quietly = TRUE)

  # copy coin
  coin2 <- coin

  # change to prank function (percentile ranks)
  # we don't need to specify any additional parameters (f_n_para) here
  coin2$Log$Normalise$global_specs <- list(f_n = "n_prank")

  # regenerate
  coin2 <- Regen(coin2)

  # compare index, sort by absolute rank difference
  df_comp <- compare_coins(coin, coin2, dset = "Aggregated", iCode = "Index",
                sort_by = "Abs.diff", decreasing = TRUE)

  iData1 <- get_dset(coin, dset = "Aggregated")
  iData2 <- get_dset(coin2, dset = "Aggregated")
  # check cols
  expect_setequal(df_comp$uCode,
                  iData1$uCode)
  # merge rank cols manually
  df_comp$coin.1.check <- rank(-1*iData1$Index[match(df_comp$uCode, iData1$uCode)], ties.method = "min")
  df_comp$coin.2.check <- rank(-1*iData2$Index[match(df_comp$uCode, iData2$uCode)], ties.method = "min")

  # check rank cols
  expect_equal(df_comp$coin.1, df_comp$coin.1.check)
  expect_equal(df_comp$coin.2, df_comp$coin.2.check)

  # another example with other options
  df_comp <- compare_coins(coin, coin2, dset = "Aggregated", iCode = "Index", also_get = "uName",
                           sort_by = "uName", decreasing = FALSE, compare_by = "scores")

  expect_true("uName" %in% names(df_comp))
  # merge score cols manually
  df_comp$coin.1.check <- iData1$Index[match(df_comp$uCode, iData1$uCode)]
  df_comp$coin.2.check <- iData2$Index[match(df_comp$uCode, iData2$uCode)]
  # check
  expect_equal(df_comp$coin.1, df_comp$coin.1.check)
  expect_equal(df_comp$coin.2, df_comp$coin.2.check)

})

test_that("compare_multi", {

  # build full example coin
  coin <- build_example_coin(quietly = TRUE)

  # copy coin
  coin2 <- coin
  # change to prank function (percentile ranks)
  # we don't need to specify any additional parameters (f_n_para) here
  coin2$Log$Normalise$global_specs <- list(f_n = "n_prank")
  # regenerate
  coin2 <- Regen(coin2)

  # copy coin
  coin3 <- coin
  # we do a case where the coin is the same, to check for zero diff.
  df_comp <- compare_coins_multi(list(c1 = coin, c2 = coin2, c3 = coin3), dset = "Aggregated",
                                 iCode = "Conn", also_get = "GDP_group")
  expect_s3_class(df_comp, "data.frame")
  expect_equal(df_comp$c1, df_comp$c3)

  iData1 <- get_dset(coin, dset = "Aggregated")
  iData2 <- get_dset(coin2, dset = "Aggregated")
  iData3 <- get_dset(coin3, dset = "Aggregated")

  # merge rank cols manually
  df_comp$c1.check <- rank(-1*iData1$Conn[match(df_comp$uCode, iData1$uCode)], ties.method = "min")
  df_comp$c2.check <- rank(-1*iData2$Conn[match(df_comp$uCode, iData2$uCode)], ties.method = "min")
  df_comp$c3.check <- rank(-1*iData3$Conn[match(df_comp$uCode, iData3$uCode)], ties.method = "min")

  # check rank cols
  expect_equal(df_comp$coin.1, df_comp$coin.1.check)
  expect_equal(df_comp$coin.2, df_comp$coin.2.check)
  expect_equal(df_comp$c1, df_comp$c1.check)
  expect_equal(df_comp$c2, df_comp$c2.check)
  expect_equal(df_comp$c3, df_comp$c3.check)

  # test for all types
  l_comp <- compare_coins_multi(list(c1 = coin, c2 = coin2, c3 = coin3), dset = "Aggregated",
                                 iCode = "Conn", also_get = "GDP_group", tabtype = "All")

  expect_setequal(names(l_comp), c("Values", "Diffs", "AbsDiffs"))
  expect_true(all(sapply(l_comp, is.data.frame)))

})
