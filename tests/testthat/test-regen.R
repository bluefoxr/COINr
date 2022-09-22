test_that("regen_coin", {

  # regen coin
  coin1 <- build_example_coin(quietly = TRUE)

  # regen
  coin2 <- Regen(coin1, quietly = FALSE)

  expect_s3_class(coin2, "coin")
  expect_equal(coin2, coin1)

  # check if can't regen
  coin2$Log$can_regen <- FALSE
  expect_error(Regen(coin2))

})

test_that("regen_purse", {

  purse <- build_example_purse(up_to = "Treat", quietly = TRUE)

  purse2 <- Regen(purse, quietly = TRUE)

  expect_s3_class(purse2, "purse")
  expect_equal(purse2, purse)

})

test_that("change_ind", {

  # example
  coin <- build_example_coin(quietly = TRUE)
  icodes <- na.omit(coin$Meta$Ind$iCode[coin$Meta$Ind$Level == 1])

  # exclude two indicators and regenerate
  coin2 <- change_ind(coin, drop = c("LPI", "Forest"), regen = TRUE)

  expect_s3_class(coin2, "coin")

  #check imeta
  icodes_ <- na.omit(coin2$Meta$Ind$iCode[coin2$Meta$Ind$Level == 1])
  expect_setequal(icodes_, setdiff(icodes, c("LPI", "Forest")))

  #check idata
  idata <- get_dset(coin2, "Raw", also_get = "none")
  expect_setequal(names(idata), setdiff(icodes, c("LPI", "Forest")))

  # replace indicator
  coin3 <- change_ind(coin2, add = "LPI", regen = TRUE)

  icodes_ <- na.omit(coin3$Meta$Ind$iCode[coin3$Meta$Ind$Level == 1])
  expect_setequal(icodes_, setdiff(icodes, c("Forest")))

  #check idata
  idata <- get_dset(coin3, "Raw", also_get = "none")
  expect_setequal(names(idata), setdiff(icodes, c("Forest")))

})
