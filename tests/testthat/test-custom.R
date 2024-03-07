test_that("coin custom op", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin")

  # create function - replaces suspected unreliable point with NA
  f_NA <- function(x){ x[3, 10] <- NA; return(x)}

  # call function from Custom()
  coin <- Custom(coin, dset = "Raw", f_cust = f_NA)

  expect_equal(coin$Data$Custom[3,10], as.numeric(NA))

})

test_that("purse custom op", {

  # build example coin
  purse <- build_example_purse(up_to = "new_coin")

  # create function - replaces suspected unreliable point with NA
  f_NA <- function(x){ x[x$uCode == "AUT", "Goods"] <- NA; return(x)}

  # call function from Custom()
  purse <- Custom(purse, dset = "Raw", f_cust = f_NA, global = FALSE)

  # check
  dat_AT <- get_data(purse, dset = "Custom", iCodes = "Goods", uCodes = "AUT")

  expect_equal(dat_AT$Goods, as.numeric(rep(NA, 5)))

})
