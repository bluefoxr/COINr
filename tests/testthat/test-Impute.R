test_that("Impute.numeric", {

  # vector with missing value
  x <- 1:10
  x[3] <- NA
  # impute
  xi <- Impute(x, f_i = "i_median")

  expect_equal(xi[3], median(x, na.rm = T))
  expect_length(xi, length(x))
  expect_equal(xi[-3], x[-3])

})

# Check data frame method: imputing using mean and median, by row and col

test_that("Impute.data.frame", {

  # a df of random numbers
  X <- as.data.frame(matrix(runif(50), 10, 5))

  # introduce NAs (2 in 3 of 5 cols)
  X[sample(1:10, 2), 1] <- NA
  X[sample(1:10, 2), 3] <- NA
  X[sample(1:10, 2), 5] <- NA

  # impute X manually using col mean
  Xi1 <- X
  Xi1[is.na(Xi1[1]), 1] <- mean(Xi1[[1]], na.rm = T)
  Xi1[is.na(Xi1[3]), 3] <- mean(Xi1[[3]], na.rm = T)
  Xi1[is.na(Xi1[5]), 5] <- mean(Xi1[[5]], na.rm = T)

  # do the same in COINr
  Xic <- Impute(X, f_i = "i_mean")

  # CHECK
  expect_equal(Xi1, Xic)

  # same for rows
  Xi2 <- X

  # manual imputation of rows using median
  for(irow in 1:nrow(Xi2)){
    xrow <- as.numeric(Xi2[irow, ])
    xrow[is.na(xrow)] <- median(xrow, na.rm = T)
    Xi2[irow, ] <- xrow
  }

  # COINr
  Xic2 <- Impute(X, f_i = "i_median", impute_by = "row", normalise_first = FALSE)

  # CHECK
  expect_equal(Xi2, Xic2)

})


test_that("impute.coin", {

  # build coin
  coin <- build_example_coin(up_to = "new_coin")

  # impute raw data set using population groups
  coin <- Impute(coin, dset = "Raw", f_i = "i_mean_grp", use_group = "Pop_group", write_to = "test1")

  # get data set
  df1 <- get_dset(coin, dset = "test1", also_get = "none")

  # impute the same but semi-manually
  # first get raw data
  dfraw <- get_dset(coin, "Raw", also_get = "Pop_group")
  # only numeric cols
  dfraw_ <- dfraw[names(dfraw) %nin% c("uCode", "Pop_group")]

  # impute using same approach but we get the ingredients manually and use data.frame method
  df2 <- Impute(dfraw_, f_i = "i_mean_grp", f_i_para = list(f = dfraw$Pop_group))

  expect_equal(df1, df2)

})
