test_that("Impute.numeric", {

  # vector with missing value
  x <- 1:10
  x[3] <- NA
  # impute
  xi <- Impute(x, f_i = "i_median")

  expect_equal(xi[3], median(x, na.rm = T))
  expect_length(xi, length(x))
  expect_equal(xi[-3], x[-3])

  # check for mis-specified function (first arg is not called x)
  f_test <- function(y){y}
  expect_error(
    Impute(x, f_i = "f_test")
  )

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

  # test group and by-row imputation
  coin <- Impute(coin, dset = "Raw", impute_by = "row", f_i = "i_mean", group_level = 2)

  # this should have imputed using a grouped row median, normalising first. let's see
  xr <- get_data(coin, dset = "Raw", iCodes = "Physical", Level = 1, uCodes = "BGD")
  xn <- get_data(coin, dset = "Imputed", iCodes = "Physical", Level = 1, uCodes = "BGD")
  expect_mapequal(xn[names(xn) != "ConSpeed"], xr[names(xr) != "ConSpeed"])

  # have to normalise this df
  X <- get_data(coin, dset = "Raw", iCodes = "Physical", Level = 1)
  Xn <- lapply(X, function(x){
    if(is.numeric(x)){
      (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    } else {
      x
    }
  })
  Xn <- as.data.frame(Xn)

  # the still-normalised imputed vector
  xn_unsc <- Xn[Xn$uCode == "BGD", ]
  # est conspeed
  xref <- rowMeans(xn_unsc[-1], na.rm = TRUE)
  xref_sc <- xref*(max(X$ConSpeed, na.rm = TRUE) - min(X$ConSpeed, na.rm = TRUE)) + min(X$ConSpeed, na.rm = TRUE)

  # FINALLY the test
  expect_equal(xn$ConSpeed, as.numeric(xref_sc))

  # also test no imputation
  coin <- Impute(coin, dset = "Raw", f_i = "i_mean_grp", use_group = "Pop_group", write_to = "test1", disable = TRUE)
  expect_identical(coin$Data$Raw, coin$Data$test1)


})

test_that("impute.purse", {

  purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)

  pursei <- Impute(purse, dset = "Raw")

  # compare
  coin1 <- Impute(purse$coin[[1]], dset = "Raw")
  expect_equal(pursei$coin[[1]], coin1)

  # manually set up panel imputation NA
  purse$coin$`2022`$Data$Raw$Flights[1] <- NA

  pursei <- Impute(purse, dset = "Raw", f_i = "impute_panel")

  # we should see that the 2022 value is equal to the 2021 value now
  x21 <- pursei$coin$`2021`$Data$Imputed$Flights[1]
  x22 <- pursei$coin$`2022`$Data$Imputed$Flights[1]

  expect_equal(x22, x21)

})

test_that("group_mean", {

  # data plus groupings
  x <- runif(20)
  f <- sample(c("a","b"), 20, replace = TRUE)

  # make some missing data
  x[c(1, 5, 10)] <- NA

  # impute
  xi <- i_mean_grp(x, f)

  # check
  expect_equal(xi[1], mean(x[f==f[1]], na.rm = TRUE))
  expect_equal(xi[5], mean(x[f==f[5]], na.rm = TRUE))
  expect_equal(xi[10], mean(x[f==f[10]], na.rm = TRUE))

  # now check case with missing groups
  f[c(2,5)] <- NA
  # impute
  xi <- i_mean_grp(x, f)

  # check - I exclude the NA group rows completely
  x2 <- x[-c(2,5)]
  f2 <- f[-c(2,5)]

  # expect the first NA value to be equal to the mean of the group in the vector excluding NAs
  expect_equal(xi[1], mean(x2[f2==f2[1]], na.rm = TRUE))
  # expect second NA to be equal to NA still, because has NA group
  expect_equal(xi[5], as.numeric(NA))

})

test_that("group_median", {

  # data plus groupings
  x <- runif(20)
  f <- sample(c("a","b"), 20, replace = TRUE)

  # make some missing data
  x[c(1, 5, 10)] <- NA

  # impute
  xi <- i_median_grp(x, f)

  # check
  expect_equal(xi[1], median(x[f==f[1]], na.rm = TRUE))
  expect_equal(xi[5], median(x[f==f[5]], na.rm = TRUE))
  expect_equal(xi[10], median(x[f==f[10]], na.rm = TRUE))

  # now check case with missing groups
  f[c(2,5)] <- NA
  # impute
  xi <- i_median_grp(x, f)

  # check - I exclude the NA group rows completely
  x2 <- x[-c(2,5)]
  f2 <- f[-c(2,5)]

  # expect the first NA value to be equal to the mean of the group in the vector excluding NAs
  expect_equal(xi[1], median(x2[f2==f2[1]], na.rm = TRUE))
  # expect second NA to be equal to NA still, because has NA group
  expect_equal(xi[5], as.numeric(NA))

})

test_that("Warning when NAs found after imputation", {

  # build coin
  coin <- build_example_coin(up_to = "new_coin")

  # impute raw data set: expect warning due to NAs still present
  expect_warning(Impute(coin, dset = "Raw", f_i = "NA_imputer"))

  expect_no_warning(Impute(coin, dset = "Raw", f_i = "NA_imputer", warn_on_NAs = FALSE))

})
