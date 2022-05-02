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
