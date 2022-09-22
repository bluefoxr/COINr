test_that("get_pca", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # PCA on "Sust" group of indicators
  l_pca <- get_PCA(coin, dset = "Raw", iCodes = "Sust",
                  out2 = "list", nowarnings = TRUE, by_groups = TRUE)
  # orig weights
  w <- coin$Meta$Weights$Original
  # check weights
  expect_equal(nrow(l_pca$Weights), nrow(w))
  expect_equal(ncol(l_pca$Weights), ncol(w))

  # check PCA output
  expect_setequal(names(l_pca$PCAresults),
                  unique(coin$Meta$Lineage[coin$Meta$Lineage$`Sub-index` == "Sust",2]))

  expect_s3_class(l_pca$PCAresults$Environ$PCAres, "prcomp")
  expect_s3_class(l_pca$PCAresults$Social$PCAres, "prcomp")
  expect_s3_class(l_pca$PCAresults$SusEcFin$PCAres, "prcomp")

  # quick check of coin output
  coin <- get_PCA(coin, dset = "Raw", iCodes = "Sust",
                  out2 = "coin", nowarnings = TRUE, by_groups = TRUE, weights_to = "test1")
  expect_s3_class(coin, "coin")
  expect_equal(l_pca$Weights, coin$Meta$Weights$test1)

})
