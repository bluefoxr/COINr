test_that("denom_corr", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # get correlations >0.7 of any indicator with denominators
  cr1 <- get_denom_corr(coin, dset = "Raw", cor_thresh = 0.7)

  expect_true(all(abs(cr1$Corr) > 0.7))

  # get correlations
  # all data
  iData <- get_dset(coin, dset = "Raw", also_get = "all")
  # only the indicator data
  iData_ <- iData[coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Indicator"]]
  # only the denoms
  denoms <- iData[coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Denominator"]]

  # correlation matrix
  crmat <- cor(iData_, denoms, use = "pairwise.complete.obs")

  # i just count the number above the threshold
  expect_equal(nrow(cr1),
               sum(abs(crmat) > 0.7, na.rm = TRUE))

})

test_that("corr_flags", {

  # build example coin
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # get correlations between indicator over 0.75 within level 2 groups
  cr_flg <- get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75,
                  thresh_type = "high", grouplev = 2, roundto = NULL)

  # get data
  iData <- get_dset(coin, dset = "Normalised", also_get = "none")
  crmat <- cor(iData, use = "pairwise.complete.obs")

  # check corr vals correct
  for(ii in 1:nrow(cr_flg)){
    expect_equal(cr_flg$Corr[ii], crmat[cr_flg$Ind1[ii], cr_flg$Ind2[ii]])
  }
  # check above thresh
  expect_true(all(cr_flg$Corr > 0.75))

})

test_that("get_corr", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # CASE 1 correlate group against self
  cmat <- get_corr(coin, dset = "Raw", iCodes = list("Environ"),
                    Levels = 1, make_long = FALSE, pval = 0)
  # get data
  iData_ <- get_data(coin, dset = "Raw", iCodes = "Environ", Level = 1, also_get = "none")

  # expect same number of rows/cols
  expect_equal(ncol(iData_), nrow(cmat))
  # expect same vals
  cmat2 <- cor(iData_, use = "pairwise.complete.obs")
  expect_equal(cmat, as.data.frame(cmat2))

  # CASE 2 correlations within groups
  cmat <- get_corr(coin, dset = "Raw", iCodes = list("Sust"),
                   Levels = 1, pval = 0, grouplev = 2)
  # check whether indicator pairs are indeed within same groups at level 2
  cmat$grp1 <- coin$Meta$Lineage[[2]][match(cmat$Var1, coin$Meta$Lineage[[1]])]
  cmat$grp2 <- coin$Meta$Lineage[[2]][match(cmat$Var2, coin$Meta$Lineage[[1]])]
  expect_true(all(cmat$grp1 == cmat$grp2))

  # CASE 3 with parents
  cmat <- get_corr(coin, dset = "Aggregated", iCodes = list("Sust"),
                   Levels = c(1,2), pval = 0, withparent = TRUE)
  # check that indicators are matched with parents
  cmat$grp2 <- coin$Meta$Lineage[[2]][match(cmat$Var2, coin$Meta$Lineage[[1]])]
  expect_true(all(cmat$grp1 == cmat$Var1))

  # CASE 4 with family
  cmat <- get_corr(coin, dset = "Aggregated", iCodes = list("Sust"),
                   Levels = 1, pval = 0, withparent = "family")

  # have to test each level separately - check correlations are correct
  # LEVEL 2
  cmat_pill <- get_corr(coin, dset = "Aggregated", iCodes = list("Sust"),
                        Levels = c(1,2), pval = 0, withparent = TRUE)
  cmat2 <- merge(cmat[cmat$Var1 == "Pillar",],
                cmat_pill[c("Var2", "Correlation")], by = "Var2")
  expect_true(all(cmat2$Correlation.x == cmat2$Correlation.y))

  # LEVEL 3
  cmat_sub <- get_corr(coin, dset = "Aggregated", iCodes = list("Sust"),
                        Levels = c(1,3), pval = 0, withparent = TRUE)
  cmat2 <- merge(cmat[cmat$Var1 == "Sub-index",],
                 cmat_sub[c("Var2", "Correlation")], by = "Var2")
  expect_true(all(cmat2$Correlation.x == cmat2$Correlation.y))

  # LEVEL 4
  cmat_ind <- get_corr(coin, dset = "Aggregated", iCodes = list("Sust"),
                        Levels = c(1,4), pval = 0, withparent = TRUE)
  cmat2 <- merge(cmat[cmat$Var1 == "Index",],
                 cmat_ind[c("Var2", "Correlation")], by = "Var2")
  expect_true(all(cmat2$Correlation.x == cmat2$Correlation.y))

})

test_that("pvals", {

  # a matrix of random numbers, 3 cols
  x <- matrix(runif(30), 10, 3)
  # get correlations between cols
  cor(x)
  # get p values of correlations between cols
  pvals <- get_pvals(x)

  # test a couple of entries
  expect_equal(pvals[1,2], cor.test(x[,1], x[,2])$p.value)
  expect_equal(pvals[1,3], cor.test(x[,1], x[,3])$p.value)
  expect_equal(pvals[2,3], cor.test(x[,2], x[,3])$p.value)

})

test_that("cronbach", {

  # coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # cronbach's for physical group
  cr1 <- get_cronbach(coin, dset = "Raw", iCodes = "Physical", Level = 1)

  # verify
  # get data
  iData_ <- get_data(coin, dset = "Raw", iCodes = "Physical", Level = 1, also_get = "none")
  k <- ncol(iData_)
  # get cov
  cov1 <- cov(iData_, use = "pairwise.complete.obs")
  # sig_x
  sig_x <- sum(cov1)
  # mean inter-item cov
  sig_ij <- (sig_x - sum(diag(cov1)))/(k^2-k)
  # cronbachs
  cr2 <- (k^2 * sig_ij)/sig_x

  expect_equal(cr1, cr2)
})
