test_that("agg_df", {

  # test data frame, 5 cols 20 rows
  X <- as.data.frame(matrix(runif(100), 20, 5))

  # test default - means of rows
  Xag <- Aggregate(X)
  expect_equal(Xag, rowMeans(X))

  # try passing some weights
  Xag <- Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = 1:5))

  # repeat manually
  Xag2 <- apply(X, 1, weighted.mean, 1:5)
  expect_equal(Xag, Xag2)

  # test data threshold
  X_NAs <- X
  # set first row to have 4/5 NAs
  X_NAs[1, 1:4] <- NA
  # aggregate (same as last time)
  XagNA <- Aggregate(X_NAs, f_ag = "a_amean", f_ag_para = list(w = 1:5), dat_thresh = 0.5)
  # check first one is NA
  expect_equal(XagNA[1], as.numeric(NA))
  # check rest are the same
  expect_equal(XagNA[-1], Xag[-1])

})

test_that("agg_coin", {

  # build example coin up to normalised dset
  coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

  # the objective here will be to check that the aggregation follows the weights
  # and the structure
  # To check this properly we'll have to change the weights, since at the moment all equal
  coin$Meta$Weights$Alt <- coin$Meta$Weights$Original
  coin$Meta$Weights$Alt$Weight <- runif(nrow(coin$Meta$Weights$Alt))

  # now aggregate
  coin <- Aggregate(coin, dset = "Normalised", w = "Alt")

  # extract normalised dset
  Xnorm <- get_dset(coin, "Normalised")
  # extract aggregated dset (via coin method)
  Xagg <- get_dset(coin, "Aggregated")

  # NOW we have to repeat manually - this will be slightly fiddly
  # the starting point is the normalised data set
  Xagg2 <- Xnorm

  # get lineage
  lin <- coin$Meta$Lineage
  # get weights
  ws <- coin$Meta$Weights$Alt

  # aggregate each pillar manually
  pcodes <- unique(lin$Pillar)
  for(pcode in pcodes){
    # ind codes
    icodes <- lin$Indicator[lin$Pillar == pcode]
    # get inds
    X <- Xagg2[icodes]
    # get weights
    w <- ws$Weight[match(icodes, ws$iCode)]
    # aggregate
    Xagg2 <- cbind(Xagg2, Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = w)))
    # rename
    names(Xagg2)[ncol(Xagg2)] <- pcode
  }

  # aggregate each sub-pillar manually
  pcodes <- unique(lin$`Sub-index`)
  for(pcode in pcodes){
    # ind codes
    icodes <- unique(lin$Pillar[lin$`Sub-index` == pcode])
    # get inds
    X <- Xagg2[icodes]
    # get weights
    w <- ws$Weight[match(icodes, ws$iCode)]
    # aggregate
    Xagg2 <- cbind(Xagg2, Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = w)))
    # rename
    names(Xagg2)[ncol(Xagg2)] <- pcode
  }

  # aggregate to index
  # ind codes
  icodes <- unique(lin$`Sub-index`)
  # get inds
  X <- Xagg2[icodes]
  # get weights
  w <- ws$Weight[match(icodes, ws$iCode)]
  # aggregate
  Xagg2 <- cbind(Xagg2, Aggregate(X, f_ag = "a_amean", f_ag_para = list(w = w)))
  # rename
  names(Xagg2)[ncol(Xagg2)] <- "Index"

  # COMPARE
  # make sure col orderings are the same
  Xagg2 <- Xagg2[names(Xagg)]

  # CHECK
  expect_equal(Xagg, Xagg2)

})

test_that("agg_purse", {

  # purse
  purse <- build_example_purse(up_to = "Normalise", quietly = TRUE)

  # aggregate via coin method
  coin1 <- Aggregate(purse$coin[[1]], dset = "Normalised")

  # aggregate via purse method
  purse <- Aggregate(purse, dset = "Normalised")

  # check
  expect_equal(purse$coin[[1]]$Data$Aggregated, coin1$Data$Aggregated)
})

test_that("agg_functions", {

  # geometric
  x <- c(1, 2, 3)
  y1 <- a_gmean(x)

  expect_equal(y1, (1*2*3)^(1/3))
  expect_error(a_gmean(c(1, 2, -1)))
  expect_error(a_gmean(c(1, 2, 0)))

  y1 <- a_gmean(x, c(1,1,2))
  expect_equal(y1, (1*2*3^2)^(1/4))

  # harmonic
  x <- c(1, 2, 3)
  y1 <- a_hmean(x, c(2, 1, 1))
  expect_equal(y1, 4/(2/1 + 1/2 + 1/3))

})

test_that("outranking", {

  # a df
  X <- data.frame(
    x1 = 1:4,
    x2 = c(3:1, 5),
    x3 = c(2,3,1, 4)
  )

  orm <- outrankMatrix(X)

  expect_equal(nrow(orm$OutRankMatrix), 4)
  expect_equal(ncol(orm$OutRankMatrix), 4)

  # check some scores
  expect_equal(diag(orm$OutRankMatrix), c(0,0,0,0))
  expect_equal(orm$OutRankMatrix[2,1], 2/3)
  expect_equal(orm$OutRankMatrix[3,1], 1/3)
  expect_equal(orm$OutRankMatrix[4,1], 1)
  expect_equal(orm$nDominant, 3)
  expect_equal(orm$fracDominant, 3/6)

})

test_that("copeland", {

  # a df
  X <- data.frame(
    x1 = 1:4,
    x2 = c(3:1, 5),
    x3 = c(2,3,1, 4)
  )

  y <- a_copeland(X)

  orm <- outrankMatrix(X)$OutRankMatrix
  orm[orm > 0.5] <- 1
  orm[orm == 0.5] <- 0
  orm[orm < 0.5] <- -1
  diag(orm) <- 0

  expect_equal(y, rowSums(orm))

})
