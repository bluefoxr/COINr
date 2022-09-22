test_that("remove_elements", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # run function removing elements in level 2
  l_res <- remove_elements(coin, Level = 2, dset = "Aggregated", iCode = "Index")
  expect_type(l_res, "list")

  # get index data
  iData <- get_data(coin, dset = "Aggregated", iCodes = "Index")
  # get names of aggs at lev 2
  agnames <- na.omit(coin$Meta$Ind$iCode[coin$Meta$Ind$Level == 2])

  # check each entry of l_res
  # scores
  expect_s3_class(l_res$Scores, "data.frame")
  expect_equal(nrow(l_res$Scores), nrow(iData))
  expect_setequal(names(l_res$Scores), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$Scores$Nominal, iData$Index)
  expect_setequal(l_res$Scores$uCode, iData$uCode)
  # ranks
  expect_s3_class(l_res$Ranks, "data.frame")
  expect_equal(nrow(l_res$Ranks), nrow(iData))
  expect_setequal(names(l_res$Ranks), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$Scores$uCode, iData$uCode)
  expect_equal(l_res$Ranks$Nominal, rank(-1*l_res$Scores$Nominal, ties.method = "min"))
  # rankdiffs
  expect_s3_class(l_res$RankDiffs, "data.frame")
  expect_equal(nrow(l_res$RankDiffs), nrow(iData))
  expect_setequal(names(l_res$RankDiffs), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$RankDiffs$uCode, iData$uCode)
  expect_equal(l_res$RankDiffs$Physical, (l_res$Ranks$Nominal - l_res$Ranks$Physical))
  # absrankdiffs
  expect_s3_class(l_res$RankAbsDiffs, "data.frame")
  expect_equal(nrow(l_res$RankAbsDiffs), nrow(iData))
  expect_setequal(names(l_res$RankAbsDiffs), c("uCode", "Nominal", agnames))
  expect_setequal(l_res$RankAbsDiffs$uCode, iData$uCode)
  expect_equal(l_res$RankAbsDiffs$Physical, abs(l_res$RankDiffs$Physical))
  # meanabsdiff
  expect_type(l_res$MeanAbsDiff, "double")
  expect_equal(l_res$MeanAbsDiff,
               sapply(l_res$RankAbsDiffs[names(l_res$RankAbsDiffs) != "uCode"], mean))

  # test with alt set of weights
  coin$Meta$Weights$test1 <- coin$Meta$Weights$Original
  coin$Log$Aggregate$w <- "test1"
  coin <- Regen(coin, from = "Normalise", quietly = TRUE)
  # run function removing elements in level 2
  l_res <- remove_elements(coin, Level = 3, dset = "Aggregated", iCode = "Index")
  expect_type(l_res, "list")


})
