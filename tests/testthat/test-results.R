test_that("results_tables", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # get simple results table
  df_results <- get_results(coin, dset = "Aggregated", tab_type = "Aggs")
  # expect all agg names avove level 1
  agg_codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type == "Aggregate"]
  expect_setequal(names(df_results), c("uCode", "Rank", agg_codes))

  # get full results table
  df_results <- get_results(coin, dset = "Aggregated", tab_type = "Full")
  # expect all agg names avove level 1
  agg_codes <- coin$Meta$Ind$iCode[coin$Meta$Ind$Type %in% c("Indicator", "Aggregate")]
  expect_setequal(names(df_results), c("uCode", "Rank", agg_codes))

})

test_that("unit_summary", {

  # build coin
  coin <- build_example_coin(quietly = TRUE)

  # summary for a unit (round to very high precision)
  dfsum <- get_unit_summary(coin, usel = "IND", Levels = c(4,3,2), dset = "Aggregated", nround = NULL)

  # cross check
  iData <- get_data(coin, dset = "Aggregated", uCodes = "IND")
  for(ii in 1:nrow(dfsum)){
    expect_equal(dfsum$Score[ii], as.numeric(iData[dfsum$Code[ii]]))
  }

  expect_setequal(names(dfsum), c("Code", "Name", "Score", "Rank"))

})

test_that("str_weak", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # get strengths and weaknesses for ESP
  l <- get_str_weak(coin, dset = "Raw", usel = "ESP")

  expect_setequal(names(l), c("Strengths", "Weaknesses"))

  # get data
  iData <- get_data(coin, dset = "Raw")

  # check vals
  for(ii in 1:nrow(l$Strengths)){
    expect_equal(l$Strengths$Value[ii],
                 signif(as.numeric(iData[iData$uCode == "ESP", l$Strengths$Code[ii]]), 3))
    expect_equal(l$Weaknesses$Value[ii],
                 signif(as.numeric(iData[iData$uCode == "ESP", l$Weaknesses$Code[ii]]), 3))
  }


})
