# tests for new_coin()
# just class checks for now

test_that("class check",{
  expect_s3_class(new_coin(ASEM_iData, ASEM_iMeta, quietly = TRUE), "coin")
  expect_s3_class(new_coin(ASEM_iData_p, ASEM_iMeta, split_to = "all", quietly = TRUE), c("purse", "data.frame"))
})

test_that("check_iData", {

  # spaces
  iData <- ASEM_iData
  names(iData)[10] <- "spa ce"
  expect_error(check_iData(iData))

  # number start
  iData <- ASEM_iData
  names(iData)[10] <- "1number"
  expect_error(check_iData(iData))

})

test_that("check_iMeta", {

  # spaces
  iMeta <- ASEM_iMeta
  iMeta$iCode[10] <- "spa ce"
  expect_error(check_iMeta(iMeta))

  # number start
  iMeta <- ASEM_iMeta
  iMeta$iCode[10] <- "1number"
  expect_error(check_iMeta(iMeta))

  # duplicate codes
  iMeta <- ASEM_iMeta
  iMeta <- rbind(iMeta[1,], iMeta)
  expect_error(check_iMeta(iMeta))

})
