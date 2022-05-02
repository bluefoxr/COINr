# tests for new_coin()
# just class checks for now

test_that("class check",{
  expect_s3_class(new_coin(ASEM_iData, ASEM_iMeta, quietly = TRUE), "coin")
  expect_s3_class(new_coin(ASEM_iData_p, ASEM_iMeta, split_to = "all", quietly = TRUE), c("purse", "data.frame"))
})
