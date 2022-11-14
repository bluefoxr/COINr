test_that("write to log", {

  # write to log must be called through another COINr function
  # new_coin
  coin <- new_coin(ASEM_iData, ASEM_iMeta)
  expect_s3_class(coin, "coin")
  expect_equal(names(coin$Log), c("new_coin", "can_regen"))

  # check with :: syntax
  coin <- COINr::new_coin(ASEM_iData, ASEM_iMeta)
  expect_s3_class(coin, "coin")
  expect_equal(names(coin$Log), c("new_coin", "can_regen"))

  # add another function
  coin <- qNormalise(coin, dset = "Raw")
  expect_setequal(names(coin$Log), c("new_coin", "can_regen", "qNormalise"))

})
