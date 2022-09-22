# note: avoid testing cointool input because involves messy download

test_that("names2codes", {

  # get names from example data
  iNames <- ASEM_iMeta$iName

  # convert to codes
  icodes <- names_to_codes(cvec = iNames, maxword = 2, maxlet = 4)

  expect_type(icodes, "character")
  expect_equal(length(icodes), length(iNames))
  # codes should be max 2*4 = 8 but if there are duplictes it will add "_x" where
  # x is an incremental number. Therefore max characters is 10 in this case, unless
  # there are more than 9 duplicates
  expect_true(all(sapply(icodes, nchar) <= 10))

})
