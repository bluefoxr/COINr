test_that("print.coin", {

  coin <- build_example_coin(up_to = "Treat", quietly = TRUE)

  # manual check and compare to cached version
  expect_snapshot_output(print(coin))

})

test_that("print.purse", {

  purse <- build_example_purse(up_to = "Treat", quietly = TRUE)

  # manual check and compare to cached version
  expect_snapshot_output(print(purse))

})

test_that("is_methods", {

  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  expect_true(is.coin(coin))
  expect_false(is.coin(mtcars))

  purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)
  expect_true(is.purse(purse))
  expect_false(is.purse(mtcars))

})
