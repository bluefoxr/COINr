test_that("screendf", {

  # test df
  X <- as.data.frame(matrix(runif(100), 10, 10))

  X <- cbind(ID = letters[1:10], X)

  # introduce missing values
  # 50% avail for first unit
  X[1, 2:6] <- NA
  # 90% avail for second unit
  X[2, 2] <- NA
  # 50% zeros for third unit
  X[3, 2:6] <- 0
  # 90% non-zero for fourth
  X[4, 2] <- 0

  # screen by 75% dat avail
  l <- Screen(X, unit_screen = "byNA", dat_thresh = 0.75, id_col = "ID")

  # expect that first row only is removed
  expect_equal(l$ScreenedData, X[-1,])

  # screen by 100% dat avail
  l <- Screen(X, unit_screen = "byNA", dat_thresh = 1)

  # expect that first two rows removed
  expect_equal(l$ScreenedData, X[-c(1, 2), ])

  # screen by 75% non zero
  l <- Screen(X, unit_screen = "byzeros", nonzero_thresh = 0.75)

  # expect that third row removed
  expect_equal(l$ScreenedData, X[-c(3), ])

  # test force out a unit
  # screen by 100% dat avail and force
  l <- Screen(X, unit_screen = "byNA", dat_thresh = 1,
              Force = data.frame(uCode = "f", Include = FALSE), id_col = "ID")

  expect_equal(l$ScreenedData, X[-c(1, 2, 6), ])

})

test_that("screen_coin", {

  # if the df method works, we just check that the dset is as expected
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # screen
  coin <- Screen(coin, dset = "Raw", unit_screen = "byNAandzeros",
                 dat_thresh = 0.9, nonzero_thresh = 0.5, Force = data.frame(uCode = "GBR", Include = FALSE),
                 write_to = "test1")

  # get screened dset
  dset <- get_dset(coin, dset = "test1")

  # now do manually
  dsetr <- get_dset(coin, dset = "Raw")
  dset2 <- Screen(dsetr, id_col = "uCode", unit_screen = "byNAandzeros",
                  dat_thresh = 0.9, nonzero_thresh = 0.5, Force = data.frame(uCode = "GBR", Include = FALSE))

  expect_equal(dset, dset2$ScreenedData)

})
