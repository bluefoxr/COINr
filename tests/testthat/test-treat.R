test_that("treat_num", {

  # make a test vector with one outlier
  x <- c(1:10, 100)

  # this has skew and kurt outside limits. Should not pass
  expect_false(check_SkewKurt(x)$Pass)

  # ok now let's winsorise
  lt <- Treat(x, f1 = "winsorise", f1_para = list(winmax = 5), f_pass = "check_SkewKurt")

  # see if point has been correctly winsorised
  expect_equal(lt$x[x == 100], 10)
  expect_equal(x[1:10], lt$x[1:10])

  # should now pass
  expect_true(check_SkewKurt(lt$x)$Pass)

  # test now a two-step treatment - add another outlier
  x <- c(1:20, 100, 200)

  # ok now let's winsorise
  lt <- Treat(x, f1 = "winsorise", f1_para = list(winmax = 1), f_pass = "check_SkewKurt")

  # we expect that since f2 is not defined, and winmax is 1, this should not pass
  expect_false(check_SkewKurt(lt$x)$Pass)

  # now allow second function
  lt <- Treat(x, f1 = "winsorise", f1_para = list(winmax = 1), f_pass = "check_SkewKurt", f2 = "log")

  # this should now be logged
  expect_equal(log(x), lt$x)

  # want to also test forced winsorisation
  lt <- Treat(x, f1 = "winsorise", f1_para = list(winmax = 3, force_win = TRUE),
              f_pass = "check_SkewKurt")

  expect_equal(sum(lt$Treated_Points$winsorise != ""), 3)
})

test_that("treat_df", {

  # we just need to check that the df method matches the num method
  X <- data.frame(runif(11),
                  c(1:10, 100),
                  c(1:10, -100))

  Xt <- Treat(X, global_specs = list(f1 = "winsorise",
                                     f1_para = list(winmax = 2),
                                     f2 = "log_CT",
                                     f2_para = list(na.rm = TRUE),
                                     f_pass = "check_SkewKurt"))

  l_t <- lapply(X, Treat, f1 = "winsorise", f1_para = list(winmax = 2), f2 = "log_CT", f2_para = list(na.rm = TRUE),
                f_pass = "check_SkewKurt")

  # compare cols of Xt (df method) with l_t (numeric method)
  for (ii in 1:3){
    expect_equal(Xt$x_treat[[ii]], l_t[[ii]][["x"]])
  }

})

test_that("treat_coin", {

  # now we just check coin method matches df method

  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
  coin <- Treat(coin, dset = "Raw")
  # get treated dset
  dset_t <- get_dset(coin, "Treated")

  # now do via df method
  dset_r <- get_dset(coin, "Raw")
  dset_t2 <- Treat(dset_r)

  # compare
  expect_identical(dset_t, dset_t2$x_treat)

})

test_that("disable_treat", {

  purse <- build_example_purse(up_to = "Screen", quietly = T)

  purse <- Treat(purse, dset = "Screened", disable = T)

  d1 <- get_dset(purse, dset = "Treated")
  d2 <- get_dset(purse, dset = "Screened")

  expect_equal(d1, d2)

})
