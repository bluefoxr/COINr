test_that("sensitivity_works", {

  # build example coin
  coin <- build_example_coin(quietly = TRUE)

  # test case where we know one sensitivity index should be zero
  # we let winmax vary
  l_winmax <- list(Address = "$Log$Treat$global_specs$f1_para$winmax",
                   Distribution = 1:5,
                   Type = "discrete")

  # we keep normalisation method FIXED
  # we trick the function by using two lists that are the same
  norm_alts <- list(
    list(f_n = "n_minmax", f_n_para = list(c(1,100))),
    list(f_n = "n_minmax", f_n_para = list(c(1,100)))
  )

  # now put this in a list
  l_norm <- list(Address = "$Log$Normalise$global_specs",
                 Distribution = norm_alts,
                 Type = "discrete")

  # create overall specification list
  SA_specs <- list(
    Winmax = l_winmax,
    Normalisation = l_norm
  )

  # run sensitivity analysis
  # we can run at fairly low sample size because very few possibilities (five in fact)
  SA_res <- get_sensitivity(coin, SA_specs = SA_specs, N = 20, SA_type = "SA",
                            dset = "Aggregated", iCode = "Index", Nboot = 100)

  # test general format of output
  expect_setequal(names(SA_res), c("Scores", "Ranks", "RankStats", "Para", "Sensitivity", "Nominal"))
  expect_equal(ncol(SA_res$Scores), 20*4+2)

  # the Si and STi of the normalisation assumption should be zero
  expect_equal(SA_res$Sensitivity$Si[2], 0)
  expect_equal(SA_res$Sensitivity$STi[2], 0)

  p1 <- plot_sensitivity(SA_res, ptype = "bar")
  p2 <- plot_sensitivity(SA_res, ptype = "pie")
  p3 <- plot_sensitivity(SA_res, ptype = "box")
  # test plotting
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")

  p4 <- plot_uncertainty(SA_res)
  p5 <- plot_uncertainty(SA_res, plot_units = "top10")
  p6 <- plot_uncertainty(SA_res, plot_units = "bottom10")
  expect_s3_class(p4, "ggplot")
  expect_s3_class(p5, "ggplot")
  expect_s3_class(p6, "ggplot")

})

test_that("sampling_works", {

  # make a sample
  N <- 100
  d <- 3
  X <- SA_sample(N, d)

  expect_equal(nrow(X), N*(d+2))
  expect_equal(ncol(X), d)
  expect_true(all(X >= 0))
  expect_true(all(X <= 1))

  # test function (note X3 not used)
  y <- X[,1] + 10*X[,2]

  # get SA estimates
  t1 <- SA_estimate(y, N, d)

  # can't really check the individual sensitivity indices due to estimation error, but some should be zero
  expect_equal(t1$SensInd$Si[3], 0)
  expect_equal(t1$SensInd$STi[3], 0)

})

test_that("noisy_weights", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # get nominal weights
  w_nom <- coin$Meta$Weights$Original

  # build data frame specifying the levels to apply the noise at
  # here we vary at levels 2 and 3
  noise_specs = data.frame(Level = c(2,3),
                           NoiseFactor = c(0.25, 0.25))

  # get 10 replications
  noisy_wts <- get_noisy_weights(w = w_nom, noise_specs = noise_specs, Nrep = 10)

  # tests
  expect_type(noisy_wts, "list")
  expect_length(noisy_wts, 10)
  expect_true(all(sapply(noisy_wts, is.data.frame)))
  # df dims are the same
  expect_true(all(sapply(noisy_wts, function(X){
    nrow(X) == nrow(w_nom)
  })))
  expect_true(all(sapply(noisy_wts, function(X){
    ncol(X) == ncol(w_nom)
  })))
  # rows with unperturbed weights are the same
  expect_true(all(sapply(noisy_wts, function(X){
    all(X$Weight[X$Level == 1] == w_nom$Weight[w_nom$Level == 1])
  })))
  # names the same
  expect_true(all(sapply(noisy_wts, function(X){
    setequal(names(X), names(w_nom))
  })))

})
