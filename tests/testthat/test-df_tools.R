test_that("rank_df", {

  df1 <- data.frame(x = c(1, 3, 2, 4),
                    y = c(4, 1, 3, 2))
  df1r <- rank_df(df1)

  expect_s3_class(df1r, "data.frame")
  expect_equal(df1r$x, c(4, 2, 3, 1))
  expect_equal(df1r$y, c(1, 4, 2, 3))

  df2 <- cbind(grp = c("a", "a", "b", "b"), df1)
  df2r <- rank_df(df2, use_group = "grp")

  expect_s3_class(df2r, "data.frame")
  expect_equal(df2r$x, c(2, 1, 2, 1))
  expect_equal(df2r$y, c(1, 2, 1, 2))

})

test_that("compare_df",{

  # take a sample of indicator data (including the uCode column)
  data1 <- ASEM_iData[c(2,12:15)]
  # copy the data
  data2 <- data1

  # first, check that if same, this is correct
  l <- compare_df(data1, data2, matchcol = "uCode")
  expect_true(l$Same)

  # now make some changes and see how responds
  # re-ordering cols
  data2 <- rev(data2)
  l <- compare_df(data1, data2, matchcol = "uCode")
  expect_true(l$Same)

  # reorder rows
  data2 <- data2[nrow(data2):1, ]
  l <- compare_df(data1, data2, matchcol = "uCode")
  expect_true(l$Same)

  # now make some changes
  data2 <- data1
  # replace one value in data2 by NA
  data2[1,"LPI"] <- NA
  l <- compare_df(data1, data2, matchcol = "uCode")
  expect_false(l$Same)
  expect_true(all(l$Details$TheSame[l$Details$Column %nin% "LPI"]))
  expect_false(l$Details$TheSame[l$Details$Column == "LPI"])
  expect_equal(l$Details$NDifferent[l$Details$Column == "LPI"], 1)
  expect_true(all(l$Details$NDifferent[l$Details$Column %nin% "LPI"] == 0))

  # test varying dimensions
  data2 <- data1
  data2 <- data2[-1,]
  l <- compare_df(data1, data2, matchcol = "uCode")
  expect_false(l$Same)

  data2 <- data1
  data2 <- data2[,-4]
  l <- compare_df(data1, data2, matchcol = "uCode")
  expect_false(l$Same)

  # test character variation
  # take a sample of indicator data (including the uCode column)
  data1 <- ASEM_iData[c(1,2,12:15)]
  # copy the data
  data2 <- data1
  # change name
  data2$uName[1] <- "la-la-land"
  l <- compare_df(data1, data2, matchcol = "uCode")
  expect_false(l$Same)

})

test_that("replace_df",{

  # replace sub-pillar codes in ASEM indicator metadata
  codeswap <- data.frame(old = c("Conn", "Sust"), new = c("SI1", "SI2"))

  # swap codes in both iMeta
  df1 <- replace_df(ASEM_iMeta, codeswap)

  # check
  expect_true(all(df1$iCode[ASEM_iMeta$iCode == "Conn"] == "SI1"))
  expect_true(all(df1$iCode[ASEM_iMeta$iCode == "Sust"] == "SI2"))
  expect_true(all(na.omit(df1$Parent[ASEM_iMeta$Parent == "Conn"]) == "SI1"))
  expect_true(all(na.omit(df1$Parent[ASEM_iMeta$Parent == "Sust"]) == "SI2"))

})

test_that("round_df", {

  # test df
  X <- data.frame(lab = letters[1:5],
                  x = runif(5),
                  y = runif(5))

  Xr <- round_df(X, decimals = 3)
  expect_equal(names(X), names(Xr))
  expect_equal(X$lab, Xr$lab)
  expect_equal(Xr$x, round(X$x, 3))
  expect_equal(Xr$y, round(X$y, 3))

})

test_that("signif_df", {

  # test df
  X <- data.frame(lab = letters[1:5],
                  x = runif(5),
                  y = runif(5))

  Xr <- signif_df(X, digits = 3)
  expect_equal(names(X), names(Xr))
  expect_equal(X$lab, Xr$lab)
  expect_equal(Xr$x, signif(X$x, 3))
  expect_equal(Xr$y, signif(X$y, 3))

})
