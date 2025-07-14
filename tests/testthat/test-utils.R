test_that("directionalise", {

  iData <- ASEM_iData[c("LPI", "Flights", "CO2")]
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  iData_ <- directionalise(iData, coin)

  iData2 <- iData
  iData2$CO2 <- -iData2$CO2

  expect_equal(iData_, iData2)

})

test_that("rbind_fill", {
  
  # --- Test Data ---
  l1 <- list(a = 1, b = 2)
  l2 <- list(b = 3, c = 4)
  v1 <- c(a = 10, b = "x")
  v2 <- c(b = "y", c = "z")
  l_same_names <- list(a = 5, b = 6)
  
  # --- Success Scenarios ---
  
  # case1: rbind_fill correctly combines two lists with different names
  result_l_l <- rbind_fill(l1, l2)
  expected_df_1 <- data.frame(a = c(1, NA), b = c(2, 3), c = c(NA, 4))
  expect_equal(result_l_l, expected_df_1)
  
  # case2: rbind_fill correctly combines two vectors with different names
  result_v_v <- rbind_fill(v1, v2)
  expected_df_2 <- data.frame(a = c("10", NA), b = c("x", "y"), c = c(NA, "z"))
  expect_equal(result_v_v, expected_df_2)
  
  # case3: rbind_fill correctly combines a list and a vector
  result_l_v <- rbind_fill(l1, v2)
  # Note: column `b` is coerced to character, `a` remains numeric
  expected_df_3 <- data.frame(a = c(1, NA), b = c("2", "y"), c = c(NA, "z"))
  expect_equal(result_l_v, expected_df_3)
  
  # case4: rbind_fill correctly combines two lists with identical names
  result_same <- rbind_fill(l1, l_same_names)
  expected_df_4 <- data.frame(a = c(1, 5), b = c(2, 6))
  expect_equal(result_same, expected_df_4)
  
  # --- Error Scenarios ---
  
  # case5: expect_error when an input is not named
  unnamed_list <- list(10, 20)
  expect_error(
    rbind_fill(l1, unnamed_list),
    "Inputs x1 and x2 must be named."
  )
  
  # case6: expect_error for invalid input types
  invalid_object <- function() {}
  expect_error(
    rbind_fill(l1, invalid_object),
    "Inputs x1 and x2 must be named lists or named vectors."
  )
})