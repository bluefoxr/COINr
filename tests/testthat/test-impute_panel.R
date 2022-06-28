# check that when introducing some NAs, we get the expected results

test_that("panel imputation correct", {

  # Copy example panel data
  iData_p <- ASEM_iData_p

  # we introduce two NAs: one for NZ in 2022 in LPI indicator
  iData_p$LPI[iData_p$uCode == "NZ" & iData_p$Time == 2022] <-  NA
  # one for AT, also in 2022, but for Flights indicator
  iData_p$Flights[iData_p$uCode == "AT" & iData_p$Time == 2022] <- NA

  # impute: target only the two columns where NAs introduced
  l_imp <- impute_panel(iData_p, cols = c("LPI", "Flights"))
  # get imputed df
  iData_imp <- l_imp$iData_imp

  # check the output is what we expect: both NAs introduced should now have 2021 values
  expect_equal(iData_imp$LPI[iData_imp$uCode == "NZ" & iData_imp$Time == 2022],
               ASEM_iData_p$LPI[ASEM_iData_p$uCode == "NZ" & ASEM_iData_p$Time == 2021]
  )

  expect_equal(iData_imp$Flights[iData_imp$uCode == "AT" & iData_imp$Time == 2022],
               ASEM_iData_p$Flights[ASEM_iData_p$uCode == "AT" & ASEM_iData_p$Time == 2021])

  expect_equal(nrow(iData_imp), nrow(iData_p))

  expect_setequal(names(iData_p), names(iData_imp))

})
