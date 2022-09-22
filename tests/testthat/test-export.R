test_that("write2excel", {

  # build example coin up to data treatment step
  coin <- build_example_coin(up_to = "Treat", quietly = TRUE)

  # write to Excel in temporary directory
  no_output <- export_to_excel(x = coin, fname = paste0(tempdir(), "\\ASEM_results.xlsx"))

  # spreadsheet is at:
  expect_true(file.exists(paste0(tempdir(), "\\ASEM_results.xlsx")))

  # now delete temporary file to keep things tidy
  unlink(paste0(tempdir(),"\\ASEM_results.xlsx"))

  # PURSE METHOD
  purse <- build_example_purse(up_to = "Treat", quietly = TRUE)

  # write to Excel in temporary directory
  no_output <- export_to_excel(purse, fname = paste0(tempdir(), "\\ASEM_results_p.xlsx"))

  # spreadsheet is at:
  expect_true(file.exists(paste0(tempdir(), "\\ASEM_results_p.xlsx")))

  # now delete temporary file to keep things tidy
  unlink(paste0(tempdir(),"\\ASEM_results_p.xlsx"))
})
