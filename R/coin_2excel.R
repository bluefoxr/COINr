#' Write a COIN to Excel
#'
#' Takes a COIN and writes all main data tables and other things to an Excel file. This uses the 'openxlsx' package.
#'
#' @param COIN A COIN object
#' @param fname The file name to write to, as a character string
#'
#' @importFrom openxlsx write.xlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples
#'
#' ## Here we write a COIN to Excel, but this is done to a temporary directory
#' ## to avoid "polluting" the working directory when running automatic tests.
#' ## In a real case, set fname to a directory of your choice.
#' ASEM <- build_ASEM()
#' # write to Excel in temporary directory
#' coin2Excel(ASEM, fname = paste0(tempdir(), "\\ASEM_results.xlsx"))
#' # spreadsheet is at:
#' print(paste0(tempdir(), "\\ASEM_results.xlsx"))
#' # now delete temporary file to keep things tidy in testing
#' unlink(paste0(tempdir(),"\\ASEM_results.xlsx"))
#'
#' @return An Excel workbook with each table on a separate named tab.
#'
#' @export

coin2Excel <- function(COIN, fname = "COINresults.xlsx"){

  stopifnot(is.coin(COIN))

  # Create workbook
  wb <- openxlsx::createWorkbook()

  # ---- Write .$Results
  if (!is.null(COIN$Results)){
    for (ii in 1:length(COIN$Results)){

      # name of worksheet
      shname <- paste0("Results",names(COIN$Results[ii]))
      # add worksheet with name
      openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "red")
      openxlsx::writeData(wb, shname, COIN$Results[[ii]], colNames = TRUE)

    }
  }


  # ---- Write .$Data
  if (!is.null(COIN$Data)){
    for (ii in 1:length(COIN$Data)){

      # name of worksheet
      shname <- paste0("Data",names(COIN$Data[ii]))
      # add worksheet with name
      openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "blue")
      openxlsx::writeData(wb, shname, COIN$Data[[ii]], colNames = TRUE)

    }
  }

  # ---- Write .$Input
  if (!is.null(COIN$Input)){
    for (ii in 1:length(COIN$Input)){

      # name of worksheet
      shname <- paste0("Input",names(COIN$Input[ii]))
      if(shname == "InputOriginal"){next}
      # add worksheet with name
      openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "gray")
      openxlsx::writeData(wb, shname, COIN$Input[[ii]], colNames = TRUE)

    }
  }

  # ---- Write .$Analysis

  if (!is.null(COIN$Analysis)){
    for (ii in 1:length(COIN$Analysis)){

      thing <- COIN$Analysis[[ii]]
      if(is.data.frame(thing)){
        # name of worksheet
        shname <- paste0("Analysis",names(COIN$Analysis[ii]))
        # add worksheet with name
        openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "green")
        openxlsx::writeData(wb, shname, COIN$Analysis[[ii]], colNames = TRUE)
      } else if (is.list(thing)) {
        for (jj in 1:length(thing)){
          if(is.data.frame(thing[[jj]])){
            # name of worksheet
            shname <- paste0(names(COIN$Analysis[ii]),names(thing)[jj])
            # add worksheet with name
            openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "green")
            openxlsx::writeData(wb, shname, thing[[jj]], colNames = TRUE)
          }
        }
      }

    }
  }

  # ---- Write .$Parameters
  # NOTE: throws an error because params have different lengths. Not sure how to fix, come back later.
  # shname <- "Parameters"
  # openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "pink")
  # for (ii in 1:length(COIN$Parameters)){
  #
  #   openxlsx::writeData(wb, shname, names(COIN$Parameters[ii]), startCol = ii, startRow = 1)
  #   openxlsx::writeData(wb, shname, COIN$Parameters[[ii]], startCol = ii, startRow = 2)
  #
  # }

  # save workbook to file
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
}
