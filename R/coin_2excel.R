#' Write data to Excel
#'
#' Takes the COIN object and writes all main data tables and other things to an Excel file. At the moment this is just a placeholder to be developed further.
#'
#' @param COINobj A COIN object
#' @param fname The file name to write to
#'
#' @importFrom openxlsx write.xlsx createWorkbook addWorksheet writeData saveWorkbook
#'
#' @examples \dontrun{coin_2excel(COINobj, fname="COINresults.xlsx")}
#'
#' @return An Excel workbook with each table on a separate named tab.
#'
#' @export

coin_2excel <- function(COINobj, fname = "COINresults.xlsx"){

  # Create workbook
  wb <- openxlsx::createWorkbook()

  # ---- Write .$Results
  if (!is.null(COINobj$Results)){
    for (ii in 1:length(COINobj$Results)){

      # name of worksheet
      shname <- paste0("Results",names(COINobj$Results[ii]))
      # add worksheet with name
      openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "red")
      openxlsx::writeData(wb, shname, COINobj$Results[[ii]], colNames = TRUE)

    }
  }


  # ---- Write .$Data
  if (!is.null(COINobj$Data)){
    for (ii in 1:length(COINobj$Data)){

      # name of worksheet
      shname <- paste0("Data",names(COINobj$Data[ii]))
      # add worksheet with name
      openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "blue")
      openxlsx::writeData(wb, shname, COINobj$Data[[ii]], colNames = TRUE)

    }
  }

  # ---- Write .$Input
  if (!is.null(COINobj$Input)){
    for (ii in 1:length(COINobj$Input)){

      # name of worksheet
      shname <- paste0("Input",names(COINobj$Input[ii]))
      if(shname == "InputOriginal"){next}
      # add worksheet with name
      openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "gray")
      openxlsx::writeData(wb, shname, COINobj$Input[[ii]], colNames = TRUE)

    }
  }

  # ---- Write .$Analysis

  if (!is.null(COINobj$Analysis)){
    for (ii in 1:length(COINobj$Analysis)){

      thing <- COINobj$Analysis[[ii]]
      if(is.data.frame(thing)){
        # name of worksheet
        shname <- paste0("Analysis",names(COINobj$Analysis[ii]))
        # add worksheet with name
        openxlsx::addWorksheet(wb, sheetName = shname, tabColour = "green")
        openxlsx::writeData(wb, shname, COINobj$Analysis[[ii]], colNames = TRUE)
      } else if (is.list(thing)) {
        for (jj in 1:length(thing)){
          if(is.data.frame(thing[[jj]])){
            # name of worksheet
            shname <- paste0(names(COINobj$Analysis[ii]),names(thing)[jj])
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
  # for (ii in 1:length(COINobj$Parameters)){
  #
  #   openxlsx::writeData(wb, shname, names(COINobj$Parameters[ii]), startCol = ii, startRow = 1)
  #   openxlsx::writeData(wb, shname, COINobj$Parameters[[ii]], startCol = ii, startRow = 2)
  #
  # }

  # save workbook to file
  openxlsx::saveWorkbook(wb, fname, overwrite = TRUE)
}
