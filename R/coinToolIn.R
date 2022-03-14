#' Import data directly from COIN Tool
#'
#' This provides a direct interface for reading a COIN Tool input deck and converting it to COINr.
#' You need to provide a COIN Tool file, with the "Database" sheet properly compiled.
#'
#' The [COIN Tool](https://knowledge4policy.ec.europa.eu/composite-indicators/coin-tool_en) is an Excel-based tool for building composite indicators.
#'
#' See [COINr online documentation](https://bluefoxr.github.io/COINrDoc/helper-functions.html#r-interfaces) for more details and an example.
#'
#' @param fname The file name and path to read, e.g. `"C:/Documents/COINToolFile.xlsx"`.
#' @param makecodes Logical: if `TRUE`, will generate short indicator codes based on indicator names,
#' otherwise if `FALSE`, will use COIN Tool indicator codes `"Ind.01"`, etc. Currently only does this
#' for indicators, not aggregation groups.
#' @param oldtool Logical: if `TRUE`, compatible with old COIN Tool (pre-release, early 2019 or earlier).
#' There are some minor differences on where the elements are found.
#'
#' @importFrom readxl read_excel cell_limits
#' @importFrom tibble as_tibble
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' ## This example downloads a COIN Tool spreadsheet containing example data,
#' ## saves it to a temporary directory, unzips, and reads into R. Finally it
#' ## assembles it into a COIN.
#'
#' # Make temp zip filename in temporary directory
#' tmpz <- tempfile(fileext = ".zip")
#' # Download an example COIN Tool file to temporary directory
#' # NOTE: the download.file() command may need its "method" option set to a
#' # specific value depending on the platform you run this on. You can also
#' # choose to download/unzip this file manually.
#' download.file("https://knowledge4policy.ec.europa.eu/sites/default/
#' files/coin_tool_v1_lite_exampledata.zip", tmpz)
#' # Unzip
#' CTpath <- unzip(tmpz, exdir = tempdir())
#' # Read COIN Tool into R
#' l <- COINToolIn(CTpath, makecodes = TRUE)
#' # We can finish by assembling it
#' CT_exampleCOIN <- assemble(l$IndData, l$IndMeta, l$AggMeta)
#' }
#'
#' @seealso
#' * [coin2Excel()] Export COIN contents to Excel
#'
#' @return A list containing:
#' * `.$IndData` A data frame of imported indicator data to be input as the `IndData` argument in [assemble()].
#' * `.$IndMeta` A data frame of imported indicator metadata to be input as the `IndMeta` argument in [assemble()].
#' * `.$AggMeta` A data frame of imported aggregation metadata to be input as the `AggMeta` argument in [assemble()].
#'
#' @export

COINToolIn <- function(fname, makecodes = FALSE, oldtool = FALSE){

  #----- GET IndData -----#

  # Get the main data first
  ind_data_only <- readxl::read_excel(fname, range = "E16:CY315", na = "n/a", col_types = "numeric",
                                      sheet = "Database") %>% suppressMessages()
  # Delete any rows and cols with all NAs
  ind_data_only  <- ind_data_only[rowSums(is.na(ind_data_only )) != ncol(ind_data_only ), ]
  ind_data_only  <- ind_data_only[,colSums(is.na(ind_data_only )) != nrow(ind_data_only )]

  # Reference points
  lastcol <- ncol(ind_data_only)+4
  lastrow <- nrow(ind_data_only)+16

  # Unit names and codes (together)
  UnitNamesCodes <- readxl::read_excel(fname, range = readxl::cell_limits(c(17, 2), c(lastrow, 3)),
                                       col_types = "text", col_names = FALSE, sheet = "Database") %>%
    suppressMessages()


  # Assemble IndData
  IndData <- cbind(UnitNamesCodes, ind_data_only) %>% tibble::as_tibble()
  colnames(IndData)[1:2] <- c("UnitName", "UnitCode")

  #----- GET IndMeta -----#

  # IndMeta (partial)
  IndMeta1 <- readxl::read_excel(fname, range = readxl::cell_limits(c(11, 5), c(16, lastcol)),
                                 col_types = "text", col_names = FALSE, sheet = "Database") %>%
    suppressMessages()

  # Put into tidy format
  IndMeta1 <- IndMeta1 %>% t() %>% as.data.frame() %>% rev()
  # Sort out aggregation columns
  aggcols <- IndMeta1[3:6]
  # Any cols with all same agg codes - means that one of the COIN Tool levels was not used
  fakecols <- purrr::map_lgl(aggcols, function(x) length(unique(x))==1)
  # I have to manually set the last col to FALSE because this is the Index col and I want to keep
  fakecols[4]<-FALSE
  # Now aggcols with any fake rows removed
  aggcols <- aggcols[!fakecols]
  # Name cols already, avoids problems later
  colnames(aggcols) <- paste0("Agg",1:ncol(aggcols))

  # Weights, directions, goalposts
  IndMeta2 <- readxl::read_excel(fname, range = readxl::cell_limits(c(7, 5), c(10, lastcol)),
                                 col_types = "numeric", col_names = FALSE, sheet = "Database") %>%
    suppressMessages()

  # Put into tidy format
  IndMeta2 <- IndMeta2 %>% t() %>% as.data.frame() %>% rev()

  # Assemble IndMeta
  IndMeta <- cbind(IndMeta1[1:2], IndMeta2, aggcols) %>% tibble::as_tibble()
  colnames(IndMeta)[1:6] <- c("IndCode", "IndName", "GPupper", "GPlower", "Direction", "IndWeight")

  #----- Get AggMeta -----#

  # Read in aggmeta cols
  # this is the only diff with the older CT - framework rows are 1 further down
  if(oldtool){
    AggMetaIn <- readxl::read_excel(fname, range = "C5:H53", col_names = TRUE, sheet = "Framework")
  } else {
    AggMetaIn <- readxl::read_excel(fname, range = "C4:H52", col_names = TRUE, sheet = "Framework")
  }

  # Delete empty rows
  AggMetaIn <- dplyr::filter(AggMetaIn,.data$`Dimension/indicator`!="--")
  # Get rid of cols we don't want
  AggMetaIn <- cbind(0, AggMetaIn[c(1,6,3)]) %>% dplyr::as_tibble()
  # Rename cols
  colnames(AggMetaIn) <- c("AgLevel", "Code", "Name", "Weight")

  # Put in correct levels
  levs <- c("sp.", "p.", "si.", "Index")
  # remove any unused level codes
  levs <- levs[!fakecols]
  levno <- 1:length(levs)+1
  for(ii in 1:length(levno)){
    AggMetaIn$AgLevel[startsWith(AggMetaIn$Code, levs[ii])] <- levno[ii]
  }
  AggMetaIn <- AggMetaIn[AggMetaIn$AgLevel != 0,]

  #----- Finish up -----#

  # generate indicator codes if asked
  if(makecodes){
    IndMeta$IndCode <- names2Codes(IndMeta$IndName)
    colnames(IndData)[3:ncol(IndData)] <- IndMeta$IndCode
    #AggMetaIn$Code <- names2Codes(AggMetaIn$Name)
  }

  message(paste0("Imported ", ncol(ind_data_only), " indicators and ", nrow(ind_data_only), " units."))

  return(list(IndData = IndData,
              IndMeta = IndMeta,
              AggMeta = AggMetaIn))

}

#' Generate short codes from long names
#'
#' Given a character vector of long names (probably with spaces), generates short codes.
#' Intended for use when importing from the COIN Tool.
#'
#' @param cvec A character vector of names
#' @param maxword The maximum number of words to use in building a short name (default 2)
#' @param maxlet The number of letters to take from each word (default 4)
#'
#' @examples
#' #
#'
#' @seealso
#' * [COINToolIn()] Import data from the COIN Tool (Excel).
#'
#' @return A corresponding character vector, but with short codes, and no duplicates.
#'
#' @export

names2Codes <- function(cvec, maxword=2, maxlet=4){

  # There is definitely a better way to do this with lapply or similar, but for now...

  codes <- cvec

  for (ii in 1:length(cvec)){

    cvecii <- cvec[ii]

    # first, split into separate elements using spaces, and remove words less than four chars
    st2 <- unlist(strsplit(gsub('\\b\\w{1,3}\\s','',cvecii), " +"))

    nwords <- min(c(length(st2),maxword))

    # now take first 3 words, take first 4 chars of each word
    st3 <- substr(st2[1:nwords],start=1,stop=maxlet)

    # capitalise first letter of each word
    st3 <- gsub("\\b([[:lower:]])([[:lower:]]+)", "\\U\\1\\L\\2", st3, perl = TRUE)

    # collapse back to one string and add to new vector
    codes[ii] <- paste(st3, collapse = '')
  }

  # if we have any duplicates, make unique
  make.unique(codes, "_")
}
