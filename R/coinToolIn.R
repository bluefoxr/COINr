#' Import data directly from COIN Tool
#'
#' This provides a direct interface for reading a COIN Tool input deck and converting it to COINr.
#' You need to provide a COIN Tool file, with the "Database" sheet properly compiled.
#'
#' @param fname The file name and path to read, e.g. "C:/Documents/COINToolFile.xlsx"
#'
#' @importFrom readxl read_excel cell_limits
#' @importFrom tibble as_tibble
#' @importFrom purrr map_lgl
#' @importFrom dplyr filter
#'
#' @examples \dontrun{
#' out <- coinToolIn("C:/Documents/COINToolFile.xlsx")}
#'
#' @return A list
#'
#' @export

COINToolIn <- function(fname){

  #----- GET IndData -----#

  # Get the main data first
  ind_data_only <- readxl::read_excel(fname, range = "E16:CY315", na = "n/a", col_types = "numeric",
                                      sheet = "Database")
  # Delete any rows and cols with all NAs
  ind_data_only  <- ind_data_only[rowSums(is.na(ind_data_only )) != ncol(ind_data_only ), ]
  ind_data_only  <- ind_data_only[,colSums(is.na(ind_data_only )) != nrow(ind_data_only )]

  # Reference points
  lastcol <- ncol(ind_data_only)+4
  lastrow <- nrow(ind_data_only)+16

  # Unit names and codes (together)
  UnitNamesCodes <- readxl::read_excel(fname, range = readxl::cell_limits(c(17, 2), c(lastrow, 3)),
                                       col_types = "text", col_names = FALSE, sheet = "Database")

  # Assemble IndData
  IndData <- cbind(UnitNamesCodes, ind_data_only) %>% tibble::as_tibble()
  colnames(IndData)[1:2] <- c("UnitName", "UnitCode")



  #----- GET IndMeta -----#

  # IndMeta (partial)
  IndMeta1 <- readxl::read_excel(fname, range = readxl::cell_limits(c(11, 5), c(16, lastcol)),
                                 col_types = "text", col_names = FALSE, sheet = "Database")
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
                                 col_types = "numeric", col_names = FALSE, sheet = "Database")

  # Put into tidy format
  IndMeta2 <- IndMeta2 %>% t() %>% as.data.frame() %>% rev()

  # Assemble IndMeta
  IndMeta <- cbind(IndMeta1[1:2], IndMeta2, aggcols) %>% tibble::as_tibble()
  colnames(IndMeta)[1:6] <- c("IndCode", "IndName", "GPupper", "GPlower", "Direction", "IndWeight")

  #----- Get AggMeta -----#

  # Read in aggmeta cols
  AggMetaIn <- readxl::read_excel(fname, range = "C5:H53", col_names = TRUE, sheet = "Framework")
  # Delete empty rows
  AggMetaIn <- dplyr::filter(AggMetaIn,`Dimension/indicator`!="--")
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

  message(paste0("Imported ", ncol(ind_data_only), " indicators and ", nrow(ind_data_only), " units."))

  return(list(IndData = IndData,
              IndMeta = IndMeta,
              AggMeta = AggMetaIn))

}
