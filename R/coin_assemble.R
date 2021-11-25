#' Build COIN object
#'
#' This takes the raw data provided by the user and puts it into a list format (COIN object) that is recognised by COINr.
#' It also checks whether there are any syntax errors in the data provided. Optionally, you can exclude
#' or include indicators using the `include` and `exclude` arguments. Note that if an indicator is specified in
#' BOTH `include` and `exclude`, it will be excluded.
#'
#' A "COIN" is an S3 class which is a structured list of indicator data, metadata, results and methodology which is used
#' throughout COINr. COINs are a convenient way to store all variables relating to the composite indicator in a single named
#' object. This keeps the workspace tidy, but also allows fast and concise calls to functions, as well as copying COINs to
#' introduce methodological variations, and enables complex operations such as global sensitivity analysis (see [sensitivity()].
#'
#' If panel data is input to this function, it will output a tibble of COINs (see `use_year`). This feature
#' is currently under development and more support will be included for these tibbles of COINs in COINr over
#' time.
#'
#' For general information on COINs see the COINr vignette as well as the [relevant chapter](https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html) in the COINr online documentation.
#'
#' For details on copying, adjusting and comparing COINs see the [COINr chapter on adjustments and comparisons](https://bluefoxr.github.io/COINrDoc/adjustments-and-comparisons.html).
#'
#' @param IndData A data frame of indicator data.
#' @param IndMeta A data frame containing auxiliary information for each indicator
#' @param AggMeta A data frame specifying the names and weights of each aggregation group
#' @param include Optional argument specifying a subset of indicator codes to include (default all indicators included)
#' @param exclude Optional argument specifying a subset of indicator codes to exclude (default none excluded)
#' @param preagg Set to `TRUE` if you want to assemble a COIN using pre-aggregated data (typically for ex-post analysis)
#' @param use_year If `IndData` includes a `Year` column, and there are multiple observations for each unit (one per year),
#' this can be set to a target year or years. For example, setting `use_year = 2020` will filter `IndData` to only include points from
#' 2020. Setting to `use_year = c(2019,2020)` will return a list of COINs. Set `use_year = "all"` to return
#' a COIN for all years where data is available. Keep in mind that a COIN represents a single year of data.
#' @param impute_latest Logical: if `TRUE`, imputes missing data points using most recent value from previous years. If `FALSE`
#' (default) simply extracts the data frame as is. This only works if `!is.null(use_year)` and there are previous years of data
#' available (before `use_year`). Currently does not support imputation using future values or interpolation.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "select"
#' @importFrom dplyr "arrange"
#' @importFrom dplyr "across"
#' @importFrom dplyr "starts_with"
#' @importFrom dplyr "ends_with"
#' @importFrom dplyr "n_distinct"
#' @importFrom purrr "map_lgl"
#' @importFrom stats "na.omit"
#' @importFrom tibble tibble
#'
#' @examples
#' # build the ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#'
#' @return A "COIN" S3 class object (list) formatted to the specifications of COINr. If the input is panel
#' data and `use_year` is set to return multiple years, this function returns a tibble of COINs, indexed
#' by the year. This latter feature is new and currently under development.
#'
#' @export

assemble <- function(IndData, IndMeta, AggMeta, include = NULL, exclude = NULL,
                     preagg = NULL, use_year = NULL, impute_latest = FALSE){

  ##----- SET DEFAULTS -------##
  # Done here because otherwise if we use regen, this input could be input as NULL
  if(is.null(preagg)){
    preagg <- FALSE
  }

  ##----- INITIAL CHECKS -----##

  stopifnot(is.data.frame(IndData),
            is.data.frame(IndMeta),
            is.data.frame(AggMeta))

  # Do some checks first - make sure required cols are present
  if(!exists("UnitCode", IndData)){
    stop("No UnitCode column found in IndData. This column is required for assembling a COIN object.")
  }
  if(!exists("UnitName", IndData)){
    stop("No UnitName column found in IndData. This column is required for assembling a COIN object.")
  }
  if(!exists("IndName", IndMeta)){
    stop("No IndName column found in IndMeta. This column is required for assembling a COIN object.")
  }
  if(!exists("IndCode", IndMeta)){
    stop("No IndCode column found in IndMeta. This column is required for assembling a COIN object.")
  }
  if(!exists("Direction", IndMeta)){
    stop("No Direction column found in IndMeta. This column is required for assembling a COIN object.")
  }
  if(!exists("IndWeight", IndMeta)){
    stop("No IndWeight column found in IndMeta. This column is required for assembling a COIN object.")
  }

  # also check aggmeta
  if(any(!(c("AgLevel", "Code", "Name", "Weight") %in% colnames(AggMeta)))){
    stop("One or more of required columns 'AgLevel', 'Code', 'Name', 'Weight' not found in AggMeta. Please check.")
  }

  # check classes of required columns in each df
  # IndData
  if( !(is.character(IndData$UnitCode) & is.character(IndData$UnitName)) ){
    stop("One or more of IndData$UnitCode or IndData$UnitName is not a character vector. Make sure all codes and names are characters (not numeric).")
  }
  # IndMeta
  if( !(is.character(IndMeta$IndName) & is.character(IndMeta$IndCode)) ){
    stop("One or more of IndMeta$IndName or IndMeta$IndCode is not a character vector. Make sure all codes and names are characters (not numeric).")
  }
  if( !(is.numeric(IndMeta$Direction) & is.numeric(IndMeta$IndWeight)) ){
    stop("One or more of IndMeta$Direction or IndMeta$IndWeight is not a numeric vector. Weights and directions are required to be numeric.")
  }
  # AggMeta
  if( !(is.character(AggMeta$Code) & is.character(AggMeta$Name)) ){
    stop("One or more of AggMeta$Code or AggMeta$Name is not a character vector. Make sure all codes and names are characters (not numeric).")
  }
  if( !(is.numeric(AggMeta$AgLevel) & is.numeric(AggMeta$Weight)) ){
    stop("One or more of AggMeta$AgLevel or AggMeta$Weight is not a numeric vector. AgLevel and Weight are required to be numeric.")
  }


  # copy ind data before going any further. Used in .$Input$Original
  IndDataOrig <- IndData

  ##----- PANEL DATA ---------------##

  # The objective here is to give a simple option to filter panel data to a certain year.
  # It will also optionally impute using the latest year available.

  if(!is.null(use_year)){

    # first we get the years to use
    if(is.character(use_year)){
      if(use_year=="all"){
        yrs <- IndData$Year |> unique() |> sort()
      } else {
        stop("use_year not recogised. Should be numerical (vector) or 'all'.")
      }
    } else {
      if(!all(use_year %in% IndData$Year)){
        stop("One or more of the time points in use_year not found in Year column of IndData.")
      }
      yrs <- use_year |> sort()
    }

    if(length(yrs)==1){
      l_imp <- extractYear(use_year, IndData, impute_latest)
      # extract indicator data
      IndData <- l_imp$IndDataImp
      # IndData is a data frame
    } else {
      # pass to function (see below in this file)
      IndData_List <- lapply(yrs, extractYear, IndData, impute_latest)
      # get just the dfs
      IndDatas <- lapply(IndData_List, function(x){x$IndDataImp})
    }

  }

  ##----- ASSEMBLE COIN FUNC -----
  # We now have either a list of IndDatas if panel data was input, or else a single IndData.
  # The following is a function which assembles a single COIN for a given IndData.
  # After defining the function, we will run it for the IndData(s)
  makeCOIN <- function(IndData){

    ##----- IND CODES AND DENOMS -----

    # Extract indicator codes from raw data
    cnames1 <- IndData %>% dplyr::select(!dplyr::starts_with(
      c("UnitCode", "UnitName", "Year", "Group_","Den_", "IndUnit", "x_")) ) %>% colnames()

    # check for any non-numeric cols and stop if any present
    ind_data_only <- IndData[cnames1]
    not_num <- cnames1[!purrr::map_lgl(ind_data_only, is.numeric)]
    if(length(not_num)>0){
      # stop, print any non-numeric
      message("Non-numerical columns in IndData (probably character/text?):")
      print(not_num)
      stop(paste0("Non-numeric columns detected. Only numerical indicators are allowed"))
    }

    # In case no indicator cols present
    if(is.null(cnames1)){
      stop("No indicators found. Please check column names.")
    }

    # everything apart from denoms is IndData
    IndData1 <- IndData %>% dplyr::select(!dplyr::starts_with("Den_"))

    # if the everything-apart-from-denoms is diff from the original data, means there are denoms
    # So, extract denoms
    message("-----------------")
    if(ncol(IndData1) < ncol(IndData)){
      denoms <- IndData %>% dplyr::select(
        dplyr::starts_with(c("UnitCode", "UnitName", "Year", "Group_", "Den_", "x_"))) # keep denominators to one side for the moment
      message("Denominators detected - stored in .$Input$Denominators")
    } else {
      denoms <- NULL # this will have the effect of not attaching to the list
      message("No denominators detected.")
    }
    message("-----------------")

    # from this point, IndData is minus any denominators
    IndData <- IndData1


    ##------- Select indicators, if needed -----##

    include0 <- include
    exclude0 <- exclude

    # if include is not specified, include everything
    if(is.null(include)){include <- cnames1}

    # the vector of indicators to include is everything in include, minus anything in exclude
    include <- setdiff(include,exclude)

    # select data and metadata accordingly
    if(!is.null(include)){
      if(any(!(include %in% IndMeta$IndCode)) & !preagg){
        # note this is not implemented if preagg is TRUE, because aggregates are not present in IndMeta
        warning("Some codes in include or exclude are not present in the indicator data and metadata. Ignoring any codes that are not found.")
        include <- include[include %in% IndMeta$IndCode]
      }

      ind_data <- IndData %>% dplyr::select(dplyr::starts_with(
        c("UnitCode", "UnitName", "Year", "Group_", "IndUnit", "x_")), include )
      ind_meta <- IndMeta[IndMeta$IndCode %in% include,]
    }

    # sort ind_meta properly according to structure of index
    # note: I sort according to the reversed order of the agg columns, so starting with the
    # highest level of aggregation first then working down

    ind_meta <- dplyr::arrange(ind_meta,
                               rev(dplyr::across(dplyr::starts_with("Agg"))))

    # I want the indicator cols to be in the same order as ind_meta, to avoid surprises
    # Don't do for preagg though, because there would be extra cols.
    if (!preagg){
      ind_data <- dplyr::select(ind_data,
                                dplyr::starts_with(c("UnitCode", "UnitName",
                                                     "Year", "Group_", "IndUnit", "x_")), ind_meta$IndCode)
    }

    # Build list
    COINobj <- list(Input = list(
      IndData = ind_data,
      IndMeta = ind_meta,
      AggMeta = AggMeta,
      Original = list(
        IndData = IndDataOrig,
        IndMeta = IndMeta,
        AggMeta = AggMeta
      )))
    #Parameters = NULL, # for various model parameters (will be populated in a min)
    #Analysis = NULL, # for analysis of missing data, correlation etc
    #Method = NULL) # a record of the methodology applied to build the index

    # add denoms separately - this is because if it is NULL it will disappear
    COINobj$Input$Denominators = denoms

    # Add $Data - depends on preagg
    if(preagg){

      COINobj$Data = list(PreAggregated = ind_data)

      # Check that codes in the two tables match, save to list
      if (!setequal(cnames1, c(IndMeta$IndCode, AggMeta$Code))){
        stop("Indicator/agg codes in metadata tables and indicator table are not the same. Please correct.")
      }

    } else {

      COINobj$Data = list(Raw = ind_data) # for various datasets as they emerge (raw, treated, etc.)

      # Check that codes in the two tables match, save to list
      if (!setequal(cnames1,IndMeta$IndCode)){
        stop("Indicator codes in metadata table and indicator table are not the same. Please correct.")
      }
    }

    # add some info if by-year data was used
    if(!is.null(use_year) & impute_latest){
      COINobj$Analysis$Years$DataYears <- l_imp$DataYears
      COINobj$Analysis$Years$ImpTable <- l_imp$ImpTable
      COINobj$Analysis$Years$NImputed <- l_imp$NImputed
    }

    # Check for duplicates
    if(anyDuplicated(cnames1) != 0){
      stop("Duplicate indicator codes detected - please ensure indicator codes are unique and try again.")
    }
    if(anyDuplicated(ind_data$UnitCode) != 0){
      stop("Duplicate unit codes detected - please ensure unit codes are unique and try again. If you have panel data, set use_year.")
    }
    if(anyDuplicated(c(ind_data$UnitCode, cnames1)) != 0){
      stop("At least one unit code is the same as an indicator code. Please make sure all codes are unique.")
    }

    message("-----------------")
    message("Indicator codes cross-checked and OK.")
    message("-----------------")
    COINobj$Parameters$NInd <- length(ind_meta$IndCode) # save to list
    COINobj$Parameters$IndCodes <- ind_meta$IndCode # save to list
    message(paste("Number of indicators =",COINobj$Parameters$NInd))
    message(paste("Number of units =", dplyr::n_distinct(ind_data$UnitCode, na.rm = TRUE)))

    # Check aggregation levels present and say how many
    agg_cols <- ind_meta %>% dplyr::select(dplyr::starts_with("Agg"))
    # In case no aggregation columns present
    if(is.null(agg_cols)){
      stop("No aggregation columns detected in IndMeta. Aggregation column names must start with 'Agg'.")
    }
    n_agg_levels <- length(agg_cols)
    COINobj$Parameters$Nlevels <- n_agg_levels + 1 # add 1 because indicator level not included

    # list with aggregation group names in, for each level
    AggCodeslist <- vector(mode = "list", length = n_agg_levels)
    for (ii in 1:n_agg_levels){
      AggCodeslist[[ii]] <- AggMeta$Code[AggMeta$AgLevel==ii+1]
    }
    COINobj$Parameters$AggCodes <- AggCodeslist

    # check for duplicates in aggmeta
    if(anyDuplicated(AggMeta$Code) != 0){
      stop("Duplicate codes found in AggMeta - please check.")
    }

    # run a check to make sure that each code is only assigned to ONE parent and not to multiple parents
    fwk <- ind_meta %>% dplyr::select(.data$IndCode, dplyr::starts_with("Agg"))

    for (ii in 1:(ncol(fwk)-1)){
      # get aggregation col plus its parent column
      child_parent <- fwk[c(ii, ii + 1)]
      # remove any duplicates (full rows)
      child_parent <- unique(child_parent)
      # at this point, each CHILD should only be present once. Otherwise it is being assigned to multiple parents
      if(anyDuplicated(child_parent[1]) != 0){
        stop(paste0("You have assigned an indicator or aggregate to more than one parent. This was detected in Level ", ii, ". Please fix."))
      }
    }

    if (n_agg_levels > 0){

      message(paste("Number of aggregation levels =", n_agg_levels, "above indicator level."))
      message("-----------------")

      # Loop through aggregation levels. Get names of agg groups and store, plus print to console.
      for (agg_no in 1:n_agg_levels){

        agg_names <- COINobj$Parameters$AggCodes[[agg_no]]
        n_agg_groups <- length(agg_names)
        message(paste("Aggregation level",agg_no,"with",n_agg_groups,"aggregate groups:",paste0(agg_names, collapse=", ")))

        agg_names2 <- unique(agg_cols[[agg_no]]) # cross check to see if agg names match

        if (!setequal(agg_names,agg_names2)){
          stop(paste0("Aggregation codes in framework are not consistent with metadata - this occured in Level ", agg_no+1))
        }  else {
          message("Cross-check between metadata and framework = OK.")
        }

      }

    } else {

      warning("No aggregation levels were detected.")

    }

    # Add framework (useful in many functions)
    COINobj$Parameters$Structure <- fwk

    message("-----------------")

    #------- Also get weights for all levels
    agg_cols <- ind_meta %>% dplyr::select(dplyr::starts_with("Agg"))
    n_agg_levels <- length(agg_cols)

    # want to check that weights are numeric and not accidentally character
    if(!is.numeric(ind_meta$IndWeight)){
      stop("Indicator weights (in IndMeta) not numeric - please check data was imported correctly.")
    }
    if(!is.numeric(AggMeta$Weight)){
      stop("Weights in AggMeta are not numeric - please check data was imported correctly.")
    }

    agweights <- data.frame(AgLevel = 1,
                            Code = ind_meta$IndCode,
                            Weight = ind_meta$IndWeight)

    agweights <- rbind(agweights,
                       AggMeta[c("AgLevel", "Code", "Weight")])

    # squirrel away in object
    COINobj$Parameters$Weights$Original <- agweights

    #------- Create a lookup dictionary for codes <--> names
    # this is like a lookup table of all indicator/agg codes and names
    COINobj$Parameters$Code2Name <- rbind(
      cbind(ind_meta$IndCode, ind_meta$IndName),
      cbind(AggMeta$Code, AggMeta$Name) ) %>% as.data.frame()
    colnames(COINobj$Parameters$Code2Name) <- c("AggCode", "AggName")

    if(anyDuplicated(COINobj$Parameters$Code2Name$AggCode) != 0){
      stop("Duplicate codes found between indicators and aggregates - please check.")
    }

    #------- Last bits

    # record function arguments
    COINobj$Method$assemble$include <- include0
    COINobj$Method$assemble$exclude <- exclude0
    COINobj$Method$assemble$preagg <- preagg
    COINobj$Method$assemble$use_year <- use_year
    COINobj$Method$assemble$impute_latest <- impute_latest

    class(COINobj) <- "COIN" # assigns a "COIN" class to the list. Helpful for later on.

    return(COINobj)

  }

  ##----- MAKE COIN(S) -----
  # We now run the IndData(s) though the makeCOIN function. IndMeta and AggMeta remain the same.

  if(is.null(use_year)){
    # No panel data. We send IndData to the function
    return(makeCOIN(IndData))
  } else {
    if(length(yrs)==1){
      # A single IndData has been created from a panel data set
      return(makeCOIN(IndData))
    } else {
      # Multiple IndDatas have been created from a panel data set
      COINlist <- lapply(IndDatas, makeCOIN)
      # make a wallet (a data frame of COINs)
      wallet <- tibble::tibble(Time = yrs, COIN = COINlist)
      # NOTE NEED TO CHECK METHOD PARAMS IN THIS CASE, SO THAT COINS IN WALLET CAN REGEN PROPERLY
      #class(wallet) <- "wallet"
      return(wallet)
    }
  }

}


#' Impute panel data
#'
#' Given a data frame of the `IndData` format, with a `Year` column, imputes any missing data using the latest available year.
#' This function is used inside [assemble()].
#'
#' This expects a data frame in the `IndData` format, i.e. it should at least have a `UnitCode` column, and a `Year` column,
#' as well as other columns that are to be imputed. It also presumes that there are multiple observations for each unit code,
#' i.e. one per year. It then searches for any missing values in the target year, and replaces them with the equivalent points
#' from previous years. It will replace using the most recently available point.
#'
#' @param use_year The year of data to extract and impute.
#' @param IndData A data frame of indicator data, containing a `Year` column and with multiple observations for each unit code.
#' @param impute_latest Logical: if `TRUE`, imputes missing data points using most recent value from previous years. If `FALSE`
#' (default) simply extracts the data frame as is.
#'
#' @examples
#' # artificial example using ASEM data
#' # We only have one year of data so we copy it and "pretend" that they are from different years
#' # First, introduce 3 NAs
#' dat2018 <- ASEMIndData
#' dat2018[2, 12] <- NA
#' dat2018[3, 13] <- NA
#' dat2018[4, 14] <- NA
#' # Now make copy, pretending it is the previous year
#' dat2017 <- ASEMIndData
#' dat2017$Year <- 2017
#' # This df still has one missing point
#' dat2017[4, 14] <- NA
#' # Finally we have a 2016 data frame where none of the previous points are missing
#' dat2016 <- ASEMIndData
#' dat2016$Year <- 2016
#' # We can now put them together
#' IndData <- rbind(dat2018, dat2017, dat2016)
#' # And extract the 2018 data, with missing data taken from previous years
#' Imp <- extractYear(2018, IndData, impute_latest = TRUE)
#' # View which points have been imputed and the years of data used
#' Imp$ImpTable
#'
#' @return A list containing:
#' * `.$IndDataImp`: An `IndData` format data frame from the specified year (`use_year`), with missing data imputed using previous years
#' (where possible).
#' * `.$DataYears`: A data frame in the same format as `IndData`, where each entry shows which year each data point came from.
#' Points where there was no missing data will have `use_year`, imputed points will have the corresponding year used to impute,
#' and any points in `.$IndDataImp` which are still `NA` will be be `NA`.
#' * `.$ImpTable`: A data frame where each row is a point that was successfully imputed. This is a filtered and arranged version
#' of `.$DataYears` that focuses only on the imputed points.
#' * `.$NImputed`: The number of imputed points.
#'
#' @seealso
#' * [assemble()] Assemble a COIN - this function optionally calls [extractYear()].
#' * [impute()] Impute data using other imputation options (not using panel data).
#'
#' @export

extractYear <- function(use_year, IndData, impute_latest = FALSE){

  # Some checks
  if(is.null(IndData$Year)){
    stop("You have specified to filter to a year but no 'Year' column is detected in IndData")
  }
  stopifnot(is.numeric(use_year),
            length(use_year)==1)
  if(!(use_year %in% IndData$Year)){
    stop("The year specified by use_year is not found in IndData$Year.")
  }

  # Now filter to year
  IndDataY <- IndData[IndData$Year == use_year, ]

  # See what years are in IndData
  yrs <- sort(unique(IndData$Year), decreasing = TRUE)

  if(impute_latest){

    # Impute using latest year

    if(length(yrs)==1){
      warning("Cannot impute by latest year because only on year of data available.")
    }

    olderyrs <- yrs[yrs < use_year]

    if(length(olderyrs) == 1){
      warning("Cannot impute by latest year because there is no year before the selected use_year.")
    }

    # I have to do this unit by unit... this is the safest way to deal with the possibility of
    # (a) different ordering of units
    # (b) subsets of units being available for different years
    # Since the each year of the data comes from the same table, column ordering is consistent so
    # I don't have to worry about that.

    # here I prep a data frame which will record the year used for each data point
    # we only make changes to this when a point is imputed
    DataYears <- IndDataY[!(colnames(IndDataY) %in% c("Year", "UnitName"))]
    DataYears[colnames(DataYears) != "UnitCode"] <- use_year

    for (ii in 1:nrow(IndDataY)){

      # get row
      irow <- IndDataY[ii, ]
      # if no NAs, go onto the next one
      if(all(!is.na(irow))){next}

      # get unit code
      unitcode <- irow$UnitCode

      # otherwise, we have to go year by year
      for(oldyr in olderyrs){

        # get row of same unit, for a previous year
        irowold <- IndData[(IndData$Year == oldyr) & (IndData$UnitCode == unitcode), ]
        # substitute in any missing values
        # first, get the equivalent entries of the old row (corresponding to NAs in new row)
        irowold_replace <- irowold[, as.logical(is.na(irow))]
        # and the names
        names_irowold <- names(irowold)[as.logical(is.na(irow))]
        # replace them into the new row
        irow[, as.logical(is.na(irow))] <- irowold_replace
        # find which indicators were imputed here
        ind_imp <- names_irowold[!as.logical(is.na(irowold_replace))]
        # record what happened in datayears
        DataYears[DataYears$UnitCode == unitcode, colnames(DataYears) %in% ind_imp] <- oldyr
        # check if we need to carry on
        if(all(!is.na(irow))){break}

      }

      # replace with imputed row
      IndDataY[ii, ] <- irow

    }

    # if there are still any NAs, we need to record this in DataYears
    NAcheck <- IndDataY[colnames(DataYears)]
    DataYears[is.na(NAcheck)] <- NA

    # count how many imputed
    nimputed <- sum(is.na(IndData[IndData$Year == use_year, ])) - sum(is.na(IndDataY))
    message(paste0("Number of imputed points = ", nimputed))

    # we also want just a table of imputed points
    ImpTable <- tidyr::pivot_longer(DataYears, !.data$UnitCode, names_to = "Variable", values_to = "YearUsed")
    ImpTable <- ImpTable[(ImpTable$YearUsed != use_year) & !is.na(ImpTable$YearUsed), ]


    # return imputed data
    return(list(IndDataImp = IndDataY,
         DataYears = DataYears,
         ImpTable = ImpTable,
         NImputed = nimputed
    ))

  } else {

    # if no imputation, return just filtered df. For consistency this is anyway wrapped in a list.
    return(list(IndDataImp = IndDataY
    ))

  }

}


#' Replace multiple values in a data frame
#'
#' Given a data frame (or vector), this function replaces values according to a look up table or dictionary. In COINr this may
#' be useful for exchanging categorical data with numeric scores, prior to assembly. Or for changing codes.
#'
#' The lookup data frame must not have any duplicated values in the `old` column. This function looks for exact matches of
#' elements of the `old` column and replaces them with the corresponding value in the `new` column. For each row of `lookup`,
#' the class of the old value must match the class of the new value. This is to keep classes of data frames columns consistent.
#' If you wish to replace with a different class, you should convert classes in your data frame before using this function.
#'
#' @param df A data frame or a vector
#' @param lookup A data frame with columns `old` (the values to be replaced) and `new` the values to replace with. See details.
#'
#' @examples
#' # replace sub-pillar codes in ASEM indicator metadata
#' codeswap <- data.frame(old = c("Conn", "Sust"), new = c("SI1", "SI2"))
#' # swap codes in both indmeta and aggmeta
#' replaceDF(ASEMIndMeta, codeswap)
#' replaceDF(ASEMAggMeta, codeswap)
#'
#' @return A data frame with replaced values
#'
#' @seealso
#' * [assemble()] Assemble a COIN - this function optionally calls [extractYear()].
#' * [rankDF()] Replace numeric columns of a data frame with ranks.
#' * [roundDF()] Replace numeric columns of a data frame with rounded values.
#' * [compareDF()] Detailed comparison of two similar data frames.
#'
#' @export

replaceDF <- function(df, lookup){

  # if a vector is input, convert to data frame
  vecflag <- FALSE
  if(is.vector(df)){
    vecflag <- TRUE
    df <- data.frame(v1 = df)
  }

  # checks
  stopifnot(is.data.frame(df),
            is.data.frame(lookup),
            !(is.null(lookup$old)),
            !(is.null(lookup$new)),
            anyDuplicated(lookup$old) == 0)

  # replace each item one at a time
  for(ii in 1:nrow(lookup)){

    # check that the class of the old/new pair is the same
    if(class(lookup$old[ii]) != class(lookup$new[ii]) ){
      stop(paste0("Class difference detected in row ", ii, " of lookup. Old class is ", class(lookup$old[ii]), " but new class is ", class(lookup$new[ii]), "."))
    }

    # replace value
    df[df == lookup$old[ii]] <- lookup$new[ii]
  }

  # if it was a vector, convert back
  if(vecflag){
    df <- unlist(df, use.names = FALSE)
  }

  df

}
