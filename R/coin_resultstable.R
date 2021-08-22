#' Results summary tables
#'
#' Generates fast results tables, either attached to the COIN or as a data frame.
#'
#' Although results are available in a COIN in `.$Data`, the format makes it difficult to quickly present results. This function
#' generates results tables that are suitable for immediate presentation, i.e. sorted by index or other indicators, and only including
#' relevant columns. Scores are also rounded by default, and there is the option to present scores or ranks.
#'
#' @param COIN The COIN object, or a data frame of indicator data
#' @param tab_type The type of table to generate. Either `"Summary"`, `"Aggregates"`, `"Full"`, or `"FullWithDenoms"`.
#' @param use Either `"scores"` (default) or `"ranks"`.
#' @param order_by A code of the indicator or aggregate to sort the table by. If not specified, defaults to the highest
#' aggregate level, i.e. the index in most cases.
#' @param nround The number of decimal places to round numerical values to. Defaults to 2.
#' @param out2 If `"df"`, outputs a data frame (tibble). Else if `"COIN"` attaches to `.$Results` in an updated COIN.
#'
#' @examples \dontrun{
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # results table of scores for index and aggregates (excluding indicator scores)
#' getResults(ASEM, tab_type = "Aggregates")
#' }
#'
#' @return Results table as data frame or updated COIN.
#'
#' @seealso
#' * [resultsDash()] Interactive results dashboard
#' * [coin2Excel()] Export results to Excel
#'
#' @export

getResults <- function(COIN, tab_type = "Summary", use = "scores", order_by = NULL,
                         nround = 2, out2 = "df"){

  # first, get the data
  df <- COIN$Data$Aggregated

  ## PREP ## ----------------------------------------------------

  if(is.null(df)){
    stop("Can't find data set. Have you aggregated your data?")
  }

  # get structure
  ag <- COIN$Input$AggMeta
  nlev <- max(ag$AgLevel)

  # rearrange structure so the highest level is at the top, then working down
  ag <- ag[order(-ag$AgLevel),]

  if(is.null(ag)){
    stop("Aggmeta not found.")
  }

  if(is.null(order_by)){
    sortcode <- ag$Code[ag$AgLevel == nlev]
  } else {
    sortcode <- order_by
  }

  ## BUILD TABLE ## ---------------------------------------------

  if(tab_type == "Summary"){

    df$Rank <- rank(-1*df[[sortcode]], na.last = "keep", ties.method = "min")

    # Just the indicator/index plus ranks
    tabout <- tibble::as_tibble(
      df[c("UnitCode", "UnitName", sortcode, "Rank")]
    )

  } else if (tab_type == "Aggregates"){

    df$Rank <- rank(-1*df[[sortcode]], na.last = "keep", ties.method = "min")

    # All the aggregate scores
    tabout <- tibble::as_tibble(
      df[c("UnitCode", "UnitName", "Rank", ag$Code)]
    )

  } else if (tab_type == "Full"){

    df$Rank <- rank(-1*df[[sortcode]], na.last = "keep", ties.method = "min")

    # Get all other codes (not names or aggs)
    othercodes <- setdiff(colnames(df), c("UnitCode", "UnitName", ag$Code, "Rank"))

    # All the aggregate scores, plus the remaining cols on the end
    tabout <- tibble::as_tibble(
      df[c("UnitCode", "UnitName", "Rank", ag$Code, othercodes)]
    )

  } else if (tab_type == "FullWithDenoms"){

    df$Rank <- rank(-1*df[[sortcode]], na.last = "keep", ties.method = "min")

    # Get all other codes (not names or aggs)
    othercodes <- setdiff(colnames(df), c("UnitCode", "UnitName", "Rank", ag$Code))

    # All the aggregate scores, plus the remaining cols on the end
    tabout <- tibble::as_tibble(
      df[c("UnitCode", "UnitName", "Rank", ag$Code, othercodes)]
    )

    if (!is.null(COIN$Input$Denominators)){

      # get denominators, discard extra cols
      denoms <- COIN$Input$Denominators %>%
        dplyr::select(.data$UnitCode, dplyr::starts_with("den"))

      # join denoms to main table
      tabout <- tabout %>%
        dplyr::left_join(denoms, by = "UnitCode")
    }

  }

  # Sorting
  tabout <- tabout[order(-tabout[[sortcode]]),]

  # Rounding
  tabout <- roundDF(tabout, nround)

  # Ranks
  if(use == "ranks"){
    tabout <- tabout[colnames(tabout) != "Rank"]
    tabout <- rankDF(tabout)
  }

  ## FINISH AND OUTPUT ## -------------------------------------------------

  if(out2 == "df"){

    return(tabout)

  } else if (out2 == "COIN"){

    if(use == "scores"){
      COIN$Results[[paste0(tab_type,"Scores")]] <- tabout
    } else if (use == "ranks"){
      COIN$Results[[paste0(tab_type,"Ranks")]] <- tabout
    }
    return(COIN)

  } else {
    stop("out2 not recognised!")
  }

}


#' Convert a data frame to ranks
#'
#' Replaces all numerical columns of a data frame with their ranks. Uses sport ranking, i.e. ties
#' share the highest rank place. Ignores non-numerical columns. See [rank()].
#'
#' @param df A data frame
#'
#' @examples \dontrun{
#' # some random data
#' df <- as.data.frame(matrix(runif(20), 5, 4))
#' # convert to ranks
#' rankDF(df)}
#'
#' @return Data frame with ranks
#'
#' @seealso
#' * [roundDF()] Round a data frame to a specified number of decimals.
#'
#' @export

rankDF <- function(df){

  df <- lapply(df, function(y) if(is.numeric(y)) rank(-1*y, na.last = "keep", ties.method = "min") else y) %>%
    data.frame()
  rownames(df) <- NULL
  df

}


#' Compare two data frames
#'
#' A custom function for comparing two data frames of indicator data, to see whether they match up, at a specified number of
#' significant figures.
#'
#' This function compares numerical and non-numerical columns to see if they match. Rows and columns can be in any order. The function
#' performs the following checks:
#'
#'   * Checks that the two data frames are the same size
#'   * Checks that column names are the same, and that the matching column has the same entries
#'   * Checks column by column that the elements are the same, after sorting according to the matching column
#'
#' It then summarises for each column whether there are any differences, and also what the differences are, if any.
#'
#' This is intended to cross-check results. For example, if you run something in COINr and want to check indicator results against
#' external calculations.
#'
#' @param df1 A data frame
#' @param df2 Another data frame
#' @param matchcol A common column name that is used to match row order. E.g. this might be `UnitCode`.
#' @param sigfigs The number of significant figures to use for matching numerical columns
#'
#' @examples \dontrun{
#' # take a sample of indicator data (including the UnitCode column)
#' data1 <- ASEMIndData[c(2,12:15)]
#' # copy the data
#' data2 <- data1
#' # make a change: replace one value in data2 by NA
#' data2[1,2] <- NA
#' # compare data frames
#' compareDF(data1, data2, matchcol = "UnitCode")}
#'
#' @return A list with comparison results
#'
#' @export

compareDF <- function(df1, df2, matchcol, sigfigs = 5){

  # general checks
  stopifnot(is.data.frame(df1),
            is.data.frame(df2),
            matchcol %in% colnames(df1),
            matchcol %in% colnames(df2))

  # check sizes
  if(nrow(df1)!=nrow(df2)){
    sameanswer <- FALSE
    details("Different number of rows.")
  }
  if(ncol(df1)!=ncol(df2)){
    sameanswer <- FALSE
    details("Different number of columns.")
  }

  # check column names
  if(!setequal(colnames(df1), colnames(df2))){
    sameanswer <- FALSE
    details("Column names not the same.")
  }
  # check row names same in matchcol
  if(!setequal(df1[matchcol], df2[matchcol])){
    sameanswer <- FALSE
    details("Elements in matchcol are not the same.")
  }

  # From this point we should be fairly sure that the two dfs are the same size and contain the same cols and rows

  # match col order
  df2 <- df2[colnames(df1)]
  # match row order
  df2 <- df2[match(df1[[matchcol]], df2[[matchcol]]),]

  # Now the dfs should be also in the same order of rows and cols. Remains to check the values.
  details = data.frame(Column = colnames(df1),
                       TheSame = NA,
                       Comment = NA,
                       NDifferent = NA)

  diffs <- vector(mode = "list", length = 0)

  # now loop over columns
  for(ii in 1:length(colnames(df1))){

    # get cols
    x <- df1[[ii]]
    y <- df2[[ii]]

    # class check
    if(class(x)!=class(y)){
      details$TheSame[[ii]] <- FALSE
      details$Comment[[ii]] <- "Class difference"
      next
    }

    # now check depending on type
    if(is.numeric(x)){

      if(identical(signif(x, sigfigs), signif(y, sigfigs))){
        details$TheSame[[ii]] <- TRUE
        details$Comment[[ii]] <- paste0("Numerical and identical to ", sigfigs, " sf.")
        details$NDifferent[[ii]] <- 0
      } else {
        details$TheSame[[ii]] <- FALSE
        details$Comment[[ii]] <- paste0("Numerical and different at ", sigfigs, " sf.")
        dfdiffs <- data.frame(MatchCol = df1[[matchcol]], df1 = x, df2 = y)
        colnames(dfdiffs)[1] <- matchcol
        diffrows <- signif(x, sigfigs) != signif(y, sigfigs)
        diffrows[is.na(diffrows)] <- TRUE
        dfdiffs <- dfdiffs[diffrows, ]
        diffs[[colnames(df1)[ii]]] <- dfdiffs
        details$NDifferent[[ii]] <- nrow(dfdiffs)
      }

    } else {

      if(identical(x, y)){
        details$TheSame[[ii]] <- TRUE
        details$Comment[[ii]] <- paste0("Non-numerical and identical")
        details$NDifferent[[ii]] <- 0
      } else {
        details$TheSame[[ii]] <- FALSE
        details$Comment[[ii]] <- paste0("Non-numerical and different")
        dfdiffs <- data.frame(MatchCol = df1[[matchcol]], df1 = x, df2 = y)
        colnames(dfdiffs)[1] <- matchcol
        dfdiffs <- dfdiffs[x != y, ]
        diffs[[colnames(df1)[ii]]] <- dfdiffs
        details$NDifferent[[ii]] <- nrow(dfdiffs)
      }
    }

  }

  return(list(Same = all(details$TheSame),
              Details = details,
              Differences = diffs))

}


#' Generate unit report
#'
#' Generates a scorecard for a given unit using an R Markdown template.
#'
#' Most likely you will want to customise the template which can be found in the COINr installed package directory under `/UnitReport`.
#' Currently, a few examples are given, such as some charts and basic summary statistics.
#'
#' This function will render the unit report to either pdf, html or word doc. As mentioned below, if you have HTML widgets
#' such as interactive plotly plots, or COINr `iplot()` functions, you will need to install the webshot package to be able to
#' render to pdf or word formats.
#'
#' To customise the template, copy the `.rmd` template found in `/UnitReport` and alter it, then point the `rmd_template` argument to
#' your new template.
#'
#' Note that this function is particularly useful for generating a large number of reports, e.g. we can generate reports for
#' all units at once using a `for` loop, [purrr::map] or [apply()] or similar.
#'
#' @param COIN A COIN
#' @param usel A selected unit code, or a character vector of unit codes (for multiple reports).
#' @param out_type A string specifying the output type. Can be either `".docx"` (Word), `".pdf"` or `".html"`. IMPORTANT: if the
#' template includes interactive plots (e.g. the `iplot()` functions from COINr), writing to `.docx` or `.pdf` will not work
#' *unless* you have installed the webshot package. To do this, run:
#' `install.packages("webshot")`
#' `webshot::install_phantomjs()`
#' @param outdir Character string specifying the output directory (defaults to current working directory).
#' @param rmd_template A character string specifying the full file path to an R Markdown template which is used to generate the report. If this is not specified,
#' defaults to COINr's inbuilt template example.
#'
#' @examples \dontrun{
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # generate a unit report for NZ
#' getUnitReport(ASEM, usel = "NZL", out_type = ".html")}
#'
#' @return Markdown document rendered to HTML, pdf or Word.
#'
#' @export

getUnitReport <- function(COIN, usel, out_type = ".html", outdir = NULL, rmd_template = NULL){

  # specify output type
  if(out_type == ".pdf"){
    output_format <- "pdf_document"
  } else if (out_type == ".docx"){
    output_format <- "word_document"
  } else if (out_type == ".html"){
    output_format <- "html_document"
  }

  # check for webshot package
  if ((out_type == ".pdf") | (out_type == ".docx") ){
    check_webshot <- system.file(package = "webshot")
    if(!nzchar(check_webshot)){
      warning("If your R Markdown template has html widgets, interactive plots (e.g. plotly or COINr iplot functions)
pdf or docx will not compile properly. Consider installing the webshot package to remedy this. See the
COINr documentation and the help file of this function for more details.")
    }
  }

  # set output directory
  if(is.null(outdir)){
    outdir <- getwd()
  }

  # get rmd template - default to the template found in \inst
  if(is.null(rmd_template)){
    rmd_template <- system.file("UnitReport", "unit_report_source.Rmd", package = "COINr")
  }

  # render the document
  for (uselii in usel){
    rmarkdown::render(rmd_template,
                      params = list(COIN = COIN, usel = uselii),
                      output_file = paste0(outdir, "/", uselii, '_report', out_type),
                      output_format = output_format)
  }

}


#' Generate unit summary table
#'
#' Generates a summary table for a single unit. This is mostly useful in unit reports.
#'
#' This returns the scores and ranks for each indicator/aggregate as specified in `aglevs`. It orders the table so that
#' the highest aggregation levels are first. This means that if the index level is included, it will be first.
#'
#' @param COIN A COIN
#' @param usel A selected unit code
#' @param aglevs The aggregation levels to display results from.
#'
#' @examples \dontrun{
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # generate unit summary for NZ - index and sub-indexes only
#' getUnitSummary(ASEM, usel ="NZL", aglevs = c(4,3))}
#'
#' @seealso
#' * [getUnitReport()] Automatic unit report as html, pdf or Word
#' * [getStrengthNWeak()] Top N-ranking indicators for a given unit
#'
#' @return A data frame summary table.
#'
#' @export

getUnitSummary <- function(COIN, usel, aglevs){

  # get rank and score tables
  if(is.null(COIN$Results$FullRanks)){
    COIN <- getResults(COIN, tab_type = "Full", use = "ranks", out2 = "COIN")
  }
  if(is.null(COIN$Results$FullScores)){
    COIN <- getResults(COIN, tab_type = "Full", use = "scores", out2 = "COIN")
  }
  rnks <- COIN$Results$FullRanks
  scrs <- COIN$Results$FullScores

  # get agg codes
  aggs <- COIN$Input$AggMeta[order(-COIN$Input$AggMeta$AgLevel),]
  agcodes <- aggs$Code[aggs$AgLevel %in% aglevs]
  agnames <- aggs$Name[aggs$AgLevel %in% aglevs]

  scrs1 <- scrs[agcodes]
  rnks1 <- rnks[agcodes]

  tabout <- data.frame(
    Indicator = agnames,
    Score = scrs1[scrs$UnitCode == usel, ]  %>% as.numeric(),
    Rank = rnks1[rnks$UnitCode == usel, ] %>% as.numeric()
  )

  return(tabout)

}


#' Generate strengths and weaknesses for a specified unit
#'
#' Generates a table of strengths and weaknesses for a selected unit, based on ranks.
#'
#' This currently only works at the indicator level. Indicators with NA values for the selected unit are ignored.
#' Strengths and weaknesses mean the top N-ranked indicators for the selected unit. Effectively, this takes the rank that the
#' selected unit has in each indicator, sorts the ranks, and takes the top N highest and lowest.
#'
#' NOTE: this function currently requires data to be aggregated. This restriction may be relaxed at some point.
#'
#' @param COIN A COIN
#' @param usel A selected unit code
#' @param topN The top N indicators to report
#' @param bottomN The bottom N indicators to report
#' @param withcodes If `TRUE` (default), also includes a column of indicator codes. Setting to `FALSE` may be more useful
#' in generating reports, where codes are not helpful.
#'
#' @examples \dontrun{
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # generate top 5 strengths and weaknesses for GBR
#' getStrengthNWeak(ASEM, usel = "GBR")}
#'
#' @seealso
#' * [getUnitReport()] Automatic unit report as html, pdf or Word
#' * [getUnitSummary()] Summary of scores for a given unit
#'
#' @return A list containing a data frame of strengths, and a data frame of weaknesses.
#'
#' @export

getStrengthNWeak <- function(COIN, usel = NULL, topN = 5, bottomN = 5, withcodes = TRUE){

  # first, get a rank table
  if(is.null(COIN$Results$FullRanks)){
    COIN <- getResults(COIN, tab_type = "Full", use = "ranks", out2 = "COIN")
  }
  rnks <- COIN$Results$FullRanks

  # indicator codes
  IndCodes <- COIN$Input$IndMeta$IndCode

  # isolate the row and indicator cols
  rnks_usel <- rnks[rnks$UnitCode == usel, IndCodes]

  # remove NAs
  rnks_usel <- rnks_usel[,!is.na(rnks_usel[1,])]

  # sort by row values
  rnks_usel <- rnks_usel[ ,order(as.numeric(rnks_usel[1,]))]

  # get strengths and weaknesses
  Scodes <- colnames(rnks_usel)[1:topN]
  Wcodes <- colnames(rnks_usel)[ (ncol(rnks_usel) - bottomN + 1) : ncol(rnks_usel) ]

  # make tables
  strengths <- data.frame(
    Code = Scodes,
    Name = COIN$Input$IndMeta$IndName[match(Scodes, COIN$Input$IndMeta$IndCode)],
    Rank = as.numeric(rnks_usel[Scodes]),
    Value = signif(as.numeric(COIN$Data$Raw[COIN$Data$Raw$UnitCode == usel ,Scodes]),3)
  )
  weaks <- data.frame(
    Code = Wcodes,
    Name = COIN$Input$IndMeta$IndName[match(Wcodes, COIN$Input$IndMeta$IndCode)],
    Rank = as.numeric(rnks_usel[Wcodes]),
    Value = signif(as.numeric(COIN$Data$Raw[COIN$Data$Raw$UnitCode == usel ,Wcodes]),3)
  )

  # remove indicator codes if needed
  if(!withcodes){
    strengths <- strengths[-1]
    weaks <- weaks[-1]
  }

  # output
  return(list(
    Strengths = strengths,
    Weaknesses = weaks
  ))


}
