#' Results summary tables
#'
#' Generates fast results tables, either attached to the COIN or as a data frame.
#'
#' Although results are available in a COIN in `.$Data`, the format makes it difficult to quickly present results. This function
#' generates results tables that are suitable for immediate presentation, i.e. sorted by index or other indicators, and only including
#' relevant columns. Scores are also rounded by default, and there is the option to present scores or ranks.
#'
#' @param COIN The COIN object, or a data frame of indicator data
#' @param tab_type The type of table to generate. Either `"Summ"` (a single indicator plus rank), `"Aggs"` (all aggregated
#' scores/ranks above indicator level), or `"Full"` (all scores/ranks plus all group, denominator columns).
#' @param use Either `"scores"` (default), `"ranks"`, or `"groupranks"`. For the latter, `use_group` must be specified.
#' @param order_by A code of the indicator or aggregate to sort the table by. If not specified, defaults to the highest
#' aggregate level, i.e. the index in most cases. If `use_group` is specified, rows will also be sorted by the specified group.
#' @param nround The number of decimal places to round numerical values to. Defaults to 2.
#' @param use_group An optional grouping variable. If specified, the results table includes this group column,
#' and if `use = "groupranks"`, ranks will be returned with respect to the groups in this column.
#' @param out2 If `"df"`, outputs a data frame (tibble). Else if `"COIN"` attaches to `.$Results` in an updated COIN.
#'
#' @examples
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # results table of scores for index and aggregates (excluding indicator scores)
#' dfResults <- getResults(ASEM, tab_type = "Aggregates", out2 = "df")
#'
#' @return If `out2 = "df"`, the results table is returned as a data frame. If `out2 = "COIN"`, this function returns an updated
#' COIN with the results table attached to `.$Results`.
#'
#' @seealso
#' * [resultsDash()] Interactive results dashboard
#' * [coin2Excel()] Export results to Excel
#'
#' @export

getResults <- function(COIN, tab_type = "Summ", use = "scores", order_by = NULL,
                         nround = 2, use_group = NULL, out2 = "df"){

  # first, get the data
  df <- COIN$Data$Aggregated

  ## PREP ## ----------------------------------------------------

  if(is.null(df)){
    stop("Can't find aggregated data set. Have you aggregated your data?")
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

  if(tab_type %in% c("Summ", "Summary")){

    df$Rank <- rank(-1*df[[sortcode]], na.last = "keep", ties.method = "min")

    # Just the indicator/index plus ranks
    tabout <- tibble::as_tibble(
      df[c("UnitCode", "UnitName", use_group, sortcode, "Rank")]
    )

  } else if (tab_type %in% c("Aggs", "Aggregates")){

    df$Rank <- rank(-1*df[[sortcode]], na.last = "keep", ties.method = "min")

    # All the aggregate scores
    tabout <- tibble::as_tibble(
      df[c("UnitCode", "UnitName", use_group, "Rank", ag$Code)]
    )

  } else if (tab_type %in% c("Full", "FullWithDenoms")){

    df$Rank <- rank(-1*df[[sortcode]], na.last = "keep", ties.method = "min")

    # Get all other codes (not names or aggs)
    othercodes <- setdiff(colnames(df), c("UnitCode", "UnitName", use_group, "Rank", ag$Code))

    # All the aggregate scores, plus the remaining cols on the end
    tabout <- tibble::as_tibble(
      df[c("UnitCode", "UnitName", use_group, "Rank", ag$Code, othercodes)]
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
  } else if (use =="groupranks"){
    if(is.null(use_group)){
      stop("If groupranks is specified, you need to also specify use_group.")
    }
    tabout <- tabout[colnames(tabout) != "Rank"]
    tabout <- rankDF(tabout, use_group = use_group)
    # sort by group
    tabout <- tabout[order(tabout[[use_group]]),]
  }

  ## FINISH AND OUTPUT ## -------------------------------------------------

  if(out2 == "df"){

    return(tabout)

  } else if (out2 == "COIN"){

    if(use == "scores"){
      COIN$Results[[paste0(tab_type,"Score")]] <- tabout
    } else if (use == "ranks"){
      COIN$Results[[paste0(tab_type,"Rank")]] <- tabout
    } else if (use == "groupranks"){
      COIN$Results[[paste0(tab_type,"GrpRnk", use_group)]] <- tabout
    }
    return(COIN)

  } else {
    stop("out2 not recognised!")
  }

}


#' Convert a data frame to ranks
#'
#' Replaces all numerical columns of a data frame with their ranks. Uses sport ranking, i.e. ties
#' share the highest rank place. Ignores non-numerical columns. See [rank()]. Optionally, returns in-group ranks
#' using a specified grouping column.
#'
#' @param df A data frame
#' @param use_group An optional column of df (specified as a string) to use as a grouping variable. If specified, returns ranks
#' inside each group present in this column.
#'
#' @examples
#' # some random data, with a column of characters
#' df <- data.frame(RName = c("A", "B", "C"),
#' Score1 = runif(3), Score2 = runif(3))
#' # convert to ranks
#' rankDF(df)
#' # grouped ranking - use some example data
#' df1 <- ASEMIndData[c("UnitCode", "Group_GDP", "Goods", "LPI")]
#' rankDF(df1, use_group = "Group_GDP")#'
#'
#' @return A data frame equal to the data frame that was input, but with any numerical columns replaced with ranks.
#'
#' @seealso
#' * [roundDF()] Round a data frame to a specified number of decimals.
#'
#' @export

rankDF <- function(df, use_group = NULL){

  if(is.null(use_group)){
    df <- lapply(df, function(y) if(is.numeric(y)) rank(-1*y, na.last = "keep", ties.method = "min") else y) |>
      data.frame()
  } else {
    stopifnot(use_group %in% colnames(df))
    # get groups
    grps <- df[[use_group]] |> unlist() |> unique()
    # I have to work over groups. To me the clearest way of doing this is with a for loop (sorry)
    dfold <- df
    for(grp in grps){
      # get current group rows
      grprows <- df[[use_group]] == grp
      # exclude any NAs
      grprows[is.na(grprows)] <- FALSE
      # now work over all columns, but just for the current group rows
      df[grprows,] <- lapply(dfold[grprows,], function(y) if(is.numeric(y)) rank(-1*y, na.last = "keep", ties.method = "min") else y) |>
        data.frame()
    }

    # now I have to fill in rows that have NA group values, with NAs
    if(any(is.na(df[[use_group]]))){
      df[is.na(df[[use_group]]),] <- lapply(df[is.na(df[[use_group]]),], function(y) if(is.numeric(y)) NA else y) |>
        data.frame()
    }
  }

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
#' @examples
#' # take a sample of indicator data (including the UnitCode column)
#' data1 <- ASEMIndData[c(2,12:15)]
#' # copy the data
#' data2 <- data1
#' # make a change: replace one value in data2 by NA
#' data2[1,2] <- NA
#' # compare data frames
#' compareDF(data1, data2, matchcol = "UnitCode")
#'
#' @return A list with comparison results. List contains:
#' * `.$Same`: overall summary: if `TRUE` the data frames are the same according to the rules specified, otherwise `FALSE`.
#' * `.$Details`: details of each column as a data frame. Each row summarises a column of the data frame, saying whether
#' the column is the same as its equivalent, and the number of differences, if any. In case the two data frames have differing
#' numbers of columns and rows, or have differing column names or entries in `matchcol`, `.$Details` will simply contain a
#' message to this effect.
#' * `.$Differences`: a list with one entry for every column which contains different entries. Differences are summarised as
#' a data frame with one row for each difference, reporting the value from `df1` and its equivalent from `df2`.
#'
#' @export

compareDF <- function(df1, df2, matchcol, sigfigs = 5){

  # general checks
  stopifnot(is.data.frame(df1),
            is.data.frame(df2),
            matchcol %in% colnames(df1),
            matchcol %in% colnames(df2))

  # check for duplicates in matchcol
  if( (anyDuplicated(df1[[matchcol]]) > 0) | (anyDuplicated(df2[[matchcol]]) > 0) ){
    stop("Duplicates found in matchcol. This function requires unique entries in matchcol to make a comparison.")
  }

  # this is default but will change if anything is found to be different
  sameanswer <- TRUE

  # check sizes
  if(nrow(df1)!=nrow(df2)){
    sameanswer <- FALSE
    details <- "Different number of rows."
  }
  if(ncol(df1)!=ncol(df2)){
    sameanswer <- FALSE
    details <- "Different number of columns."
  }

  # check column names
  if(!setequal(colnames(df1), colnames(df2))){
    sameanswer <- FALSE
    details <- "Column names not the same."
  }
  # check row names same in matchcol
  if(!setequal(df1[matchcol], df2[matchcol])){
    sameanswer <- FALSE
    details <- "Elements in matchcol are not the same."
  }

  if(!sameanswer){
    # exiting because dfs have different sizes or column/row names
    return(list(Same = sameanswer,
                Details = details))
  } else {

    # From this point we should be fairly sure that the two dfs are the same size and contain the same cols and rows

    # match col order
    df2 <- df2[colnames(df1)]

    # match row order
    df2 <- df2[match(df1[[matchcol]], df2[[matchcol]]),]

    # Now the dfs should be also in the same order of rows and cols. Remains to check the values.
    details <- data.frame(Column = colnames(df1),
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
#' @importFrom rmarkdown render pandoc_available
#'
#' @examples
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # Generate a unit report for NZ
#' # This is written to the temporary directory to avoid polluting other directories
#' # during automated testing.
#' # It will be deleted at the end of the R session.
#' # Normally you would set the directory to somewhere else to save the resulting files
#' getUnitReport(ASEM, usel = "NZL", out_type = ".html", outdir = tempdir())
#' # You can find this file in the temporary directory:
#' print(tempdir())
#' # We will now delete the file to keep things tidy in testing
#' unlink(paste0(tempdir(),"\\NZL_report.html"))
#'
#' @return Markdown document rendered to HTML, pdf or Word. This function requires Pandoc to be installed. If Pandoc is not found,
#' then it returns a warning and a printed message (string).
#'
#' @export

getUnitReport <- function(COIN, usel, out_type = ".html", outdir = NULL, rmd_template = NULL){

  # first check if Pandoc exists and is accessible. If not, fail without error because
  # otherwise this messes up CRAN tests. It seems that one of the distributions that CRAN uses
  # to test packages does not have Pandoc installed.
  if(!(rmarkdown::pandoc_available("1.12.3"))){
    warning("Pandoc version >= 1.12.3 is required to run this function (knit R markdown documents).")
    return("Function exited because Pandoc not available.")
  }

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
                      output_format = output_format, intermediates_dir = outdir,
                      knit_root_dir = outdir, clean = TRUE)
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
#' @examples
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # generate unit summary for NZ - index and sub-indexes only
#' getUnitSummary(ASEM, usel ="NZL", aglevs = c(4,3))
#'
#' @seealso
#' * [getUnitReport()] Automatic unit report as html, pdf or Word
#' * [getStrengthNWeak()] Top N-ranking indicators for a given unit
#'
#' @return A summary table as a data frame, containing scores and ranks for specified indicators/aggregates.
#'
#' @export

getUnitSummary <- function(COIN, usel, aglevs){

  # get rank and score tables
  if(is.null(COIN$Results$FullRank)){
    COIN <- getResults(COIN, tab_type = "Full", use = "ranks", out2 = "COIN")
  }
  if(is.null(COIN$Results$FullScore)){
    COIN <- getResults(COIN, tab_type = "Full", use = "scores", out2 = "COIN")
  }

  rnks <- COIN$Results$FullRank
  scrs <- COIN$Results$FullScore

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
#' Generates a table of strengths and weaknesses for a selected unit, based on ranks, or ranks within
#' a specified grouping variable.
#'
#' This currently only works at the indicator level. Indicators with NA values for the selected unit are ignored.
#' Strengths and weaknesses mean the top N-ranked indicators for the selected unit. Effectively, this takes the rank that the
#' selected unit has in each indicator, sorts the ranks, and takes the top N highest and lowest.
#'
#' This function must be used with a little care: indicators should be adjusted for their directions before use,
#' otherwise a weakness might be counted as a strength, and vice versa. Use the `adjust_direction` parameter
#' to help here.
#'
#' A further useful parameter is `unq_discard`, which also filters out any indicators with a low number of
#' unique values, based on a specified threshold. Also `min_discard` which filters out any indicators which
#' have the minimum rank.
#'
#' The best way to use this function is to play around with the settings a little bit. The reason being that
#' in practice, indicators have very different distributions and these can sometimes lead to unexpected
#' outcomes. An example is if you have an indicator with 50% zero values, and the rest non-zero (but unique).
#' Using the sport ranking system, all units with zero values will receive a rank which is equal to the number
#' of units divided by two. This then might be counted as a "strength" for some units with overall low scores.
#' But a zero value can hardly be called a strength. This is where the `min_discard` function can help out.
#'
#' Problems such as these mainly arise when e.g. generating a large number of country profiles.
#'
#' @param COIN A COIN
#' @param dset The data set to extract indicator data from, to use as strengths and weaknesses.
#' @param usel A selected unit code
#' @param topN The top N indicators to report
#' @param bottomN The bottom N indicators to report
#' @param withcodes If `TRUE` (default), also includes a column of indicator codes. Setting to `FALSE` may be more useful
#' in generating reports, where codes are not helpful.
#' @param use_group An optional grouping variable (named column of `.Data$Aggregated`) to use for reporting
#' in-group ranks. Specifying this will report the ranks of the selected unit within the group of `use_group`
#' to which it belongs.
#' @param unq_discard Optional parameter for handling discrete indicators. Some indicators may be binary
#' variables of the type "yes = 1", "no = 0". These may be picked up as strengths or weaknesses, when they
#' may not be wanted to be highlighted, since e.g. maybe half of units will have a zero or a one. This argument
#' takes a number between 0 and 1 specifying a unique value threshold for ignoring indicators as strengths. E.g.
#' setting `prc_unq_discard = 0.2` will ensure that only indicators with at least 20% unique values will be
#' highlighted as strengths or weaknesses. Set to `NULL` to disable (default).
#' @param min_discard If `TRUE` (default), discards any strengths which correspond to the minimum rank for the given
#' indicator. See details.
#' @param report_level Optional aggregation level to report parent codes from. For example, setting
#' `report_level = 2` will add a column to the strengths and weaknesses tables which reports the aggregation
#' group from level 2, to which each reported indicator belongs. Set to `NULL` to disable (default).
#' @param with_units If `TRUE` (default), includes indicator units in output tables.
#' @param adjust_direction If `TRUE`, will adjust directions of indicators according to the "Direction" column
#' of `IndMeta`. By default, this is `TRUE` *if* `dset = "Raw"`, and `FALSE` otherwise.
#'
#' @examples
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # generate top 5 strengths and weaknesses for GBR
#' getStrengthNWeak(ASEM, usel = "GBR")
#'
#' @seealso
#' * [getUnitReport()] Automatic unit report as html, pdf or Word
#' * [getUnitSummary()] Summary of scores for a given unit
#'
#' @return A list containing a data frame `.$Strengths`, and a data frame `.$Weaknesses`.
#' Each data frame has columns with indicator code, name, rank and value (for the selected unit).
#'
#' @export

getStrengthNWeak <- function(COIN, dset = NULL, usel = NULL, topN = 5, bottomN = 5, withcodes = TRUE,
                             use_group = NULL, unq_discard = NULL, min_discard = TRUE, report_level = NULL,
                             with_units = TRUE, adjust_direction = NULL){

  # indicator codes
  IndCodes <- COIN$Input$IndMeta$IndCode
  # scores
  if(is.null(dset)) dset <- "Raw"
  data_scrs <- COIN$Data[[dset]]
  # ranks
  # first, we have to adjust for direction
  if(is.null(adjust_direction)){
    if(dset == "Raw"){
      adjust_direction <- TRUE
    } else {
      adjust_direction <- FALSE
    }
  }
  # make a copy to adjust by direction
  data_scrs1 <- data_scrs
  if(adjust_direction){
    data_scrs1[IndCodes] <- data_scrs1[IndCodes]

    data_scrs1[IndCodes] <- purrr::map2_df(data_scrs1[IndCodes],
                                           COIN$Input$IndMeta$Direction, ~ .x*.y)
  }

  data_rnks <- rankDF(data_scrs1, use_group = use_group)

  # unique value filtering
  if(!is.null(unq_discard)){
    # find fraction of unique vals for each indicator
    frc_unique <- apply(data_scrs[IndCodes], MARGIN = 2,
                        function(x){
                          length(unique(x))/length(x)
                        })
    # filter indicator codes to only the ones with frac unique above thresh
    IndCodes <- IndCodes[frc_unique > unq_discard]
  }

  # isolate the row and indicator cols
  rnks_usel <- data_rnks[data_rnks$UnitCode == usel, IndCodes]

  # remove NAs
  rnks_usel <- rnks_usel[,!is.na(as.numeric(rnks_usel))]

  # Also need to (optionally) remove minimum rank entries
  # (by min I mean MAX, i.e. min SCORE)
  if(min_discard){
    rnks_min <- lapply(data_rnks[colnames(rnks_usel)], max, na.rm = T) |> as.data.frame()
    rnks_usel <- rnks_usel[,!(rnks_usel == rnks_min)]
  }

  # sort by row values
  rnks_usel <- rnks_usel[ ,order(as.numeric(rnks_usel[1,]))]

  # get strengths and weaknesses
  Scodes <- colnames(rnks_usel)[1:topN]
  Wcodes <- colnames(rnks_usel)[ (ncol(rnks_usel) - bottomN + 1) : ncol(rnks_usel) ]

  # find agg level column of interest
  if(is.null(report_level)){
    report_level <- 2
  }
  agcols <- colnames(COIN$Input$IndMeta)[startsWith(colnames(COIN$Input$IndMeta), "Agg")]
  agcolname <- agcols[report_level - 1]

  # make tables
  strengths <- data.frame(
    Code = Scodes,
    Name = COIN$Input$IndMeta$IndName[match(Scodes, COIN$Input$IndMeta$IndCode)],
    Dimension = COIN$Input$IndMeta[[agcolname]][match(Scodes, COIN$Input$IndMeta$IndCode)],
    Rank = as.numeric(rnks_usel[Scodes]),
    Value = signif(as.numeric(data_scrs[data_scrs$UnitCode == usel ,Scodes]),3)
  )

  weaks <- data.frame(
    Code = Wcodes,
    Name = COIN$Input$IndMeta$IndName[match(Wcodes, COIN$Input$IndMeta$IndCode)],
    Dimension = COIN$Input$IndMeta[[agcolname]][match(Wcodes, COIN$Input$IndMeta$IndCode)],
    Rank = as.numeric(rnks_usel[Wcodes]),
    Value = signif(as.numeric(data_scrs[data_scrs$UnitCode == usel ,Wcodes]),3)
  )

  # units
  # if units col exists and requested
  if(with_units & !is.null(COIN$Input$IndMeta$IndUnit)){
    strengths$Unit <- COIN$Input$IndMeta$IndUnit[match(Scodes, COIN$Input$IndMeta$IndCode)]
    weaks$Unit <- COIN$Input$IndMeta$IndUnit[match(Wcodes, COIN$Input$IndMeta$IndCode)]
  }

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
