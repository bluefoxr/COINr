#' Results summary tables
#'
#' Generates fast results tables, either attached to the COIN or as a data frame.
#'
#' @param COIN The COIN object, or a data frame of indicator data
#' @param tab_type The type of table to generate. Either "Summary", "Aggregates", "Full", or "FullWithDenoms".
#' @param use Either "scores" (default) or "ranks".
#' @param order_by A code of the indicator or aggregate to sort the table by. If not specified, defaults to the highest
#' aggregate level, i.e. the index in most cases.
#' @param nround The number of decimal places to round numerical values to. Defaults to 2.
#' @param out2 If "df", outputs a data frame (tibble). Else if "COIN" attaches to .$Results in an updated COIN.
#'
#' @examples \dontrun{"test")}
#'
#' @return Results table as data frame or updated COIN.
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
#' share the highest rank place.
#'
#' @param df A data frame
#'
#' @examples \dontrun{"test")}
#'
#' @return Data frame with ranks
#'
#' @export

rankDF <- function(df){

  df <- lapply(df, function(y) if(is.numeric(y)) rank(-1*y, na.last = "keep", ties.method = "min") else y) %>%
    data.frame()
  rownames(df) <- NULL
  df

}
