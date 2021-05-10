#' Normalise indicator data sets
#'
#' A dataset of indicators is normalised using one of several methods. This function also supports custom normalisation.
#'
#' @param COIN Either the COIN object, or a data frame of indicator data
#' @param ntype The type of normalisation method. Either "minmax", "zscore", "scaled", "rank", "borda", "prank", "fracmax", "dist2targ", "dist2ref", "dist2max", "custom" or "none".
#' @param npara Supporting object for ntype.
#' @param dset The data set to normalise
#' @param directions A vector specifying the direction assigned to each indicator.
#' Needs to be the same length as the number of indicators, or the number of indicators in icodes, if specified.
#' @param individual A list of named lists specifiying individual normalisation to apply to specific indicators. Should be structured as follows:
#' The name of each sublist should be the indicator code. The the list elements are:
#' .$ntype is the type of normalisation to apply
#' .$npara is a corresponding object or parameters that are used by ntype
#' @param indiv_only Logical: if FALSE (default), indicators not specified in individual are subjected to default normalisation. Otherwise if TRUE they are not normalised.
#' @param out2 Where to output the results. If "COIN" (default for COIN input), appends to updated COIN,
#' otherwise if "df" outputs to data frame.
#'
#' @importFrom purrr "map2"
#' @importFrom purrr "modify"
#'
#' @examples \dontrun{df_norm <- normalise(COIN, ntype="minmax", npara = c(0,1))}
#'
#' @return An updated COIN object with .$Data$Normalised added.
#'
#' @export

normalise <- function(COIN, ntype="minmax", npara = NULL,
                      dset = "Raw", directions = NULL, individual = NULL,
                      indiv_only = FALSE, out2 = NULL){

  # First. check to see what kind of input we have.
  out <- getIn(COIN, dset = dset)
  ind_data_only <- out$ind_data_only
  IndCodes <- out$IndCodes

  if(out$otype == "COINobj"){
    # Write inputs to Method
    COIN$Method$normalise$ntype <- ntype
    COIN$Method$normalise$npara <- npara
    COIN$Method$normalise$dset <- dset
    COIN$Method$normalise$directions <- directions
    COIN$Method$normalise$indiv_only <- indiv_only
    COIN$Method$normalise$individual <- individual
  }

  if (is.null(directions) & out$otype == "COINobj"){ # if no directions are explicitly specified, but COINobj input

    if (exists("Direction", COIN$Input$IndMeta)){ # check if available in COIN obj
      directions <- COIN$Input$IndMeta$Direction
    } else { # if not, just set everything positive
      directions <- rep(1,length(IndCodes))
      warning("No indicator directions found in COIN object. Using positive directions for all indicators.")
    }

  } else if (is.null(directions) & out$otype == "df"){

    directions <- rep(1,length(IndCodes))
    warning("No indicator directions specified. Using positive directions for all indicators.")

  } # otherwise, directions will be taken from the function input.

  # nifty map2 implementation here. Multiply each column of ind_data_only by corresponding column of directions
  # This def works with minmax, check if works with other methods.
  ind_data_only <- purrr::map2_df(ind_data_only, directions, ~ .x*.y)

  # get codes for individual treatment, if specified, and separated data sets
  if (!is.null(individual)){
    # individual codes
    indiv_codes <- names(individual)
    # the remaining codes (for default treatment)
    notindiv_codes <- setdiff(IndCodes,indiv_codes)
    # get indicators for default treatment
    ind_data_def <- ind_data_only[notindiv_codes]
    # get indicators for individual treatment
    ind_data_indv <- ind_data_only[indiv_codes]
  } else {
    ind_data_def <- ind_data_only
  }

  ##---- Normalisation function ----##

  normfunc <- function(df, ntype, npara){

    if (ntype == "minmax"){

      # MIN MAX
      if (is.null(npara)){ # default parameters
        npara <- c(0,100)
      }
      if(is.data.frame(df)){
        dfn <- purrr::modify(df,~{ (.x-min(.x, na.rm = TRUE))/(max(.x, na.rm = TRUE)-min(.x, na.rm = TRUE))*(npara[2]-npara[1]) + npara[1]} )
      } else {
        dfn <- (df-min(df, na.rm = TRUE))/(max(df, na.rm = TRUE)-min(df, na.rm = TRUE))*(npara[2]-npara[1]) + npara[1]
      }

    } else if (ntype == "zscore"){

      # Z SCORE
      if (is.null(npara)){ # default parameters
        npara <- c(0,1)
      }
      if(is.data.frame(df)){
        dfn <- purrr::modify(df,~{(.x-mean(.x, na.rm = TRUE))/stats::sd(.x, na.rm = TRUE)*npara[2] + npara[1]})
      } else {
        dfn <- (df-mean(df, na.rm = TRUE))/stats::sd(df, na.rm = TRUE)*npara[2] + npara[1]
      }

    } else if (ntype == "custom"){

      # CUSTOM
      if(is.data.frame(df)){
        dfn = tryCatch({
          purrr::modify(df,npara)
        }, error = function(e) {
          stop("Error: custom function not valid for some reason.")
        })
      } else {
        dfn = tryCatch({
          rlang::exec(npara, df)
        }, error = function(e) {
          stop("Error: custom function not valid for some reason.")
        })
      }

    } else if (ntype == "scaled"){

      # SCALED
      if (is.null(npara)){ # default parameters
        npara <- c(0,1)
      }
      dfn <- (df-npara[1])/npara[2]

    } else if (ntype == "rank"){

      # RANK
      if(is.data.frame(df)){
        dfn <- purrr::modify(df,~{rank(.x, na.last = "keep", ties.method = "average")} )
      } else {
        dfn <- rank(df, na.last = "keep", ties.method = "average")
      }

    }  else if (ntype == "borda"){

      # BORDA (rank just minus 1)
      if(is.data.frame(df)){
        dfn <- purrr::modify(df,~{rank(.x, na.last = "keep", ties.method = "average") - 1} )
      } else {
        dfn <- rank(df, na.last = "keep", ties.method = "average") - 1
      }

    } else if (ntype == "prank"){

      # PERCENTILE RANK
      if(is.data.frame(df)){
        dfn <- purrr::modify(df,dplyr::percent_rank)
      } else {
        dfn <- dplyr::percent_rank(df)
      }
    } else if (ntype == "fracmax"){

      # FRACTION OF MAX VALUE
      if(is.data.frame(df)){
        dfn <- purrr::modify(df, ~{.x/max(.x, na.rm = T)} )
      } else {
        dfn <- df/max(df, na.rm = T)
      }

    } else if (ntype == "dist2targ"){

      # DISTANCE TO TARGET (targ taken from metadata)
      if(is.data.frame(df)){
        dfn <- purrr::modify2(df, COIN$Input$IndMeta$Target, ~{1 - (.y - .x)/(max(.x,na.rm = T)-min(.x, na.rm = T))} )
        dfn[dfn>1] <- 1
      } else {
        dfn <- 1 - (COIN$Input$IndMeta$Target[ii] - df)/(max(df,na.rm = T)-min(df, na.rm = T))
        dfn[dfn>1] <- 1
      }
    } else if (ntype == "dist2ref"){

      # DISTANCE TO REFERENCE UNIT
      if (is.null(npara)){
        stop("You need to specify a reference unit (UnitCode) via npara.")
      }
      # get index of reference country
      iref <- which(out$UnitCodes==npara)

      if(is.data.frame(df)){
        dfn <- purrr::modify(df, ~{ 1 - (.x[iref] - .x)/(max(.x,na.rm = T)-min(.x, na.rm = T)) } )
      } else {
        dfn <- 1 - (df[iref] - df)/(max(df,na.rm = T)-min(df, na.rm = T))
      }

    } else if (ntype == "dist2max"){

      # DISTANCE TO MAX
      if(is.data.frame(df)){
        dfn <- purrr::modify(df, ~{ 1 - (max(.x, na.rm = T) - .x)/(max(.x,na.rm = T)-min(.x, na.rm = T)) } )
      } else {
        dfn <- 1 - (max(df, na.rm = T) - df)/(max(df,na.rm = T)-min(df, na.rm = T))
      }

    } else if (ntype == "none"){

      # Nothing
      dfn <- df

    } else {
      stop("Normalisation type not recognised.")
    }
    return(dfn)
  }

  ##---- First, deal with default treatment

  if (indiv_only == FALSE){
    ind_data_def_norm <- normfunc(ind_data_def, ntype, npara)
  } else {
    ind_data_def_norm <- ind_data_def
  }# end default treatment

  ##---- Now, deal with individual treatment

  if(!is.null(individual)){ # check if individual is specified

    # loop over indicators specced in individual
    for(ii in 1:length(indiv_codes)){

      # get col
      icol <- ind_data_indv[[ii]]
      # normalise col
      icol <- normfunc(icol, individual[[ii]]$ntype, individual[[ii]]$npara)
      # put col back
      ind_data_indv[ii] <- icol

    } # end loop over indicators
  } # end if individual

  # to output, we just need to reunite with the non-data cols of the data set first (code and name)
  dataout <- out$ind_data
  if (!is.null(individual)){
    dataout[notindiv_codes] <- ind_data_def_norm
    dataout[indiv_codes] <- ind_data_indv
  } else {
    dataout[IndCodes] <- ind_data_def_norm
  }

  if(is.null(out2)){
    out2 <- "COIN"
  }

  if (is.data.frame(COIN) | (out2=="df") ){ # Output to data frame

    return(dataout)

  } else {

    COIN$Data$Normalised <- dataout
    return(COIN)
  }
}


