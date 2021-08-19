#' Normalise indicator data sets
#'
#' A dataset of indicators is normalised using one of several methods. This function also supports custom normalisation.
#'
#' Normalisation refers to the operation of bringing variables (indicators) onto a common scale. This is typically done by matching
#' one or more indicator statistics. For example, the *min-max* method operates a linear transformation to make the minimum and
#' maximum values of each indicator to be equal. The *z-score* method makes the standard deviation and variance equal. And so on.
#'
#' This function supports a range of normalisation methods - see `ntype`. Some of these require supporting parameters or similar -
#' to see full details check the [online documentation](https://bluefoxr.github.io/COINrDoc/normalisation.html).
#'
#' Indicators can also be each normalised by a different method. See `individual`.
#'
#' @param COIN Either the COIN object, or a data frame of indicator data
#' @param ntype The type of normalisation method. Either "minmax", "zscore", "scaled", "rank", "borda", "prank", "fracmax", "dist2targ", "dist2ref", "dist2max", "custom" or "none".
#' @param npara Supporting object for ntype. This is a list of the form list(ntype = parameters_for_ntype). So,
#' if ntype = "minmax", npara could be list(minmax = c(0, 100)) to scale into the 0 to 100 interval
#' If ntype = "zscore", npara could be list(zscore = c(0, 1)) to scale to mean zero and std 1.
#' This means you can store parameters for more than one normalisation type side by side, which helps in
#' comparisons, adjustments, and sensitivity analyses.
#' @param dset The data set to normalise
#' @param directions A vector specifying the direction assigned to each indicator.
#' Needs to be the same length as the number of indicators, or the number of indicators in icodes, if specified.
#' @param individual A list of named lists specifiying individual normalisation to apply to specific indicators. Should be structured as follows:
#' The name of each sublist should be the indicator code. The the list elements are:
#' .$ntype is the type of normalisation to apply
#' .$npara is a corresponding object or parameters that are used by ntype, in the same format as npara above.
#' @param indiv_only Logical: if FALSE (default), indicators not specified in individual are subjected to default normalisation. Otherwise if TRUE they are not normalised.
#' @param out2 Where to output the results. If "COIN" (default for COIN input), appends to updated COIN,
#' otherwise if "df" outputs to data frame.
#'
#' @importFrom purrr "map2"
#' @importFrom purrr "modify"
#'
#' @examples \dontrun{
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # directly normalise raw data using min-max, onto 0-10 interval
#' ASEM <- normalise(ASEM, dset = "Raw", ntype = "minmax", npara = list(minmax = c(0,10)))}
#'
#' @return An updated COIN object with .$Data$Normalised added, or a normalised data frame.
#'
#' @export

normalise <- function(COIN, ntype = "minmax", npara = NULL,
                      dset = NULL, directions = NULL, individual = NULL,
                      indiv_only = NULL, out2 = NULL){

  # Check for dset. If not specified, exit.
  if (is.null(dset) & !("data.frame" %in% class(COIN))){
    stop("dset is NULL. Please specify which data set to operate on.")
  }

  ##----- SET DEFAULTS -------##
  # Done here because otherwise if we use regen, this input could be input as NULL
  if(is.null(indiv_only)){
    indiv_only <- FALSE
  }

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
      if (is.null(npara$minmax)){ # default parameters
        npara$minmax <- c(0,100)
      }
      if(is.data.frame(df)){
        dfn <- purrr::modify(df,~{ (.x-min(.x, na.rm = TRUE))/(max(.x, na.rm = TRUE)-min(.x, na.rm = TRUE))*(npara$minmax[2]-npara$minmax[1]) + npara$minmax[1]} )
      } else {
        dfn <- (df-min(df, na.rm = TRUE))/(max(df, na.rm = TRUE)-min(df, na.rm = TRUE))*(npara$minmax[2]-npara$minmax[1]) + npara$minmax[1]
      }

    } else if (ntype == "zscore"){

      # Z SCORE
      if (is.null(npara$zscore)){ # default parameters
        npara$zscore <- c(0,1)
      }
      if(is.data.frame(df)){
        dfn <- purrr::modify(df,~{(.x-mean(.x, na.rm = TRUE))/stats::sd(.x, na.rm = TRUE)*npara$zscore[2] + npara$zscore[1]})
      } else {
        dfn <- (df-mean(df, na.rm = TRUE))/stats::sd(df, na.rm = TRUE)*npara$zscore[2] + npara$zscore[1]
      }

    } else if (ntype == "custom"){

      # CUSTOM
      if(is.data.frame(df)){
        dfn = tryCatch({
          purrr::modify(df,npara$custom)
        }, error = function(e) {
          stop("Error: custom function not valid for some reason.")
        })
      } else {
        dfn = tryCatch({
          rlang::exec(npara$custom, df)
        }, error = function(e) {
          stop("Error: custom function not valid for some reason.")
        })
      }

    } else if (ntype == "scaled"){

      # SCALED
      if (is.null(npara$scaled)){ # default parameters
        npara$scaled <- c(0,1)
      }
      dfn <- (df-npara$scaled[1])/npara$scaled[2]

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
      if (is.null(npara$dist2ref)){
        stop("You need to specify a reference unit (UnitCode) via npara$dist2ref.")
      }
      # get index of reference country
      iref <- which(out$UnitCodes == npara$dist2ref)

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


