#' Aggregate indicators
#'
#' Takes indicator data and a specified structure and hierarchically aggregates to a single index (or whatever the structure specified)
#'
#' @param COIN COIN object
#' @param agtype The type of aggregation method.
#' @param agweights The weights to use in the aggregation. This can either be:
#' NULL, in which case it will use the weights that were attached to IndMeta and AggMeta in GII_assemble (if they exist), or
#' A character string which corresponds to a named list of weights stored in .$Parameters$Weights. You can either add these manually or through rew8r.
#' E.g. entering agweights = "Original" will use the original weights read in on assembly. This is equivalent to agweights = NULL.
#' Or, a list of weights to use in the aggregation.
#' @param dset Which data set (contained in COIN object) to use
#' @param agtype_bylevel A character vector with aggregation types for each level
#'
#' @importFrom dplyr "all_of"
#' @importFrom dplyr "rowwise"
#' @importFrom dplyr "transmute"
#' @importFrom dplyr "pull"
#' @importFrom rlang :=
#' @importFrom matrixStats "weightedMean"
#' @importFrom matrixStats "weightedMedian"
#' @importFrom dplyr "c_across"
#' @importFrom tibble as_tibble
#'
#' @examples \dontrun{COINlist <- aggregate(COINlist, agtype="arith_mean", dset = "normalised")}
#'
#' @return An updated COIN object containing the new aggregated data set.
#'
#' @export

aggregate <- function(COIN, agtype="arith_mean", agweights = NULL,
                           dset = "Normalised", agtype_bylevel = NULL, out2 = NULL){

  out <- getIn(COIN, dset = dset)
  ind_data <- out$ind_data

  # Require COIN object for this function
  if (out$otype != "COINobj"){ # NOT COIN obj
    stop("Aggregation requires a COIN object (you need data plus the index structure). Use coin_assemble first.")
  }

  # get weights - if not explicitly specified, we assume it is in the GII obj
  if (is.null(agweights)){
    # use original weights
    agweights <- COIN$Parameters$Weights$Original
  } else if (is.character(agweights)){
    # agweights specified by a character string
    agweights <- COIN$Parameters$Weights[[agweights]]
  }
  # if the operations above point to sth that doesnt exist, will return NULL
  if (is.null(agweights)){
    stop("Cannot find weights, please check.")
  }

  metad <- COIN$Input$IndMeta

  ##### NOW AGGREGATE #######

  if (agtype != "mixed"){
    # we just use the same type for every level
    agtype_bylevel <- rep(agtype, COIN$Parameters$Nlevels - 1)
  }
  agtypes <- agtype_bylevel

  # the columns with aggregation info in them
  agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg"))

  for (aglev in 1:(COIN$Parameters$Nlevels - 1)){ # Loop over number of aggregation levels

    agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
    agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
    weights_lev <- agweights[[aglev]] # the weights at this level

    # first we have to get the columns (indicators, or agg levels below) to aggregate
    if (aglev ==1){
      sub_codes <- metad$IndCode # the ingredients to aggregate are the base indicators
    } else {
      sub_codes <- dplyr::pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
    }

    agtype_lev <- agtypes[aglev] # the aggregation type for this level

    for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level

      iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
      # get weights belonging to group, using codes
      weights_group <- weights_lev[unique(sub_codes) %in% iselect]

      if (agtype_lev == "arith_mean"){
        newcol <- ind_data %>% select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMean(dplyr::c_across(cols =dplyr:: everything()),
                                                                            w = weights_group, na.rm = TRUE))
      } else if (agtype_lev == "median"){
        newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMedian(dplyr::c_across(cols = dplyr::everything()),
                                                                              w = weights_group, na.rm = TRUE))
      } else if (agtype_lev == "geom_mean"){
        newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := geoMean(dplyr::c_across(cols = dplyr::everything()),
                                                          w = weights_group))
      } else if (agtype_lev == "harm_mean"){
        newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := harMean(dplyr::c_across(cols = dplyr::everything()),
                                                          w = weights_group))

      } else if (agtype_lev == "copeland") {

        # get columns
        colz <- ind_data[iselect]

        # get copeland scores
        copeout <- copeland(colz, weights_group)
        newcol <- copeout$Scores %>% as.data.frame()
        colnames(newcol) <- agg_names[agroup]

        # chkk <- copeout$OutRankMat %>% as.data.frame()
        # colnames(chkk) <- out$UnitCodes
        # rownames(chkk) <- out$UnitCodes
        # browser()


      } else {
        stop("Normalisation type not recognised.")
      }

      ind_data <- cbind(ind_data,newcol) # add new col to data set
    }
  }

  # OUTPUT
  if(is.null(out2)){out2 <- "COIN"}

  if(out2=="COIN"){

    # Write function inputs to .$Method
    COIN$Method$Aggregation$agtype <- agtype
    COIN$Method$Aggregation$agweights <- agweights
    COIN$Method$Aggregation$dset <- dset
    COIN$Method$Aggregation$agtype_bylevel <- agtype_bylevel

    # add to the COIN object
    COIN$Data$Aggregated <- tibble::as_tibble(ind_data)
    return(COIN)

  } else if (out2=="df"){
    # output tibble directly
    return(tibble::as_tibble(ind_data))
  } else {
    stop("Sorry, didn't recognise out2.")
  }

}

#' Weighted geometric mean
#'
#' Weighted geometric mean of a vector. NAs are skipped by default.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to length(x). Weights are relative
#' and will be rescaled to sum to 1. If w is not specified, defaults to equal weights.
#'
#' @return Geometric mean
#'
#' @export

geoMean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,ncol(ind_data))
    message("No weights specified for outranking matrix, using equal weights.")
  }

  if(any(x <= 0)){
    stop("Negative or zero values found when applying geometric mean. This doesn't work because geometric
         mean uses log. Normalise to remove negative/zero values first or use another aggregation method.")}

  gm <- exp( sum(w * log(x), na.rm = TRUE)/sum(w, na.rm = TRUE) )

  return(gm)

}

#' Weighted harmonic mean
#'
#' Weighted harmonic mean of a vector. NAs are skipped by default.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to length(x). Weights are relative
#' and will be rescaled to sum to 1. If w is not specified, defaults to equal weights.
#'
#' @return Harmonic mean
#'
#' @export

harMean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,ncol(ind_data))
    message("No weights specified for outranking matrix, using equal weights.")
  }

  if(any(x == 0)){
    stop("Zero values found when applying harmonic mean. This doesn't work because harmonic
         mean uses 1/x. Normalise to remove zero values first or use another aggregation method.")}

  hm <- sum(w, na.rm = TRUE)/sum(w/x, na.rm = TRUE)

  return(hm)

}

#' Outranking matrix
#'
#' Constructs an outranking matrix based on a data frame of indicator data and corresponding weights
#'
#' @param ind_data A data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A vector of weights, which should have length equal to ncol(ind_data). Weights are relative
#' and will be rescaled to sum to 1. If w is not specified, defaults to equal weights.
#'
#' @return Outranking matrix
#'
#' @export

outrankMatrix <- function(ind_data, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,ncol(ind_data))
    message("No weights specified for outranking matrix, using equal weights.")
  }

  # make w sum to 1
  w = w/sum(w, na.rm = TRUE)

  # prep outranking matrix
  orm <- matrix(NA, nrow = nrow(ind_data), ncol = nrow(ind_data))

  for (ii in 1:nrow(orm)){

    # get iith row, i.e. the indicator values of unit ii
    rowii <- ind_data[ii,]

    for (jj in 1:ncol(orm)){

      if (ii==jj){
        # diag vals are zero
        orm[ii, jj] <- 0
      } else if (ii>jj){
        # to save time, only calc upper triangle of matrix. If lower triangle, do 1-upper
        orm[ii, jj] <- 1 - orm[jj, ii]
      } else {

        # get jjth row, i.e. the indicator values of unit jj
        rowjj <- ind_data[jj,]

        # get score. Sum of weights where ii scores higher than jj, and half sum of weights where they are equal
        orm[ii, jj] <- sum(
          sum(w[rowii > rowjj], na.rm = TRUE),
          sum(w[rowii == rowjj], na.rm = TRUE)/2,
          na.rm = TRUE)

      }
    }
  }

  return(orm)

}

#' Copeland scores
#'
#' Aggregates a data frame of indicator values into a single column using the Copeland method.
#'
#' @param ind_data A data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A vector of weights, which should have length equal to ncol(ind_data). Weights are relative
#' and will be rescaled to sum to 1. If w is not specified, defaults to equal weights.
#'
#' @return Numeric vector of Copeland scores.
#'
#' @export

copeland <- function(ind_data, w = NULL){

  # get outranking matrix
  orm <- outrankMatrix(ind_data, w)

  # get scores by summing across rows
  scores <- rowSums(orm, na.rm = TRUE)

  outlist <- list(Scores = scores, OutRankMat = orm)
  return(outlist)
}
