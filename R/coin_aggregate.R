#' Aggregate indicators
#'
#' Takes indicator data and a specified structure and hierarchically aggregates according to the structure
#' specified in `IndMeta`. Uses a variety of aggregation methods as specified by `agtype`, which can be different for
#' each level of aggregation (see `agtype_by_level`).
#'
#' This function aggregates indicators according to the index structure specified in `IndMeta`. It will either use a single
#' aggregation method for all aggregation levels (by specifying `agtype`) or can use a different aggregation method for each
#' level of the index (see `agtype_by_level`). Aggregation methods are typically weighted (e.g. weighted means), and the weights
#' for the aggregation are specified using the `agweights` argument.
#'
#' By default, this function will aggregate wherever possible - generally this means that if at least one value is available
#' for a given unit inside an aggregation group, it will return an aggregated score. Optionally, you can also specify a data
#' availability threshold which will instead return `NA` if the data availability (within group and for each unit) falls below
#' the threshold. For example, you may have four indicators inside a group, and you might want to only produce an aggregated
#' score if data availability is at least 50% - this would be specified by `avail_limit = 0.5`. It is also possible to specify
#' different data availability thresholds for different levels of the index, by specifying `avail_limit` as a vector which has
#' one value for every aggregation level (the first value gives the threshold for the first aggregation, and so on up to the
#' final level).
#'
#' @param COIN COIN object
#' @param agtype The type of aggregation method. One of either:
#' * `"arith_mean"` - weighted arithmetic mean
#' * `"median"` - weighted median
#' * `"geom_mean"` - weighted geometric mean
#' * `"harm_mean"` - weighted harmonic mean
#' * `"copeland"` - weighted Copeland method
#' * `"custom"` - a custom function -  see `agfunc`
#' * `"mixed"` - a different aggregation method for each level. In this case, aggregation methods are specified as any of the previous
#' options using the `agtype_bylevel` argument.
#' @param agweights The weights to use in the aggregation. This can either be:
#' `NULL`, in which case it will use the weights that were attached to `IndMeta` and `AggMeta` in [assemble()] (if they exist), or
#' A character string which corresponds to a named list of weights stored in `.$Parameters$Weights`. You can either add these manually or through [rew8r()].
#' E.g. entering `agweights = "Original"` will use the original weights read in on assembly. This is equivalent to `agweights = NULL`.
#' Or, a data frame of weights to use in the aggregation.
#' @param dset Which data set (contained in COIN object) to use
#' @param agtype_bylevel A character vector with aggregation types for each level. Note that if this is specified, `agtype` *must*
#' be specified as `agtype = "mixed"`, otherwise `agtype_by_level` will be ignored.
#' @param agfunc A custom function to use for aggregation if `agtype = "custom"`, of the type \eqn{y = f(x,w)},
#' where \eqn{y} is a scalar aggregated value and \eqn{x} and \eqn{w} are vectors of indicator values and weights respectively.
#' Ensure that `NA`s are handled (e.g. set `na.rm = T`) if your data has missing values.
#' @param avail_limit A data availability threshold, below which aggregation returns NA. This parameter is the fraction of data
#' availability needed in a given aggregation group to return an aggregated score. Specified as either `NULL` (default, aggregation
#' values are always returned if possible) or a value between 0 and 1 (below this value of data availability, `NA` will be
#' returned). See Details.
#' are ignored during aggregation, so that as long as there is at least one value in an aggregation group
#' @param out2 Where to output the results. If `"COIN"` (default for COIN input), appends to updated COIN,
#' otherwise if `"df"` outputs to data frame.
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
#' @examples
#' # assemble a COIN first
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # normalise the data
#' ASEM <- normalise(ASEM, dset = "Raw")
#' # aggregate the data
#' ASEM <- COINr::aggregate(ASEM, agtype="arith_mean", dset = "Normalised")
#' # check aggregated data set exists
#' stopifnot(!is.null(ASEM$Data$Aggregated))
#'
#' @return An updated COIN containing the new aggregated data set at `.$Data$Aggregated`.
#'
#' @export

aggregate <- function(COIN, agtype = "arith_mean", agweights = NULL, dset = NULL,
                      agtype_bylevel = NULL, agfunc = NULL, avail_limit = NULL, out2 = NULL){

  # Check for dset. If not specified, exit.
  if (is.null(dset)){
    stop("dset is NULL. Please specify which data set to operate on.")
  }

  out <- getIn(COIN, dset = dset)
  ind_data <- out$ind_data

  # Require COIN object for this function
  if (out$otype != "COINobj"){ # NOT COIN obj
    stop("Aggregation requires a COIN object (you need data plus the index structure). Use coin_assemble first.")
  }

  # Write function inputs to .$Method
  COIN$Method$aggregate$agtype <- agtype
  COIN$Method$aggregate$agweights <- agweights
  COIN$Method$aggregate$dset <- dset
  COIN$Method$aggregate$agtype_bylevel <- agtype_bylevel
  COIN$Method$aggregate$agfunc <- agfunc
  COIN$Method$aggregate$avail_limit <- avail_limit

  # get weights - if not explicitly specified, we assume it is in the obj
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
  } else {
    if(is.null(agtype_bylevel)){
      stop("If you specify a mixed aggregation method you must specify the method for each level using the agtype_bylevel argument. See ?aggregate.")
    }
  }
  agtypes <- agtype_bylevel

  # prepare data availability thresholds, if they exist
  if(!is.null(avail_limit)){
    if(length(avail_limit)==1){
      # if only one value, use for all levels
      avail_limit <- rep(avail_limit, COIN$Parameters$Nlevels - 1)
    } else if (length(avail_limit) != COIN$Parameters$Nlevels - 1){
      # trap error if any other length spefified
      stop("Length of avail_limit must be equal to 1 or the number of levels minus 1.")
    }
  }

  # the columns with aggregation info in them
  agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg"))
  iMeta <- as.data.frame(metad[c("IndCode", names(agg_cols))])

  for (aglev in 1:(COIN$Parameters$Nlevels - 1)){ # Loop over number of aggregation levels

    agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
    agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
    weights_lev <- agweights$Weight[agweights$AgLevel == aglev] # the weights at this level

    # first we have to get the columns (indicators, or agg levels below) to aggregate
    if (aglev ==1){
      sub_codes <- metad$IndCode # the ingredients to aggregate are the base indicators
    } else {
      sub_codes <- dplyr::pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
    }

    # get codes for just this level and the one below, with no repetitions
    iMeta_lev <- unique(iMeta[c(aglev, aglev + 1)])

    agtype_lev <- agtypes[aglev] # the aggregation type for this level

    for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level

      #iselect_old <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group

      iselect <- iMeta_lev[iMeta_lev[2] == agg_names[agroup], 1]

      # get weights belonging to group, using codes
      # note the following line was a bug, now fixed
      # weights_group_old <- weights_lev[unique(sub_codes) %in% iselect_old]
      weights_group <- agweights$Weight[match(iselect, agweights$Code)]

      # aggregate. NOTE: this is is not a very efficient way to do these operations and will be updated
      # at some point.
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
      } else if (agtype_lev == "geom_mean_rescaled"){
        newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := geoMean_rescaled(dplyr::c_across(cols = dplyr::everything()),
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

      } else if (agtype_lev == "custom") {

        newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := agfunc(dplyr::c_across(cols = dplyr::everything()),
                                                          w = weights_group))

      } else {
        stop("Aggregation type not recognised.")
      }

      # Missing data reset, if asked
      if(!is.null(avail_limit)){
        # we will reset any values that have missing data thresholds below limits specified
        # get data
        dat2agg <- ind_data[iselect]
        # now replace any aggregated vals by NA, if they fall below the limit
        newcol[rowSums(!is.na(dat2agg))/ncol(dat2agg) < avail_limit[aglev], ] <- NA

      }

      ind_data <- cbind(ind_data,newcol) # add new col to data set
    }
  }

  # OUTPUT
  if(is.null(out2)){out2 <- "COIN"}

  if(out2=="COIN"){

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
#' Weighted geometric mean of a vector. `NA` are skipped by default. This function is used inside
#' [aggregate()].
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # weighted geometric mean
#' geoMean(x,w)
#'
#' @return The geometric mean, as a numeric value.
#'
#' @export

geoMean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified for geometric mean, using equal weights.")
  }

  if(any(!is.na(x))){

    if(any((x <= 0), na.rm = TRUE)){
      stop("Negative or zero values found when applying geometric mean. This doesn't work because geometric
         mean uses log. Normalise to remove negative/zero values first or use another aggregation method.")}

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA
    # calculate geom mean
    gm <- exp( sum(w * log(x), na.rm = TRUE)/sum(w, na.rm = TRUE) )

  } else {
    gm <- NA
  }

  return(gm)

}

#' Rescaled weighted geometric mean
#'
#' *NOTE this function is not really in use but is kept here for the moment. Not sure it is very useful.*
#'
#' Weighted geometric mean of a vector. Here, any zero or negative values are automatically dealt with
#' by re-scaling the data to be all positive, i.e. it shifts so that the minimum is equal to 0.1.
#'
#' Note that this could be better achieved by normalising first. However, following default normalisation
#' between 0 and 100, this function offers a quick way to test the effect of a geometric mean, for example in
#' a sensitivity analysis, and avoids bugs arising.
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # rescaled weighted geometric mean
#' geoMean_rescaled(x,w)
#'
#' @return Rescaled weighted geometric mean, as a numeric value.
#'
#' @export

geoMean_rescaled <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified for geometric mean, using equal weights.")
  }

  # because this uses the min function, sometimes we may get all NAs.
  # in that case, it throws an annoying warning. To avoid, we just output NA.

  if(any(!is.na(x))){
    # make all values positive with min value 0.1
    x <- x - min(x, na.rm = TRUE) + 0.1
    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA
    # calc geo mean
    gm <- exp( sum(w * log(x), na.rm = TRUE)/sum(w, na.rm = TRUE) )
  } else {
    gm <- NA
  }





  return(gm)

}

#' Weighted harmonic mean
#'
#' Weighted harmonic mean of a vector. `NA` are skipped by default. This function is used inside
#' [aggregate()].
#'
#' @param x A numeric vector of positive values.
#' @param w A vector of weights, which should have length equal to `length(x)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # a vector of values
#' x <- 1:10
#' # a vector of weights
#' w <- runif(10)
#' # weighted harmonic mean
#' harMean(x,w)
#'
#' @return Weighted harmonic mean, as a numeric value.
#'
#' @export

harMean <- function(x, w = NULL){

  if(is.null(w)){
    # default equal weights
    w <- rep(1,length(x))
    message("No weights specified harmonic mean, using equal weights.")
  }

  if(any(!is.na(x))){

    if(any(x == 0, na.rm = TRUE)){
      stop("Zero values found when applying harmonic mean. This doesn't work because harmonic
         mean uses 1/x. Normalise to remove zero values first or use another aggregation method.")}

    # have to set any weights to NA to correspond to NAs in x
    w[is.na(x)] <- NA

    hm <- sum(w, na.rm = TRUE)/sum(w/x, na.rm = TRUE)

  } else {
    hm <- NA
  }

  return(hm)

}

#' Outranking matrix
#'
#' Constructs an outranking matrix based on a data frame of indicator data and corresponding weights. This function is used inside
#' [aggregate()].
#'
#' @param ind_data A data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A vector of weights, which should have length equal to `ncol(ind_data)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # get a sample of a few indicators
#' ind_data <- COINr::ASEMIndData[12:16]
#' # calculate outranking matrix
#' outlist <- outrankMatrix(ind_data)
#' # see fraction of dominant pairs (robustness)
#' outlist$fracDominant
#'
#' @return A list with:
#' * `.$OutRankMatrix` the outranking matrix with `nrow(ind_data)` rows and columns (matrix class).
#' * `.$nDominant` the number of dominance/robust pairs
#' * `.$fracDominant` the percentage of dominance/robust pairs
#'
#' @export

outrankMatrix <- function(ind_data, w = NULL){

  stopifnot(is.data.frame(ind_data) | is.matrix(ind_data))

  if (!all(apply(ind_data, 2, is.numeric))){
    stop("Non-numeric columns in input data frame or matrix not allowed.")
  }

  nInd <- ncol(ind_data)
  nUnit <- nrow(ind_data)

  if(is.null(w)){
    # default equal weights
    w <- rep(1,nUnit)
    message("No weights specified for outranking matrix, using equal weights.")
  }

  # make w sum to 1
  w = w/sum(w, na.rm = TRUE)

  # prep outranking matrix
  orm <- matrix(NA, nrow = nUnit, ncol = nUnit)

  for (ii in 1:nUnit){

    # get iith row, i.e. the indicator values of unit ii
    rowii <- ind_data[ii,]

    for (jj in 1:nUnit){

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

  # find number of dominance pairs
  ndom <- sum(orm==1, na.rm = TRUE)
  npairs <- (nUnit^2 - nUnit)/2
  prcdom <- ndom/npairs

  return(list(
    OutRankMatrix = orm,
    nDominant = ndom,
    fracDominant = prcdom))

}

#' Copeland scores
#'
#' Aggregates a data frame of indicator values into a single column using the Copeland method. This function is used inside
#' [aggregate()], and calls `outrankMatrix()`.
#'
#' @param ind_data A data frame or matrix of indicator data, with observations as rows and indicators
#' as columns. No other columns should be present (e.g. label columns).
#' @param w A vector of weights, which should have length equal to `ncol(ind_data)`. Weights are relative
#' and will be re-scaled to sum to 1. If `w` is not specified, defaults to equal weights.
#'
#' @examples
#' # get a sample of a few indicators
#' ind_data <- ASEMIndData[12:16]
#' # calculate outranking matrix
#' cop_results <- copeland(ind_data)
#' # check output
#' stopifnot(length(cop_results$Scores) == nrow(ind_data))
#'
#' @return Numeric vector of Copeland scores.
#'
#' @export

copeland <- function(ind_data, w = NULL){

  # get outranking matrix
  orm <- outrankMatrix(ind_data, w)$OutRankMatrix

  # get scores by summing across rows
  scores <- rowSums(orm, na.rm = TRUE)

  outlist <- list(Scores = scores, OutRankMat = orm)
  return(outlist)
}
