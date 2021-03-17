#' Aggregate indicators
#'
#' Takes indicator data and a specified structure and hierarchically aggregates to a single index (or whatever the structure specified)
#'
#' @param COINobj COIN object
#' @param agtype The type of aggregation method.
#' @param agweights A list of weights to use in the aggregation. If not specified,
#' will look in the COIN object (i.e. via coin_weight.R).
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

aggregate <- function(COINobj, agtype="arith_mean", agweights = NULL,
                           dset = "Normalised", agtype_bylevel = NULL){

  out <- coin_aux_objcheck(COINobj, dset = dset)
  ind_data <- out$ind_data

  # Require COIN object for this function
  if (out$otype != "COINobj"){ # NOT COIN obj
    stop("Aggregation requires a COIN object (you need data plus the index structure). Use coin_assemble first.")
  }

  # Write function inputs to .$Method
  COINobj$Method$Aggregation$agtype <- agtype
  COINobj$Method$Aggregation$agweights <- agweights
  COINobj$Method$Aggregation$dset <- dset
  COINobj$Method$Aggregation$agtype_bylevel <- agtype_bylevel

  # get weights - if not explicitly specified, we assume it is in the COIN obj
  if (is.null(agweights)){
    if (exists("Weights", COINobj$Parameters)){
      agweights = COINobj$Parameters$Weights
    } else {
      stop("No weights found in COIN object. Specify weights first either through coin_weight.R or as metadata input to coin_assemble.")
    }
  }

  metad <- COINobj$Input$IndMeta

  ##### NOW AGGREGATE #######

  if (agtype == "arith_mean"){ # Arithmetic mean

    agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg")) # the columns with aggregation info in them

    for (aglev in 1:(COINobj$Parameters$Nlevels - 1)){ # Loop over number of aggregation levels

      agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
      agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
      weights_lev <- agweights[[aglev]] # the weights at this level

      # first we have to get the columns (indicators, or agg levels below) to aggregate
      if (aglev ==1){
        sub_codes <- metad$IndCode # the ingredients to aggregate are the base indicators
      } else {
        sub_codes <- dplyr::pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
      }

      for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level

        iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
        # get weights belonging to group, using codes
        weights_group <- weights_lev[unique(sub_codes) %in% iselect]

        # Now get the mean. Had to do in a roundabout way to avoid rowmeans type functions... probably an easier way exists though
        newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMean(dplyr::c_across(cols = dplyr::everything()),
                                                                            w = weights_group, na.rm = TRUE))
        ind_data <- cbind(ind_data,newcol) # add new col to data set

      }
    }
  } else if (agtype == "median"){ # Arithmetic mean

    agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg")) # the columns with aggregation info in them

    for (aglev in 1:(COINobj$Parameters$Nlevels - 1)){ # Loop over number of aggregation levels

      agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
      agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
      weights_lev <- agweights[[aglev]] # the weights at this level

      # first we have to get the columns (indicators, or agg levels below) to aggregate
      if (aglev ==1){
        sub_codes <- metad$Code # the ingredients to aggregate are the base indicators
      } else {
        sub_codes <- dplyr::pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
      }

      for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level

        iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
        # get weights belonging to group, using codes
        weights_group <- weights_lev[unique(sub_codes) %in% iselect]

        #ind_data <- ind_data %>% mutate(!!agg_names[agroup] := rowMeans(select(ind_data, all_of(iselect)), na.rm = TRUE)) # take mean, removing NAs. Also assigned col name using the weird := thing

        # Now get the mean. Had to do in a roundabout way to avoid rowmeans type functions... probably an easier way exists though
        newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
          dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMedian(dplyr::c_across(cols = dplyr::everything()),
                                                        w = weights_group, na.rm = TRUE))
        ind_data <- cbind(ind_data,newcol) # add new col to data set

      }
    }
  } else if (agtype == "mixed"){ # the aggregation can vary from one level to the next

    agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg")) # the columns with aggregation info in them

    for (aglev in 1:(COINobj$Parameters$Nlevels - 1)){ # Loop over number of aggregation levels

      agg_colname <- colnames(agg_cols[aglev]) # the name of the aggregation level
      agg_names <- unique(agg_cols[[aglev]]) # the names of the aggregation groups
      weights_lev <- agweights[[aglev]] # the weights at this level

      # first we have to get the columns (indicators, or agg levels below) to aggregate
      if (aglev ==1){
        sub_codes <- metad$Code # the ingredients to aggregate are the base indicators
      } else {
        sub_codes <- dplyr::pull(agg_cols, aglev-1) # the ingredients to aggregate are aggregate level below
      }

      agtype_lev <- agtype_bylevel[aglev] # the aggregation type for this level

      for (agroup in 1:length(agg_names)){ # loop over aggregation groups, inside the given agg level

        iselect <- sub_codes[metad[,agg_colname]==agg_names[agroup]] %>% unique() # get indicator names belonging to group
        # get weights belonging to group, using codes
        weights_group <- weights_lev[unique(sub_codes) %in% iselect]

        if (agtype_lev == "arith_mean"){
          newcol <- ind_data %>% select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
            dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMean(dplyr::c_across(cols =dplyr:: everything()),
                                                          w = weights_group, na.rm = TRUE))
          ind_data <- cbind(ind_data,newcol) # add new col to data set
        } else if (agtype_lev == "median"){
          newcol <- ind_data %>% dplyr::select(dplyr::all_of(iselect)) %>% dplyr::rowwise() %>%
            dplyr::transmute(!!agg_names[agroup] := matrixStats::weightedMedian(dplyr::c_across(cols = dplyr::everything()),
                                                          w = weights_group, na.rm = TRUE))
          ind_data <- cbind(ind_data,newcol) # add new col to data set
        } else if (agtype_lev == "geom_mean"){

        }
      }
    }
  }

  COINobj$Data$Aggregated <- tibble::as_tibble(ind_data) # add to the COIN object
  return(COINobj)
}
