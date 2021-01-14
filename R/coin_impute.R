#' Impute missing data
#'
#' Imputation of data sets using a variety of methods.
#'
#' @param COINobj A dataframe of indicator data following the COINR format
#' @param imtype The type of normalisation method. Either "minmax", "zscore", or "custom".
#' @param inames Character vector of indiator names, indicating which columns to normalise. Use this if you only want to normalise certain columns, or you are inputting a data frame with some columns which are not to be normalised (e.g. country names, groups, ...)
#' @param dset The data set in .$Data to impute
#' @param groupvar The name of the column to use for by-group imputation. Only applies when imtype is set to a group option.
#' @param byyear Logical: set to TRUE to impute separately for each year, otherwise FALSE to impute across all years.
#'
#' @importFrom tidyr replace_na
#' @importFrom stringr str_subset
#' @importFrom dplyr filter mutate across bind_rows group_by ungroup
#' @importFrom rlang .data
#'
#' @examples \dontrun{COINobj <- coin_impute(COINobj, imtype = "ind_mean", dset = "raw")}
#'
#' @return A dataframe of normalised indicators.
#'
#' @export

coin_impute <- function(COINobj, imtype = "ind_mean", inames = NULL,
                        dset = "Raw", groupvar = NULL, byyear = FALSE){

  # First. check to see what kind of input we have.
  out <- coin_aux_objcheck(COINobj, dset = dset, inames = inames)
  ind_data <- out$ind_data
  ind_names <- out$ind_names

  # get number of NAs before imputation
  nasumz <- colSums(is.na(ind_data))
  nNA_start <- sum(nasumz[ind_names])

  message(paste0("Missing data points detected = ", nNA_start))

  ###### IMPUTATION ######

  # first, get some info about years, if needed (either when imputing by year, or when using latest year)
  if (byyear==T | imtype == "latest_year"){
    nyears <- ind_data %>% select(starts_with("Year")) %>% unique() %>% nrow() # number of years present
    yrcol <- ind_data %>% colnames() %>% stringr::str_subset("Year") # the column name which has the years in it
    yrs <- ind_data %>% select(starts_with("Year")) %>% unique()
  }

  ## Now actually do the imputation, depending on the type...

  if (imtype == "agg_mean"){ # use the mean of the other indicators in the aggregation group. Only works if data is normalised first.

    # first check that normalised data is available
    if (exists("Normalised",COINobj$Data)){ # we may proceed...

      # call the aggregation function.... seems easiest. TO FINISH
      stop("Sorry, didn't finish this option yet. Stay tuned.")

    } else {
      stop("Normalised data set not found (required for agg_mean). Please run coin_normalise first.")
    }

  } else if (imtype == "ind_mean"){ # impute using column MEAN, i.e. the mean of the indicator over all units

    if (byyear==T){ # If we have to impute by year

      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>%
          dplyr::mutate(dplyr::across(all_of(ind_names), ~{tidyr::replace_na(.x, mean(.x, na.rm = TRUE))}))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # the following applies the replace_na function to all columns specified by ind_names
      ind_data_imp <- ind_data %>% dplyr::mutate(dplyr::across(all_of(ind_names), ~{tidyr::replace_na(.x, mean(.x, na.rm = TRUE))}))
    }

  } else if (imtype == "ind_median"){ # impute using column MEDIAN, i.e. the median of the indicator over all units

    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>%
          dplyr::mutate(dplyr::across(all_of(ind_names), ~{tidyr::replace_na(.x, median(.x, na.rm = TRUE))}))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # the following applies the replace_na function to all columns specified by ind_names
      ind_data_imp <- ind_data %>% dplyr::mutate(dplyr::across(all_of(ind_names), ~{tidyr::replace_na(.x, median(.x, na.rm = TRUE))}))
    }

  } else if (imtype == "indgroup_mean"){ # use column MEAN, restricted to a particular group

    if(is.null(groupvar)){stop("Group mean imputation requires that you specify which grouping to use (column name).")} # throw error if no group

    if (byyear==T){ # If we have to impute by year

      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% dplyr::group_by(.dots=groupvar) %>%
          dplyr::mutate(dplyr::across(all_of(ind_names), ~tidyr::replace_na(.x, mean(.x, na.rm = TRUE))))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # This works by grouping the data by the grouping variable first. Operations then performed by group.
      ind_data_imp <- ind_data %>% dplyr::group_by(.dots=groupvar) %>%
        dplyr::mutate(dplyr::across(all_of(ind_names), ~tidyr::replace_na(.x, mean(.x, na.rm = TRUE))))
    }

  } else if (imtype == "indgroup_median"){ # use column MEDIAN, restricted to a particular group

    if(is.null(groupvar)){stop("Group median imputation requires that you specify which grouping to use (column name).")} # throw error if no group

    if (byyear==T){ # If we have to impute by year
      ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
      for (yr in 1:nyears){

        ind_data_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) # get only rows from year
        #now impute...
        ind_data_imp_list[[yr]] <- ind_data_yr %>% dplyr::group_by(.dots=groupvar) %>%
          dplyr::mutate(dplyr::across(all_of(ind_names), ~tidyr::replace_na(.x, median(.x, na.rm = TRUE))))
      }
      ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together

    } else { # if not imputing by year
      # This works by grouping the data by the grouping variable first. Operations then performed by group.
      ind_data_imp <- ind_data %>% dplyr::group_by(.dots=groupvar) %>%
        dplyr::mutate(dplyr::across(all_of(ind_names), ~tidyr::replace_na(.x, median(.x, na.rm = TRUE))))
    }

  } else if (imtype == "latest_year"){ # substitute NAs with any available points from previous years

    ind_data_imp_list <- vector("list",nyears) # make an empty list for populating, one for each year
    ind_data_imp_list[[1]] <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[1,1]]) # only imputing backwards in time, so first year available remains the same.

    if(nyears>1){
    for (yr in 2:nyears){

      # get indicator from year and year-1 as separate dfs
      ind_data_yr_all <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr,1]]) %>% as.data.frame() # get only rows from year and ind. cols. Have to change to df because otherwise next step doesn't work
      ind_data_yr <- ind_data_yr_all %>% select(ind_names) # done in 2 steps so can access the other cols in a min.
      ind_data_prev_yr <- dplyr::filter(ind_data,.data[[yrcol]] == yrs[[yr-1,1]]) %>% as.data.frame() %>% select(ind_names) # get only rows from year-1

      #now substitute any NAs from yr with those from prev_yr
      ind_data_yr[is.na(ind_data_yr)] <- ind_data_prev_yr[is.na(ind_data_yr)]

      ind_data_yr <- cbind(select(ind_data_yr_all,-ind_names),ind_data_yr)

      ind_data_imp_list[[yr]] <- ind_data_yr # add to the list
    }
    ind_data_imp <- dplyr::bind_rows(ind_data_imp_list) # join everything back together
    } else {stop("You can't impute by latest year with only one year of data.")}
  }

  nasumz <- colSums(is.na(ind_data_imp))
  nNA_end <- sum(nasumz[ind_names]) # counts total number of NAs in indicator columns, after imputation
  message(paste0("Missing data points imputed = ", nNA_start-nNA_end, ", using method = ", imtype))

  if (is.data.frame(COINobj)){ # Data frame
    return(ind_data_imp)
  } else {
    COINobj$Data$Imputed <- dplyr::ungroup(ind_data_imp)
    COINobj$Method$Imputation$imtype <- imtype
    COINobj$Method$Imputation$inames <- inames
    COINobj$Method$Imputation$dset <- dset
    COINobj$Method$Imputation$groupvar <- groupvar
    COINobj$Method$Imputation$byyear <- byyear
    COINobj$Method$Imputation$NImputed <- nNA_start-nNA_end
    return(COINobj)
  }

}
