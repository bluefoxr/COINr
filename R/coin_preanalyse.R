#' Pre-aggregation analysis
#'
#' Takes a COIN object, or data frame and returns a table of statistics, including max, min, median, mean, std, kurtosis, etc. Flags indicators with possible outliers.
#'
#' @param COINobj A list of indicator data, stuctured using the COIN_assemble function
#' @param inames A character vector of indicator names to analyse. Defaults to all indicators.
#' @param dset The data set to analyse
#' @param t_skew Skewness threshold
#' @param t_kurt Kurtosis threshold
#' @param t_colin Collinearity threshold (absolute value of correlation)
#' @param t_denom High correlation with denominator threshold
#' @param t_missing Missing data threshold, in percent
#' @param IQR_coef Interquartile range coefficient, used for identifying outliers
#'
#' @importFrom e1071 skewness kurtosis
#' @importFrom purrr map_dbl
#' @importFrom dplyr if_else n_distinct
#' @importFrom corrplot cor.mtest
#' @importFrom tibble tibble add_column
#' @importFrom stats IQR cor median sd
#'
#' @examples \dontrun{df_norm <- getStats(COINobj, dset = "Raw")}
#'
#' @return If the input is a COIN object, returns an updated COIN object with relevant tables. If the input is a data frame, returns a table as a data frame.
#'
#' @export

getStats <- function(COINobj, inames = NULL, dset = "Raw",
                            t_skew = 2, t_kurt = 3.5, t_colin = 0.9, t_denom = 0.7,
                            t_missing = 65, IQR_coef = 1.5){

  # First. check to see what kind of input we have and get relevant data
  checkout <- getIn(obj = COINobj, dset = dset, inames = inames, aglev = 1)
  ind_data_only <- checkout$ind_data_only
  ind_names <- checkout$IndCodes

  ##------ Get loads of different stats on indicators. Will be added to a big table at the end.

  imean <- ind_data_only %>% purrr::map_dbl(mean, na.rm = T) # means
  imed <- ind_data_only %>% purrr::map_dbl(stats::median, na.rm = T) # means
  imin <- ind_data_only %>% purrr::map_dbl(min, na.rm = T) # min
  imax <- ind_data_only %>% purrr::map_dbl(max, na.rm = T) # max
  istd <- ind_data_only %>% purrr::map_dbl(stats::sd, na.rm = T) # std
  iskew <- ind_data_only %>% purrr::map_dbl(e1071::skewness, na.rm = T, type = 2) # skew
  ikurt <- ind_data_only %>% purrr::map_dbl(e1071::kurtosis, na.rm = T, type = 2) # kurtosis
  ina <- ind_data_only %>% purrr::map_dbl(~sum(is.na(.x)), na.rm = T) # n. missing
  iprcna <- (1-ina/COINobj$Parameters$NUnit) * 100  # percent available data
  imissflag <- (iprcna < t_missing) %>% dplyr::if_else(true = "Low", false = "OK") # flag if data availability is below threshold
  skflag <- ((abs(iskew)>t_skew) & (ikurt>t_kurt)) %>% dplyr::if_else(true = "Outliers", false = "OK") # flag if exceed both skew and kurt thresholds
  q25 <- ind_data_only %>% purrr::map_dbl(~quantile(.x, probs = 0.25, na.rm = TRUE)) # 25th prc
  q75 <- ind_data_only %>% purrr::map_dbl(~quantile(.x, probs = 0.75, na.rm = TRUE)) # 75th prc
  iIQR <- ind_data_only %>% purrr::map_dbl(stats::IQR, na.rm = TRUE) # interquartile range
  Nunique <- ind_data_only %>% purrr::map_dbl(~{dplyr::n_distinct(.x)/length(.x)})

  # switching some things into a for loop for clarity
  out_flag <- matrix("OK", nrow = nrow(ind_data_only), ncol = ncol(ind_data_only)) # empty df for populating

  for (ii in 1:ncol(ind_data_only)){

    # Populate matrix with high and low outliers, using IQR approach
    icol <- ind_data_only[ii]
    flag_col <- out_flag[,ii]
    flag_col <- replace(flag_col, icol<(q25[ii]-IQR_coef*iIQR[ii]), "Low")
    flag_col <- replace(flag_col, icol>(q75[ii]+IQR_coef*iIQR[ii]), "High")
    out_flag[,ii] <- flag_col

  }
  # now convert to data fame and add column names
  out_flag <- data.frame(out_flag)
  colnames(out_flag) <- ind_names

  # count number of high and low outliers for each variable
  out_low <- out_flag %>% purrr::map_dbl(~sum(.x=="Low", na.rm = TRUE))
  out_high <- out_flag %>% purrr::map_dbl(~sum(.x=="High", na.rm = TRUE)) # interquartile range

  # build indicator stats table. Will add some more columns also below.
  ind_stats <- tibble::tibble(
    Indicator = ind_names,
    Min = imin, Max = imax,
    Mean = imean, Median = imed,
    Q.25 = q25, Q.75 = q75, IQ.range = iIQR,
    Std.dev = istd, Skew = iskew, Kurtosis = ikurt,
    N.missing = ina, Prc.complete = iprcna, Low.data.flag = imissflag, Prc.Unique = Nunique,
    SK.outlier.flag = skflag, Low.Outliers.IQR = out_low, High.Outliers.IQR = out_high
  )



  ##------- Now checking correlations ---------
  # isolate relevant data
  den_data_only <- COINobj$Input$Denominators
  # filter for only unit codes in selected data (in case we have dropped some)
  den_data_only <- den_data_only[den_data_only$UnitCode %in% checkout$UnitCodes,]
  den_data_only <- select(den_data_only, starts_with("Den_"))


  # indicator correlations
  corr_ind <- stats::cor(ind_data_only, method = "pearson", use = "na.or.complete") # get correlation matrix, just indicators
  diag(corr_ind) <- NA # replace 1s with NAs since we are not interested in them
  p_ind <- corrplot::cor.mtest(ind_data_only, method = "pearson") # p values

  # check for collinearity
  maxcor <- sapply(as.data.frame(abs(corr_ind)), max, na.rm = T) # the max absolute correlations
  maxcor <- dplyr::if_else(maxcor>t_colin,"Collinear","OK") # if any values exceed threshold, flag
  ind_stats <- ind_stats %>% tibble::add_column(Collinearity = maxcor) # add to table
  message(paste("Number of collinear indicators = ",sum(maxcor=="Collinear")))

  # check for significant negative correls
  signegs <- purrr::map2_dbl(as.data.frame(corr_ind),as.data.frame(p_ind$p), # loops over correlations and p values simultaneously
           ~ sum(.x<0 & .y<0.05,na.rm = T)) # if corr is negative AND p value below 0.05, count
  ind_stats <- ind_stats %>% tibble::add_column(Neg.Correls = signegs) # add to table
  message(paste("Number of signficant negative indicator correlations = ",sum(signegs)))

  # denominator correlations
  corr_denom <- stats::cor(den_data_only, ind_data_only, method = "pearson", use = "na.or.complete")
  maxcor <- as.data.frame(abs(corr_denom)) %>% purrr::map_dbl(max, na.rm = T) # the max absolute correlations
  maxcor <- dplyr::if_else(maxcor>t_denom,"High","OK") # if any values exceed threshold, flag
  ind_stats <- ind_stats %>% tibble::add_column(Denom.correlation = maxcor) # add to table
  message(paste("Number of indicators with high denominator correlations = ",sum(maxcor=="High")))

  ##---- Write results -----##

  if (is.data.frame(COINobj)){
    return(ind_stats) # if a data.frame was input, return data frame. Otherwise append to COIN object.
  } else {
    eval(parse(text=paste0("COINobj$Analysis$",dset,"$StatTable<- ind_stats")))
    eval(parse(text=paste0("COINobj$Analysis$",dset,"$Outliers<- out_flag")))
    eval(parse(text=paste0("COINobj$Analysis$",dset,"$Correlations<- corr_ind")))
    eval(parse(text=paste0("COINobj$Analysis$",dset,"$DenomCorrelations<- corr_denom")))

    # COINobj$Analysis$outlier_flag <- out_flag
    # COINobj$Analysis$indicator_correlations <- corr_ind
    # COINobj$Analysis$denominator_correlations <- corr_denom
    # add, but referring to correct data set so no confusion what it is
    return(COINobj)
  }

}
