#' Treatment of outliers
#'
#' Takes the COIN object and Winsorises indicators where necessary or specified, or reverts to log transform.
#'
#' @param COINobj The COIN object
#' @param dset The data set to treat
#' @param winmax The maximum number of points to Winsorise for each indicator. If NA, will keep Winsorising until skew&kurt thresholds achieved (but it is likely this will cause errors)
#' @param winchange Logical: if TRUE, Winsorisation can change direction from one iteration to the next. Otherwise if FALSE (default), no change.
#' @param deflog If "log", use simple ln(x) as log transform (note: indicators containing negative values
#' will be skipped). IF "CTlog", will do ln(x-min(x)+1), as used in the COIN Tool. If "GIIlog", use GII log transformation.
#' @param t_skew Absolute skew threshold (default 2)
#' @param t_kurt Kurtosis threshold (default 3.5)
#' @param individual A data frame specifying individual treatment for each indicator
#' @param indiv_only Logical: if TRUE, only the indicators specified in "individual" are treated.
#' If false, all indicators are treated: any outside of "individual" will get default treatment.
#'
#' @importFrom dplyr pull
#' @importFrom e1071 skewness kurtosis
#' @importFrom tibble add_column
#'
#' @return A treated data set plus information about how the data was treated.
#'
#' @export
#'

coin_treat <- function(COINobj, dset = "Raw", winmax = NULL, winchange = FALSE, deflog = "log",
                       t_skew = 2, t_kurt = 3.5, individual = NULL, indiv_only = TRUE){

  # First check object type and extract
  out <- coin_aux_objcheck(COINobj, dset = dset)

  # if winmax not specified, default to 10% of units, rounded up
  if(is.null(winmax)){
    winmax <- (COINobj$Parameters$NUnit*0.1) %>% ceiling()
  }

  # get basic data sets
  ind_data <- out$ind_data # all indicator data
  ind_data_only <- out$ind_data_only
  ind_names <- out$ind_names

  ind_data_treated <- ind_data # make a copy, for treated data
  treat_flag <- matrix("No", nrow = nrow(ind_data_only), ncol = ncol(ind_data_only)) # empty matrix for populating
  Treatment <- matrix(NA, nrow = ncol(ind_data_only), 1) # this will summarise the data treatment for each indicator
  TreatSpec <- matrix(NA, nrow = ncol(ind_data_only), 1) # record of what treatment was specified

  ###----------- DEFAULT INDICATOR TREATMENT -------------------------

  if (is.null(individual)){ # means that all indicators follow the same default treatment process

    # looping over indicators (columns)
    for (ii in 1:ncol(ind_data_only)){

      icol <- dplyr::pull(ind_data_only,ii) # get relevant column

      w <- coin_win(icol, winmax, winchange, t_skew, t_kurt)

      # test skew and kurtosis again
      sk <- e1071::skewness(w$icol, na.rm = T, type = 2)
      kt <- e1071::kurtosis(w$icol, na.rm = T, type = 2)

      # Here, loop may have exited because treatment succeeded, or reached winmax. Let's check
      if ( (w$winz >= winmax)  &  ((abs(sk)>t_skew) & (kt>t_kurt)) ){ # didn't work

        if ( (sum(icol<=0, na.rm=T)>0) & (deflog == "log") ){ # negative values. No can normal log.

          treat_flag[,ii] <- "Err"
          # icol will be passed through with no treatment
          warning(paste0(ind_names[ii],": indicator exceeded max winsorisation but cannot do log transform because negative or zero values. Please check."))
          Treatment[ii] <- "None: exceeded winmax but log error"
          TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

        } else if ( (sum(icol<=0, na.rm=T)==0) & (deflog == "log") ) { # OK to normal log

          icol <- log(icol)
          treat_flag[,ii] <- "Log"
          Treatment[ii] <- "Log (exceeded winmax)"
          TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

        } else if (deflog == "GIIlog"){ # GII log

          # get GII log
          icol <- log( (max(icol, na.rm = T)-1)*(icol-min(icol, na.rm = T))/(max(icol, na.rm = T)-min(icol, na.rm = T)) + 1 )
          treat_flag[,ii] <- "GIILog"
          Treatment[ii] <- "GIILog (exceeded winmax)"
          TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

        } else if (deflog == "CTlog"){
          # COIN TOOl style log: subtract min and add 1
          icol <- log(icol- min(icol,na.rm = T) + 1)
          treat_flag[,ii] <- "CTLog"
          Treatment[ii] <- "CTLog (exceeded winmax)"
          TreatSpec[ii] <- paste0("Default, winmax = ", winmax)
        }

      } else { # Winsorization DID work
        treat_flag[w$imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
        treat_flag[w$imin,ii] <- "WLow"
        icol <- w$icol
        if (w$winz>0){
          Treatment[ii] <- paste0("Winsorised ", w$winz, " points")}
        else {Treatment[ii] <- "None"}
        TreatSpec[ii] <- paste0("Default, winmax = ", winmax)
      }

      ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set
    }

    ###------ INDIVIDUAL INDICATOR TREATMENT -----

  } else {

    # looping over indicators (columns)
    for (ii in 1:ncol(ind_data_only)){

      icol <- dplyr::pull(ind_data_only,ii) # get relevant column
      ind_name <- ind_names[ii] # get indicator name

      # check if this indicator is specified in the "individual" table
      # if it is, we treat it as specified in the table
      if (ind_name %in% individual$IndCode) {

        # INDIVIDUAL TREATMENT

        # Now check which kind of treatment to apply using table
        if( individual$Treat[individual$IndCode==ind_name] == "win"){
          # this indicator should be winsorised

          # get the winmax for this indicator from the table
          winmaxii <- individual$Winmax[individual$IndCode==ind_name]

          # get skew and kurtosis threshold to use
          if ( is.na(individual$Thresh[individual$IndCode==ind_name]) ) {
            # NA implies to keep Winsorising up to winmax. To do this, set thresholds to zero (cannot be reached)
            t_skewi <- 0
            t_kurti <- 0
          } else if ( individual$Thresh[individual$IndCode==ind_name] == "thresh"){
            # use the global threshold
            t_skewi <- t_skew
            t_kurti <- t_kurt
          }

          w <- coin_win(icol, winmaxii, winchange, t_skewi, t_kurti)

          # loop exited, we don't know if we succeeded or not and don't go to log

          treat_flag[w$imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
          treat_flag[w$imin,ii] <- "WLow"
          if ( is.na(individual$Thresh[individual$IndCode==ind_name]) ){
            TreatSpec <- paste0("Forced Win (no thresh), winmax = ", winmaxii)
          } else {
            TreatSpec <- paste0("Forced Win, winmax = ", winmaxii)
          }
          TreatSpec <- paste0("Forced Win, winmax = ", winmaxii)
          if (w$winz>0){
            Treatment[ii] <- paste0("Winsorised ", w$winz, " points")}
          else {Treatment[ii] <- "None"}

          ind_data_treated[ind_names[ii]]<-w$icol # subst treated col into treated data set

        } else if ( individual$Treat[individual$IndCode==ind_name] == "log"){
          # this indicator should be log transformed

          icol <- dplyr::pull(ind_data_only,ii) # get fresh version of column

          if (sum(icol<=0, na.rm=T)>0){ # negative values. No can log.

            treat_flag[,ii] <- "Err"
            # icol will be passed through with no treatment
            warning(paste0(ind_names[ii],": cannot do log transform because negative or zero values. Please check."))
            TreatSpec[ii] <- "Forced log"
            Treatment[ii] <- "None: log error."

          } else { # OK to log

            icol <- log(icol)
            treat_flag[,ii] <- "Log"
            TreatSpec[ii] <- "Forced log"
            Treatment[ii] <- "Log"

          }
          ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set

          # END INDIVIDUAL LOG TRANSFORM

        } else if ( individual$Treat[individual$IndCode==ind_name] == "GIIlog"){

          # get GII log
          icol <- log( (max(icol, na.rm = T)-1)*(icol-min(icol, na.rm = T))/(max(icol, na.rm = T)-min(icol, na.rm = T)) + 1 )
          treat_flag[,ii] <- "GIILog"
          Treatment[ii] <- "GIILog"
          TreatSpec[ii] <- "Forced GIILog"
          ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set

        } else if ( individual$Treat[individual$IndCode==ind_name] == "none"){

          # this indicator should be excluded from any treatment
          treat_flag[,ii] <- "ForcedNo"
          TreatSpec[ii] <- "ForcedNone"
          Treatment[ii] <- "None"

        }

      } else if (indiv_only == FALSE){
        # here means that indicator is NOT specified in the individual table, and
        # the indiv_only flag is set so that all other indicators should be treated by default process
        # So, applying default process to this one.

        w <- coin_win(icol, winmax, winchange, t_skew, t_kurt)

        sk <- e1071::skewness(w$icol, na.rm = T, type = 2)
        kt <- e1071::kurtosis(w$icol, na.rm = T, type = 2)

        # Here, loop may have exited because treatment succeeded, or reached winmax. Let's check
        if ( (w$winz >= winmax)  &  ((abs(sk)>t_skew) & (kt>t_kurt)) ){ # didn't work

          icol <- dplyr::pull(ind_data_only,ii) # get fresh version of column

          if ( (sum(icol<=0, na.rm=T)>0) & (deflog == "log") ){ # negative values. No can normal log.

            treat_flag[,ii] <- "Err"
            # icol will be passed through with no treatment
            warning(paste0(ind_names[ii],": indicator exceeded max winsorisation but cannot do log transform because negative or zero values. Please check."))
            Treatment[ii] <- "None: exceeded winmax but log error"
            TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

          } else if ( (sum(icol<=0, na.rm=T)==0) & (deflog == "log") ) { # OK to normal log

            icol <- log(icol)
            treat_flag[,ii] <- "Log"
            Treatment[ii] <- "Log (exceeded winmax)"
            TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

          } else if (deflog == "GIIlog"){ # GII log

            # get GII log
            icol <- log( (max(icol, na.rm = T)-1)*(icol-min(icol, na.rm = T))/(max(icol, na.rm = T)-min(icol, na.rm = T)) + 1 )
            treat_flag[,ii] <- "GIILog"
            Treatment[ii] <- "GIILog (exceeded winmax)"
            TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

          } else if (deflog == "CTlog"){
            # COIN TOOl style log: subtract min and add 1
            icol <- log(icol- min(icol,na.rm = T) + 1)
            treat_flag[,ii] <- "CTLog"
            Treatment[ii] <- "CTLog (exceeded winmax)"
            TreatSpec[ii] <- paste0("Default, winmax = ", winmax)
          }

        } else { # Winsorization DID work
          treat_flag[w$imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
          treat_flag[w$imin,ii] <- "WLow"
          icol <- w$icol
          if (w$winz>0){
            Treatment[ii] <- paste0("Winsorised ", w$winz, " points")}
          else {Treatment[ii] <- "None"}
          TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

        }

        ind_data_treated[ind_names[ii]]<-icol # subst treated col into treated data set

      } # end of if indicator in individual table
    } # end indicator loop
  } # end IF indicator individual treatment

  # tidy up a bit

  ntreated <- data.frame(
    IndCode = ind_names,
    Low = map_dbl(as.data.frame(treat_flag), ~sum(.x=="WLow")),
    High = map_dbl(as.data.frame(treat_flag), ~sum(.x=="WHigh")),
    TreatSpec = TreatSpec,
    Treatment = Treatment
  )

  colnames(treat_flag) <- colnames(ind_data_only)
  treat_flag <- as.data.frame(treat_flag)
  treat_flag <- treat_flag %>%
    tibble::add_column(UnitCode = out$UnitCodes, .before = 1)

  ###---- Write results and method -----##

  if (out$otype == "COINobj"){
    # write function arguments to Method
    COINobj$Method$Treatment$winmax <- winmax
    COINobj$Method$Treatment$winchange <- winchange
    COINobj$Method$Treatment$deflog <- deflog
    COINobj$Method$Treatment$t_skew <- t_skew
    COINobj$Method$Treatment$t_kurt <- t_kurt
    COINobj$Method$Treatment$individual <- individual
    COINobj$Method$Treatment$indiv_only <- indiv_only

    # write results
    COINobj$Data$Treated <- ind_data_treated
    COINobj$Analysis$Treatment$Summary <- ntreated
    COINobj$Analysis$Treatment$Flags <- treat_flag
    return(COINobj)
  } else {
    # if input was a data frame, output a list
    fout <- list(
      DataTreated <- ind_data_treated,
      TreatmentSummary <- ntreated,
      TreatmentFlags <- treat_flag
    )
    return(fout)
  }
}


##### -------- WINZ FUNCTION (HELPER) ---- #####

#' Winsorisation helper function
#'
#' To be used inside COIN_treat to avoid repetitions
#'
#' @param icol The vector of data to Winsorize
#' @param winmax The maximum number of points to Winsorise for each indicator. If NA, will keep Winsorising until skew&kurt thresholds achieved (but it is likely this will cause errors)
#' @param winchange Logical: if TRUE, Winsorisation can change direction from one iteration to the next. Otherwise if FALSE (default), no change.
#' @param t_skew Absolute skew threshold (default 2)
#' @param t_kurt Kurtosis threshold (default 3.5)
#'
#' @export

coin_win <- function(icol, winmax, winchange, t_skew, t_kurt){

  # first, check skew and kurtosis
  sk <- e1071::skewness(icol, na.rm = T, type = 2)
  kt <- e1071::kurtosis(icol, na.rm = T, type = 2)

  # now make a flag to either always do high Winsorisation, or always low, depending on sk
  if (winchange==F){ # if F, we should always Winsorise from the same direction
    if (sk>0){
      windir <- 1 # always high
    } else {
      windir <- -1 # always low
    }
  } else { windir <- 0} # can change from high to low (winchange == T)

  winz<-0 # set counter to 0
  imax<-imin<-NULL # reset to NULL

  if (is.na(winmax)){winmax <- 1e6}

  while ( ((abs(sk)>t_skew) & (kt>t_kurt)) & (winz < winmax) ) { # keep going until sk and kt below thresholds OR reached winsorisation limit

    # high winz if high skew AND windir =1, OR windir = 1 (always use high)
    if ((sk>=0 & windir==0) | (windir == 1)){ # skew is positive, implies high outliers

      imax <- which(icol==max(icol, na.rm = T)) # position(s) of maximum value(s)
      icol[imax] <- max(icol[-imax], na.rm = T) # replace imax with max value of indicator if imax value(s) excluded

    } else { # skew is negative, implies low outliers

      imin <- which(icol==min(icol, na.rm = T)) # ditto, but with min
      icol[imin] <- min(icol[-imin], na.rm = T)
    }

    winz<-winz+1 # add the winsorisation counter

    # test skew and kurtosis again
    sk <- e1071::skewness(icol, na.rm = T, type = 2)
    kt <- e1071::kurtosis(icol, na.rm = T, type = 2)

  }

  # write outputs
  w <- list(
    icol = icol,
    imax = imax,
    imin = imin,
    winz = winz
  )
  return(w)
}
