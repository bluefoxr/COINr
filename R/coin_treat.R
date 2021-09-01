#' Treatment of outliers
#'
#' Takes the COIN object and Winsorises indicators where necessary or specified, or reverts to log transform or similar. This is done
#' one indicator at a time.
#'
#' Outliers are identified according to skewness and kurtosis thresholds. The algorithm attempts to reduce the absolute skew and
#' kurtosis by successively Winsorising points up to a specified limit. If this limit is reached, it applies a nonlinear transformation.
#'
#' The process is detailed in the [COINr online documentation](https://bluefoxr.github.io/COINrDoc/data-treatment.html#data-treatment-in-coinr).
#'
#' @param COIN The COIN object
#' @param dset The data set to treat
#' @param winmax The maximum number of points to Winsorise for each indicator. If `NA`, will keep Winsorising until skewness and kurtosis thresholds
#' achieved (but it is likely this will cause errors).
#' @param winchange Logical: if `TRUE` (default), Winsorisation can change direction from one iteration to the next. Otherwise if `FALSE`, no change.
#' @param deflog The type of transformation to apply if Winsorisation fails. If `"log"`, use simple `log(x)` as log transform
#' (note: indicators containing negative values will be skipped). If `"CTlog"`, will do `log(x-min(x) + a)`, where `a <- 0.01*(max(x)-min(x))`, similar to that used in the COIN Tool.
#' If `"CTlog_orig"`, this is exactly the COIN Tool log transformation, which is `log(x-min(x) + 1)`.
#' If `"GIIlog"`, use GII log transformation.
#' If "`boxcox"`, performs a Box-Cox transformation. In this latter case, you should also specify `boxlam`. Finally, if `"none"`, will
#' return the indicator untreated.
#' @param boxlam The lambda parameter of the Box-Cox transform.
#' @param t_skew Absolute skew threshold (default 2)
#' @param t_kurt Kurtosis threshold (default 3.5)
#' @param individual A data frame specifying individual treatment for each indicator, with each row corresponding to one indicator to be treated. Columns are:
#' * `IndCode` The code of the indicator to be treated.
#' * `Treat` The type of treatment to apply, one of `"win"` (Winsorise), `"log"` (log), `"GIIlog"` (GII log), `"CTlog"` (COIN Tool log),
#' `"boxcox"` (Box Cox), or `"None"` (no treatment).
#' * `Winmax` The maximum number of points to Winsorise. Ignored if the corresponding entry in `"Treat"` is not `"win"`.
#' * `Thresh` Either `NA`, which means that Winsorisation will continue up to `winmax` with no checks on skew and kurtosis, or `"thresh"`,
#' which uses the skew and kurtosis thresholds specified in `t_skew` and `t_kurt`.
#' * `boxlam` Lambda parameter for the Box Cox transformation
#' @param indiv_only Logical: if `TRUE`, only the indicators specified in `"individual"` are treated.
#' If `FALSE`, all indicators are treated: any outside of `individual` will get default treatment.
#' @param bypass_all Logical: if `TRUE`, bypasses all data treatment and returns the original data. This
#' is useful for sensitivity analysis and comparing the effects of turning data treatment on and off.
#'
#' @importFrom dplyr pull
#' @importFrom e1071 skewness kurtosis
#' @importFrom tibble add_column
#'
#' @examples
#' # assemble ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # treat raw data set, Winsorise up to a maximum of five points
#' ASEM <- treat(ASEM, dset = "Raw", winmax = 5)
#' # inspect what was done
#' ASEM$Analysis$Treated$TreatSummary
#' # check whether skew and kurtosis now within limits
#' ASEM$Analysis$Treated$StatTable$SK.outlier.flag
#'
#' @seealso
#' * [indDash()] Interactive app for checking indicator distributions. Useful for comparing before/after data treatment.
#'
#' @return If the input is a COIN, outputs an updated COIN with a new treated data set at `.$Data$Treated`, as well as
#' information about the data treatment in `.$Analysis$Treated`. Else if the input is a data frame, outputs both the treated
#' data set and the information about data treatment to a list.
#'
#' @export

treat <- function(COIN, dset = NULL, winmax = NULL, winchange = NULL, deflog = NULL, boxlam = NULL,
                       t_skew = NULL, t_kurt = NULL, individual = NULL, indiv_only = NULL, bypass_all = NULL){

  # Check for dset. If not specified, exit.
  if (is.null(dset) & !("data.frame" %in% class(COIN))){
    stop("dset is NULL. Please specify which data set to operate on.")
  }

  ##----- SET DEFAULTS -------##
  # Done here because otherwise if we use regen, this input could be input as NULL
  if(is.null(winchange)){
    winchange <- TRUE
  }
  if(is.null(deflog)){
    deflog <- "CTlog"
  }
  if(is.null(t_skew)){
    t_skew <- 2
  }
  if(is.null(t_kurt)){
    t_kurt <- 3.5
  }
  if(is.null(indiv_only)){
    indiv_only <- TRUE
  }
  if(is.null(bypass_all)){
    bypass_all <- FALSE
  }

  # First check object type and extract
  out <- getIn(COIN, dset = dset)

  if (out$otype == "COINobj"){
    # write function arguments to Method
    COIN$Method$treat$dset <- dset
    COIN$Method$treat$winmax <- winmax
    COIN$Method$treat$winchange <- winchange
    COIN$Method$treat$deflog <- deflog
    COIN$Method$treat$boxlam <- boxlam
    COIN$Method$treat$t_skew <- t_skew
    COIN$Method$treat$t_kurt <- t_kurt
    COIN$Method$treat$individual <- individual
    COIN$Method$treat$indiv_only <- indiv_only
    COIN$Method$treat$bypass_all <- bypass_all
  }

  # if winmax not specified, default to 10% of units, rounded up
  if(is.null(winmax)){
    winmax <- ceiling((length(out$UnitCodes)*0.1))
  }

  # get basic data sets
  ind_data <- out$ind_data # all indicator data
  ind_data_only <- out$ind_data_only
  IndCodes <- out$IndCodes

  ###---- Bypass everything if requested -----##

  if(bypass_all){
    if (out$otype == "COINobj"){
      # write results
      COIN$Data$Treated <- ind_data
      COIN$Analysis$Treated$TreatSummary <- data.frame(Treatment = "Treatment bypassed")
      COIN$Analysis$Treated$TreatFlags <- NULL
      return(COIN)
    } else {
      # if input was a data frame, output a list
      fout <- list(
        DataTreated = ind_data,
        TreatmentSummary = data.frame(Treatment = "Treatment bypassed")
      )
      return(fout)
    }
  }

  ind_data_treated <- ind_data # make a copy, for treated data
  treat_flag <- matrix("No", nrow = nrow(ind_data_only), ncol = ncol(ind_data_only)) # empty matrix for populating
  Treatment <- matrix(NA, nrow = ncol(ind_data_only), 1) # this will summarise the data treatment for each indicator
  TreatSpec <- matrix(NA, nrow = ncol(ind_data_only), 1) # record of what treatment was specified

  ###----------- DEFAULT INDICATOR TREATMENT -------------------------

  if (is.null(individual)){ # means that all indicators follow the same default treatment process

    # looping over indicators (columns)
    for (ii in 1:ncol(ind_data_only)){

      icol <- dplyr::pull(ind_data_only,ii) # get relevant column
      icode <- colnames(ind_data_only)[ii]

      w <- coin_win(icol, winmax, winchange, t_skew, t_kurt, icode)

      # test skew and kurtosis again
      sk <- e1071::skewness(w$icol, na.rm = T, type = 2)
      kt <- e1071::kurtosis(w$icol, na.rm = T, type = 2)

      # Here, loop may have exited because treatment succeeded, or reached winmax. Let's check
      if ( (w$winz >= winmax)  &  ((abs(sk)>t_skew) & (kt>t_kurt)) ){ # didn't work

        # do log-type transformation
        params <- list(winmax = winmax, IndCodes = IndCodes, ii = ii, boxlam = boxlam,
                       forced = FALSE)
        logout <- loggish(icol, deflog, params)
        # record outputs
        icol <- logout$x # the transformed values
        treat_flag[,ii] <- logout$Flag
        Treatment[ii] <- logout$Treatment
        TreatSpec[ii] <- logout$TreatSpec

      } else { # Winsorization DID work
        treat_flag[w$imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
        treat_flag[w$imin,ii] <- "WLow"
        icol <- w$icol
        if (w$winz>0){
          Treatment[ii] <- paste0("Winsorised ", w$winz, " points")}
        else {Treatment[ii] <- "None"}
        TreatSpec[ii] <- paste0("Default, winmax = ", winmax)
      }

      ind_data_treated[IndCodes[ii]]<-icol # subst treated col into treated data set
    }

    ###------ INDIVIDUAL INDICATOR TREATMENT -----

  } else {

    # looping over indicators (columns)
    for (ii in 1:ncol(ind_data_only)){

      icol <- dplyr::pull(ind_data_only,ii) # get relevant column
      ind_name <- IndCodes[ii] # get indicator name

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
          } else {
            warning("Threshold type (in individual$Thresh) not recognised, using global values.")
            # use the global threshold
            t_skewi <- t_skew
            t_kurti <- t_kurt
          }

          w <- coin_win(icol, winmaxii, winchange, t_skewi, t_kurti, ind_name)

          # loop exited, we don't know if we succeeded or not and don't go to log

          treat_flag[w$imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
          treat_flag[w$imin,ii] <- "WLow"
          if ( is.na(individual$Thresh[individual$IndCode==ind_name]) ){
            TreatSpec[ii] <- paste0("Forced Win (no thresh), winmax = ", winmaxii)
          } else {
            TreatSpec[ii] <- paste0("Forced Win, winmax = ", winmaxii)
          }
          TreatSpec[ii] <- paste0("Forced Win, winmax = ", winmaxii)
          if (w$winz>0){
            Treatment[ii] <- paste0("Winsorised ", w$winz, " points")}
          else {Treatment[ii] <- "None"}

          ind_data_treated[IndCodes[ii]]<-w$icol # subst treated col into treated data set

        } else {

          icol <- dplyr::pull(ind_data_only,ii) # get fresh version of column

          # do log-type transformation
          params <- list(winmax = individual$Winmax[individual$IndCode==ind_name], IndCodes = IndCodes, ii = ii,
                         boxlam = individual$Boxlam[individual$IndCode==ind_name], forced = TRUE)

          logout <- loggish(icol, individual$Treat[individual$IndCode==ind_name], params)
          # record outputs
          ind_data_treated[IndCodes[ii]] <- logout$x # the transformed values
          treat_flag[,ii] <- logout$Flag
          Treatment[ii] <- logout$Treatment
          TreatSpec[ii] <- logout$TreatSpec

        }

      } else if (indiv_only == FALSE){
        # here means that indicator is NOT specified in the individual table, and
        # the indiv_only flag is set so that all other indicators should be treated by default process
        # So, applying default process to this one.

        w <- coin_win(icol, winmax, winchange, t_skew, t_kurt, ind_name)

        sk <- e1071::skewness(w$icol, na.rm = T, type = 2)
        kt <- e1071::kurtosis(w$icol, na.rm = T, type = 2)

        # Here, loop may have exited because treatment succeeded, or reached winmax. Let's check
        if ( (w$winz >= winmax)  &  ((abs(sk)>t_skew) & (kt>t_kurt)) ){ # didn't work

          icol <- dplyr::pull(ind_data_only,ii) # get fresh version of column

          # do log-type transformation
          params <- list(winmax = winmax, IndCodes = IndCodes, ii = ii, boxlam = boxlam,
                         forced = FALSE)
          logout <- loggish(icol, deflog, params)
          # record outputs
          icol <- logout$x # the transformed values
          treat_flag[,ii] <- logout$Flag
          Treatment[ii] <- logout$Treatment
          TreatSpec[ii] <- logout$TreatSpec

        } else { # Winsorization DID work
          treat_flag[w$imax,ii] <- "WHigh" # Flag Winsorisation (if NULL, will not assign anything)
          treat_flag[w$imin,ii] <- "WLow"
          icol <- w$icol
          if (w$winz>0){
            Treatment[ii] <- paste0("Winsorised ", w$winz, " points")}
          else {Treatment[ii] <- "None"}
          TreatSpec[ii] <- paste0("Default, winmax = ", winmax)

        }

        ind_data_treated[IndCodes[ii]]<-icol # subst treated col into treated data set

      } # end of if indicator in individual table
    } # end indicator loop
  } # end IF indicator individual treatment

  # tidy up a bit

  Treatment[is.na(Treatment)] <- "None"
  TreatSpec[is.na(TreatSpec)] <- "None"

  ntreated <- data.frame(
    IndCode = IndCodes,
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
    # write results
    COIN$Data$Treated <- ind_data_treated
    COIN$Analysis$Treated$TreatSummary <- ntreated
    COIN$Analysis$Treated$TreatFlags <- treat_flag
    return(COIN)
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


#' Winsorisation helper function
#'
#' To be used inside [treat()] to avoid repetitions. Winsorises a numerical vector of data.
#'
#' Outliers are identified according to skewness and kurtosis thresholds. The algorithm attempts to reduce the absolute skew and
#' kurtosis by successively Winsorising points up to a specified limit.
#'
#' The process is detailed in the [COINr online documentation](https://bluefoxr.github.io/COINrDoc/data-treatment.html#data-treatment-in-coinr).
#'
#' @param icol The vector of data to Winsorise
#' @param winmax The maximum number of points to Winsorise for each indicator. If `NA`, will keep Winsorising until skewness and kurtosis
#' thresholds achieved (but it is likely this will cause errors).
#' @param winchange Logical: if `TRUE`, Winsorisation can change direction from one iteration to the next. Otherwise if `FALSE` (default), no change.
#' @param t_skew Absolute skew threshold (default 2).
#' @param t_kurt Kurtosis threshold (default 3.5).
#' @param icode The indicator name - used for error messages in [treat()].
#'
#' @examples
#' # get a column of data with outliers
#' x <- ASEMIndData$Tariff
#' # Winsorise up to five points
#' winlist <- coin_win(x, winmax = 5)
#' # check the differences
#' data.frame(
#' Orig = x,
#' Treated = winlist$icol,
#' Changes = ifelse(x == winlist$icol, "Same", "Treated"))
#'
#' @seealso
#' * [treat()] Outlier treatment
#'
#' @return A list containing:
#' * `.$icol` the vector of treated data
#' * `.$imax` the indices of elements of the vector that were Winsorised as high values
#' * `.$imax` the indices of elements of the vector that were Winsorised as low values
#' * `.$winz` the total number of Winsorised points
#'
#' @export

coin_win <- function(icol, winmax, winchange = TRUE, t_skew = 2, t_kurt = 3.5, icode = NULL){

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
    if(length(unique(icol)) < 2){
      stop(paste0("Can't Winsorise further because it would imply less than two unique values in the indicator.
      This is probably not a good idea. Consider individual settings for this indicator, such as
      a lower winmax, using a transformation by default, or excluding from treatment.
      **INDICATOR = ",icode,"**"))
    } else {
      sk <- e1071::skewness(icol, na.rm = T, type = 2)
      kt <- e1071::kurtosis(icol, na.rm = T, type = 2)
    }
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

#' Box Cox transformation
#'
#' Simple Box Cox, with no optimisation of lambda.
#' See [COINr online documentation](https://bluefoxr.github.io/COINrDoc/data-treatment.html#transformation) for more details.
#'
#' @param x A vector or column of data to transform
#' @param lambda The lambda parameter of the Box Cox transform
#' @param makepos If `TRUE` (default) makes all values positive by subtracting the minimum and adding 1.
#'
#' @examples
#' # get a column of data with outliers
#' x <- ASEMIndData$Tariff
#' # Apply Box Cox
#' xBox <- BoxCox(x, lambda = 2)
#' # plot one against the other
#' plot(x, xBox)
#'
#' @seealso
#' * [treat()] Outlier treatment
#'
#' @return A vector of length `length(x)` with transformed values.
#'
#' @export

BoxCox <- function(x, lambda, makepos = TRUE){

  if(makepos){
    # make positive using COIN Tool style shift
    x <- x - min(x,na.rm = T) + 1
  }

  # Box Cox
  if (lambda==0){
    x <- log(x)
  } else {
    x <- (x^lambda - 1)/lambda
  }

  return(x)

}

#' Log-type transformation of a vector
#'
#' This applies various simple transformations, to be used by the [treat()] function.
#' This function is probably not very useful on its own because it requires `params`,
#' a list of parameters which are used to output the type and status of treatment applied.
#'
#' @param x A vector or column of data to transform
#' @param ltype The type of log transformation - see `deflog` in [treat()].
#' @param params Some extra parameters to pass. These parameters mostly concern internal messages for [treat()] and this can be
#' left unspecified unless `ltype == "boxcox"`, in which case there should be a parameter `params$boxlam` specified
#' (see [BoxCox()]). However, if you wish to use a Box Cox transformation, it is better to use [BoxCox()] directly.
#'
#' @examples
#' # get a column of data with outliers
#' x <- ASEMIndData$Tariff
#' # apply a GII transformation
#' xdash <- loggish(x, ltype = "GIIlog")
#' # plot one against the other
#' plot(x, xdash$x)
#'
#' @seealso
#' * [treat()] Outlier treatment
#'
#' @return A list with
#' * `.$x` is the transformed vector of data
#' * `.$Flag` is a flag of the type of treatment specified (used inside [treat()])
#' * `.$Treatment` the treatment applied (used inside [treat()])
#' * `.$TreatSpec` the treatment specified (used inside [treat()])
#'
#' @export

loggish <- function(x, ltype, params = NULL){

  # some default parameters if this function is used on its own
  if(is.null(params)){
    params = list(IndCodes = "Indicator",
                  ii = 1,
                  forced = FALSE,
                  winmax = "unspecified",
                  boxlam = 1)
  } else if (!is.null(params$boxlam)){
    params$IndCodes = "Indicator"
    params$ii = 1
    params$forced = FALSE
    params$winmax = "unspecified"
  }

  # prep a list
  l <- list(x = NA, Flag = NA, Treatment = NA, TreatSpec = NA)

  if ( (sum(x<=0, na.rm=T)>0) & (ltype == "log") ){ # negative values. No can normal log.

    l$Flag <- "Err"
    # x will be passed through with no treatment
    l$x <- x
    warning(paste0(params$IndCodes[params$ii],": log transform attempted but failed because negative or zero values. Please check."))
    if(params$forced){
      l$TreatSpec <- paste0("Forced log")
      l$Treatment <- "None: log error"
    } else {
      l$TreatSpec <- paste0("Default, winmax = ", params$winmax)
      l$Treatment <- "None: exceeded winmax but log error"
    }

  } else if ( (sum(x<=0, na.rm=T)==0) & (ltype == "log") ) { # OK to normal log

    l$x <- log(x)
    l$Flag <- "Log"
    if(params$forced){
      l$TreatSpec <- paste0("Forced log")
      l$Treatment <- "Log"
    } else {
      l$TreatSpec <- paste0("Default, winmax = ", params$winmax)
      l$Treatment <- "Log (exceeded winmax)"
    }

  } else if (ltype == "GIIlog"){ # GII log

    # get GII log
    l$x <- log( (max(x, na.rm = T)-1)*(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) + 1 )
    l$Flag <- "GIILog"
    if(params$forced){
      l$TreatSpec <- paste0("Forced GIIlog")
      l$Treatment <- "GIILog"
    } else {
      l$TreatSpec <- paste0("Default, winmax = ", params$winmax)
      l$Treatment <- "GIILog (exceeded winmax)"
    }

  } else if (ltype == "CTlog"){
    # COIN TOOl style log: subtract min and add fraction of range
    l$x <- log(x- min(x,na.rm = T) + 0.01*(max(x, na.rm = T)-min(x, na.rm = T)))
    l$Flag <- "CTLog"
    if(params$forced){
      l$TreatSpec <- paste0("Forced CTlog")
      l$Treatment <- "CTLog"
    } else {
      l$TreatSpec <- paste0("Default, winmax = ", params$winmax)
      l$Treatment <- "CTLog (exceeded winmax)"
    }

  } else if (ltype == "CTlog_orig"){
    # COIN TOOl original log: subtract min and add 1
    l$x <- log(x- min(x,na.rm = T) + 1)
    l$Flag <- "CTlog_orig"
    if(params$forced){
      l$TreatSpec <- paste0("Forced CTlog_orig")
      l$Treatment <- "CTlog_orig"
    } else {
      l$TreatSpec <- paste0("Default, winmax = ", params$winmax)
      l$Treatment <- "CTlog_orig (exceeded winmax)"
    }

  } else if (ltype == "boxcox"){
    # Box Cox transform
    l$x <- BoxCox(x,params$boxlam)
    l$Flag <- "BoxCox"
    if(params$forced){
      l$TreatSpec <- paste0("Forced Box-Cox")
      l$Treatment <- paste0("Box Cox with lambda = ",params$boxlam)
    } else {
      l$TreatSpec <- paste0("Default, winmax = ", params$winmax)
      l$Treatment <- paste0("Box Cox with lambda = ",params$boxlam," (exceeded winmax)")
    }

  } else if (ltype == "None"){

    # No transform
    l$x <- x
    # this indicator should be excluded from any treatment
    l$Flag <- "ForcedNo"
    l$Treatment <- "ForcedNone"
    l$TreatSpec <- "None"
  }

  return(l)

}
