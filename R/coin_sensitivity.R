#' Sensitivity analysis
#'
#' Performs global uncertainty and sensitivity analysis on a COIN.
#'
#' To perform a sensitivity or uncertainty analysis, you must specify *which* parameters/assumptions to vary and *what* their alternative
#' values are. This is the `SA_specs` argument below. To understand how this works, please see the [COINr online documentation](https://bluefoxr.github.io/COINrDoc/sensitivity-analysis.html#sensitivity-in-coinr).
#'
#' @param COIN A COIN (this function does not support data frame input)
#' @param v_targ The target variable to perform SA or UA on. Currently just supports one variable, which
#' should be an indicator/aggregate code present in .$Data$Aggregated.
#' @param SA_specs A list which specifies which variables to perturb, and which alternatives/distributions to use
#' @param N The number of Monte Carlo replications
#' @param SA_type The type of analysis to run. "UA" runs an uncertainty analysis.
#' @param NrepWeights The number of weight-replications to generate. Default 1000.
#' @param store_results Which results to store
#'   * "onlyresults" only stores scores, ranks and rank statistics (e.g. mean, median, quantiles)
#'   * "results+params" (default) stores all of the above, plus a record of the parameter values used for each replication
#'   * "results+method" stores all of the above, plus the full .$Method list of each replication
#'   * "results+COIN" stores all results and the complete COIN of each replication
#' @param Nboot Number of bootstrap draws for estimates of confidence intervals on sensitivity indices.
#' If this is not specified, bootstrapping is not applied. Ignored if SA_type = "UA".
#' @param quietly If FALSE (default), gives progress messages. Set TRUE to suppress these.
#'
#' @importFrom purrr flatten
#' @importFrom stats median quantile runif
#'
#' @examples \dontrun{
#'
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#'
#' # define noise to be applied to weights
#' nspecs <- data.frame(AgLevel = c(2,3), NoiseFactor = c(0.25,0.25))
#'
#' # create list specifying assumptions to vary and alternatives
#' SAspecs <- list(
#'   impute = list(imtype = c("indgroup_mean", "ind_mean", "none")),
#'   normalise = list(ntype = c("minmax", "rank", "dist2max")),
#'   weights = list(NoiseSpecs = nspecs, Nominal = "Original")
#' )
#'
#' # run uncertainty analysis
#' SAresults <- sensitivity(ASEM, v_targ = "Index",
#'                          SA_specs = SAspecs,
#'                          N = 100,
#'                          SA_type = "UA")
#'
#' # to run a sensitivity analysis set SA_type = "SA" (takes longer)}
#'
#' @seealso
#' * [plotSARanks()] Plot confidence intervals of ranks following UA or SA
#' * [plotSA()] Plot sensitivity indices following a sensitivity analysis
#'
#' @return Sensitivity analysis results
#'
#' @export

sensitivity <- function(COIN, v_targ, SA_specs, N, SA_type = "UA", NrepWeights = 1000,
                        store_results = "results+params", Nboot = NULL, quietly = FALSE){

  # to add:
  # - extend to multiple target variables
  # - continuous distribution support

  ### REBUILD FUNCTION ### --------------------------------------------------------
  # We will need a function which rebuilds the index according to specifications in the SA_specs list.

  t0 <- proc.time()

  COIN_rebuild <- function(x, store_results){

    if (!quietly){
      message(paste0("Iteration ",ii," of ",nrow(XX)," ... ", round(ii*100/nrow(XX),1), "% complete" ))
    }

    # make a fresh copy of the COIN
    iCOIN <- COIN
    # make a vector for storing parameter values
    vpara <- matrix(NA, nrow = 1, ncol = npara_noweights) %>% as.data.frame()
    colnames(vpara) <- paranames

    # variable counter (makes indexing easier in a hierarchical list)
    pcount <- 1
    # flag whether weights have already been counted (because they are separate from the df that records parameter values)
    wflag <- 0

    # first, loop over the construction functions contained in the list
    for (ifunc in 1:length(SA_specs)){


      # function name
      fname <- names(SA_specs)[ifunc]

      # If it something other than weights, we directly edit the Method folder
      if (fname != "weights"){

        # get list of parameters to perturb for this function
        plist <- SA_specs[[ifunc]]

        # now loop over parameters
        for (iparam in 1:length(plist)){

          # parameter name
          pname <- names(plist)[iparam]

          # get the alternative parameter values
          #pvals <- unlist(plist[[iparam]])
          pvals <- plist[[iparam]]

          # we now have to alter the parameter
          # First, get the parameter index according to x
          ix <- ceiling(x[pcount]*length(pvals))

          # Now, select the parameter value
          ipval <- pvals[[ix]]

          # Update the Method
          iCOIN$Method[[fname]][[pname]] <- ipval

          # also store value in vector (for now, I just collapsed any vectors into one)
          # here I have to account for the fact that any weights variable may or may not
          # have passed. Do this using the wflag variable (see elsewhere in function)
          if(wflag==0){
            vpara[pcount] <- paste(ipval, collapse = ",")
          } else if (wflag == 1){
            vpara[pcount-1] <- paste(ipval, collapse = ",")
          }

          # Update the counter (for recording parameter index)
          pcount <- pcount + 1

        } # end parameter loop

      } else {
        # If we get here, this means that we have to perturb the weights
        # first, convert continuous x to discrete to select a set of weights
        ix <- ceiling(x[pcount]*NrepWeights)
        # store selected weights in object
        iCOIN$Parameters$Weights$Randomised <- wlist[[ix]]
        # update Method to point at randomised weights
        iCOIN$Method$aggregate$agweights <- "Randomised"
        # add to counter
        pcount <- pcount + 1
        # flag that weights have been counted
        wflag <- 1
      } # end if weights

    } # end function loop

    # regenerate the results
    iCOINr <- tryCatch(
      expr = {
        regen(iCOIN, quietly = TRUE)
      },
      error = function(e){
        message("Regen failed. Probably a conflict between methods.")
        print(e)
        return(NA)
      }
    )

    #iCOINr <- regen(iCOIN, quietly = TRUE)

    if (store_results == "onlyresults"){

      # extract the index (to be made more flexible later)
      if(is.list(iCOINr)){
        return(iCOINr$Data$Aggregated[[v_targ]])
      } else {
        return(NA)
      }

    } else if (store_results == "results+params"){

      if(is.list(iCOINr)){
        # index plus specs in method folder
        return(list(Index = iCOINr$Data$Aggregated[[v_targ]],
                    Para = vpara))
      } else {
        # index plus specs in method folder
        return(list(Index = NA,
                    Para = vpara))
      }

    } else if (store_results == "results+method"){

      if(is.list(iCOINr)){
        # index plus specs in method folder
        return(list(Index = iCOINr$Data$Aggregated[[v_targ]],
                    Para = vpara,
                    Method = iCOINr$Method))
      } else {
        # index plus specs in method folder
        return(list(Index = NA,
                    Para = vpara,
                    Method = iCOIN$Method))
      }

    } else if (store_results == "results+COIN"){

      if(is.list(iCOINr)){
        # index plus full COIN
        return(list(Index = iCOINr$Data$Aggregated[[v_targ]],
                    Para = vpara,
                    COIN = iCOINr))
      } else {
        # index plus specs in method folder
        return(list(Index = NA,
                    Para = vpara,
                    COIN = iCOIN))
      }

    } else {stop("store_results not recognised...")}

  } # end rebuild function

  ### CREATE DESIGN ### -----------------------------------------------------------

  # The names and number of SA parameters. This is slightly tricky because of the list format
  # of SA_specs. We exclude the weights here as has different structure. If it exists, we add one to the number
  # of parameters, since weights are treated as a single parameter

  paranames <- names(purrr::flatten(SA_specs[-which(names(SA_specs)=="weights")]))
  # get full names, including weights. We need this for labelling later on, as we need to know
  # the order as well
  paranames_all <- names(purrr::flatten(SA_specs))
  paranames_all[paranames_all == "NoiseSpecs"] <- "weights"
  paranames_all <- paranames_all[paranames_all != "Nominal"]

  npara_noweights <- length(paranames)

  # this is the number of parameters including weights. This is added to in a sec if the weights
  # are also present as a parameter
  npara_all <- npara_noweights

  if (!is.null(SA_specs$weights)){
    # add one to the number of parameters if we have weights...
    npara_all <- npara_all + 1
    # also we need to generate the alternative sets of weights. Do this with function.
    wlist <- noisyWeights(COIN$Parameters$Weights[[SA_specs$weights$Nominal]],
                 noise_specs = SA_specs$weights$NoiseSpecs, Nrep = NrepWeights)
  }



  if (SA_type == "UA"){

    # Run uncertainty analysis. Randomly sample from uncertain variables.
    # a random (uniform) sample
    XX <- matrix(runif(npara_all*N), nrow = N, ncol = npara_all)

  } else if (SA_type == "SA"){

    if(npara_all==1){
      stop("Only one uncertain input defined. It is not meaningful to run a sensitivity analysis
      with only one input variable. Consider changing SA_type to \"UA\".")
    }

    # use standard MC estimators of sensitivity indices
    XX <- SA_sample(N, npara_all)

  }

  # first, make a data frame to record parameter values in (except weights)
  Xpara <- matrix(NA, nrow = nrow(XX), ncol = npara_noweights) %>% as.data.frame()
  colnames(Xpara) <- paranames
  # now a matrix to store the results in
  SAres <- matrix(NA, nrow = nrow(COIN$Data$Aggregated), ncol = nrow(XX))
  # also an empty list for the methods
  SAmethods <- vector(mode = "list", length = nrow(XX))

  ### RUN DESIGN ### --------------------------------------------------------------

  if (store_results == "onlyresults"){

    # run the sample through the function
    SAlist <- list(Scores = apply(XX, MARGIN = 1, FUN = COIN_rebuild, "onlyresults"))

  } else if (store_results == "results+params") {

    # Now run the sample through function
    for (ii in 1:nrow(XX)) {
      SAout <- COIN_rebuild(XX[ii,], store_results = "results+params")
      Xpara[ii,] <- SAout$Para
      SAres[, ii] <- SAout$Index
    }

    SAlist <- list(Scores = SAres,
                Parameters = Xpara)

  } else if (store_results == "results+method") {

    # Now run the sample through function
    for (ii in 1:nrow(XX)) {
      SAout <- COIN_rebuild(XX[ii,], store_results = "results+method")
      Xpara[ii,] <- SAout$Para
      SAres[, ii] <- SAout$Index
      SAmethods[[ii]] <- SAout$Method
    }

    SAlist <- list(Scores = SAres,
                Parameters = Xpara,
                Methods = SAmethods)

  } else if (store_results == "results+COIN") {

    # Now run the sample through function
    for (ii in 1:nrow(XX)) {
      SAout <- COIN_rebuild(XX[ii,], store_results = "results+COIN")
      Xpara[ii,] <- SAout$Para
      SAres[, ii] <- SAout$Index
      SAmethods[[ii]] <- SAout$COIN
    }

    SAlist <- list(Scores = SAres,
                Parameters = Xpara,
                COINs = SAmethods)
  }

  ### POST PROCESSING ### ---------------------------------------------------------

  # ranks for each iteration
  SAlist$Ranks <- apply(SAlist$Scores, MARGIN = 2,
                        function(xx) rank(-1*xx, na.last = "keep", ties.method = "min"))

  # get nominal ranks
  nomranks <- rank(-1*COIN$Data$Aggregated[[v_targ]], na.last = "keep", ties.method = "min")

  if (SA_type == "UA"){

    # Get stats directly from ranks
    SAlist$RankStats <- data.frame(
      UnitCode = COIN$Data$Aggregated$UnitCode,
      Nominal = nomranks,
      Mean = apply(SAlist$Ranks, MARGIN = 1, mean, na.rm = TRUE),
      Median = apply(SAlist$Ranks, MARGIN = 1, stats::median, na.rm = TRUE),
      Q5 = apply(SAlist$Ranks, MARGIN = 1,
                 function(xx) stats::quantile(xx, probs = 0.05, na.rm = TRUE)),
      Q95 = apply(SAlist$Ranks, MARGIN = 1,
                  function(xx) stats::quantile(xx, probs = 0.95, na.rm = TRUE))
    )

  } else if (SA_type == "SA"){

    # UA PART - we anyway get confidence intervals, as with UA, but not using all runs
    # Get runs corresponding to random sampling (the first 2N)
    UAranks <- SAlist$Ranks[,1:(2*N)]
    # Now make results table based on this
    SAlist$RankStats <- data.frame(
      UnitCode = COIN$Data$Aggregated$UnitCode,
      Nominal = nomranks,
      Mean = apply(UAranks, MARGIN = 1, mean, na.rm = TRUE),
      Median = apply(UAranks, MARGIN = 1, stats::median, na.rm = TRUE),
      Q5 = apply(UAranks, MARGIN = 1,
                 function(xx) stats::quantile(xx, probs = 0.05, na.rm = TRUE)),
      Q95 = apply(UAranks, MARGIN = 1,
                  function(xx) stats::quantile(xx, probs = 0.95, na.rm = TRUE))
    )

    # SA PART
    # here we need to process the ranks into a target for the sensitivity analysis
    # An easy target is the mean absolute rank change
    y_AvDiffs <- apply(SAlist$Ranks, 2, FUN = function(x) sum(abs(x-nomranks))/nrow(SAlist$Ranks))

    # using this, get sensitivity estimates and write to output list
    SAout <- SA_estimate(y_AvDiffs, N = N, d = npara_all, Nboot = Nboot)
    SAlist$Sensitivity <- SAout$SensInd
    SAlist$Sensitivity$Variable <- paranames_all
  }

  addUC <- function(mx){
    mx <- as.data.frame(mx)
    colnames(mx) <- paste0("r_",1:ncol(mx))
    cbind(UnitCode = COIN$Data$Aggregated$UnitCode, mx)
  }

  SAlist[c("Scores", "Ranks")] <- lapply(SAlist[c("Scores", "Ranks")], addUC)

  SAlist$Nominal <- data.frame(UnitCode = COIN$Data$Aggregated$UnitCode,
                               Score = COIN$Data$Aggregated[[v_targ]],
                               Rank = nomranks)

  # timing
  tf <- proc.time()
  tdiff <- tf-t0
  telapse <- as.numeric(tdiff[3])
  taverage <- telapse/nrow(XX)
  SAlist$t_elapse <- telapse
  SAlist$t_average <- taverage

  # write parameter names (for plotting, mainly)
  SAlist$ParaNames <- paranames_all

  message(paste0("Time elapsed = ", round(telapse,2), "s, average ", round(taverage,2), "s/rep."))

  return(SAlist)

}


#' Noisy replications of weights
#'
#' Given a set of weights, this function returns multiple replicates of the weights, with added
#' noise. This is intended for use in uncertainty and sensitivity analysis.
#'
#' Weights are expected to be in a long data frame format with columns Aglevel, Code and Weight, as
#' used inside COINs.
#'
#' Noise is added using the noise_specs argument, which is specified by a data frame with columns
#' AgLevel and NoiseFactor. The aggregation level refers to number of the aggregation level to target
#' while the NoiseFactor refers to the size of the perturbation. If e.g. a row is AgLevel = 1 and
#' NoiseFactor = 0.2, this will allow the weights in aggregation level 1 to deviate by +/- 20% of their
#' nominal values (the values in w).
#'
#' @param w A data frame of weights, in the format found in .$Parameters$Weights
#' @param noise_specs a data frame with columns:
#'  * AgLevel: The aggregation level to apply noise to
#'  * NoiseFactor. The size of the perturbation: setting e.g. 0.2 peturbs by +/- 20% of nominal values
#' @param Nrep The number of weight replications to generate.
#'
#' @examples \dontrun{
#'
#' # build ASEM COIN
#' ASEM <- ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta,
#' AggMeta = ASEMAggMeta)
#'
#' # generate 2 sets of weights based on original ASEM weights,
#' # perturbed by +/-20% only at indicator level
#' wlist <- noisyWeights(ASEM$Parameters$Weights$Original,
#' noise_specs = data.frame(AgLevel = 1, NoiseFactor = 0.2), Nrep = 2)}
#'
#' @return A list of sets of weights (data frames)
#'
#' @seealso
#' * [sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#'
#' @export

noisyWeights <- function(w, noise_specs, Nrep){

  stopifnot(is.data.frame(noise_specs))
  if (length(unique(noise_specs$AgLevel)) < nrow(noise_specs)){
    stop("Looks like you have duplicate AgLevel values in the noise_specs df?")
  }

  # make list for weights
  wlist <- vector(mode = "list", length = Nrep)

  for (irep in 1:Nrep){

    # make fresh copy of weights
    wrep <- w

    for (ii in noise_specs$AgLevel){
      # weights for this level
      wts <- wrep$Weight[w$AgLevel == ii]
      # vector of noise: random number in [0,1] times 2, -1. This interprets NoiseFactor as
      # a +/-% deviation.
      wnoise <- (stats::runif(length(wts))*2 - 1)*noise_specs$NoiseFactor[noise_specs$AgLevel == ii]*wts
      # add noise to weights and store
      wts <- wts + wnoise
      wrep$Weight[w$AgLevel == ii] <- wts
    }
    wlist[[irep]] <- wrep
  }

  return(wlist)
}


#' Plot ranks from an uncertainty/sensitivity analysis
#'
#' Plots the ranks resulting from an uncertainty and sensitivity analysis, in particular plots
#' the median, and 5th/95th percentiles of ranks.
#'
#' To use this function you first need to run sensitivity(). Then enter the resulting list as the
#' `SAresults` argument here. I haven't provided an example because of the time required for performing
#' a sensitivity analysis. See [COINr online documentation](https://bluefoxr.github.io/COINrDoc/sensitivity-analysis.html) for more details.
#'
#' @param SAresults A list of sensitivity/uncertainty analysis results from COINr::sensitivity().
#' @param plot_units A character vector of units to plot. Defaults to all units. You can also set
#' to "top10" to only plot top 10 units, and "bottom10" for bottom ten.
#' @param order_by If set to "nominal", orders the rank plot by nominal ranks
#' (i.e. the original ranks prior to the sensitivity analysis). Otherwise if "median", orders by
#' median ranks.
#'
#' @seealso
#' * [sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#' * [plotSA()] Plot sensitivity indices following a sensitivity analysis.
#'
#' @export

plotSARanks <- function(SAresults, plot_units = NULL, order_by = "nominal"){

  rnks <- SAresults$RankStats

  if(!is.null(plot_units)){
    if(length(plot_units == 1)){

      if (plot_units == "top10"){
        unit_include <- SAresults$Nominal$UnitCode[SAresults$Nominal$Rank <= 10]
      } else if (plot_units == "bottom10"){
        unit_include <- SAresults$Nominal$UnitCode[
          SAresults$Nominal$Rank >= (max(SAresults$Nominal$Rank, na.rm = TRUE)-10)]
      } else {
        stop("plot_units not recognised: should be either a character vector of unit codes or else
      \"top10\" or \"bottom10\" ")
      }
    } else {
      # vector, so this should be a vector of unit codes
      unit_include <- plot_units
    }
    rnks <- rnks[rnks$UnitCode %in% unit_include,]
    SAresults$Nominal <- SAresults$Nominal[SAresults$Nominal$UnitCode %in% unit_include,]
  }

  # set ordering of plot
  if (order_by == "nominal"){
    plot_order <- SAresults$Nominal$UnitCode[order(SAresults$Nominal$Score, decreasing = FALSE)]
  } else if (order_by == "median"){
    plot_order <- SAresults$Nominal$UnitCode[order(rnks$Median, decreasing = TRUE)]
  }

  # first, pivot to long
  stats_long <- tidyr::pivot_longer(rnks,
                               cols = c("Median", "Q5", "Q95"),
                               names_to = "Statistic",
                               values_to = "Rank")

  # generate plot
  stats_long %>%
  ggplot2::ggplot(aes(x = .data$Rank, y = .data$UnitCode)) +
    ggplot2::geom_line(aes(group = .data$UnitCode), color = "grey") +
    ggplot2::geom_point(aes(color = .data$Statistic, shape = .data$Statistic, size= .data$Statistic)) +
    ggplot2::scale_shape_manual(values = c(16, 15, 15)) +
    ggplot2::scale_size_manual(values = c(2, 0, 0)) +
    ggplot2::labs(y = "", color = "") +
    ggplot2::guides(shape = FALSE, size = FALSE, color = FALSE) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="top") +
    ggplot2::scale_color_manual(values = c("#83af70", "#e67f83", "#e67f83")) +
    ggplot2::scale_y_discrete(limits = plot_order) +
    ggplot2::scale_x_reverse() +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

}


#' Generate sample for sensitivity analysis
#'
#' Generates an input sample for a Monte Carlo estimation of global sensitivity indices. Used in
#' the sensitivity() function. The total sample size will be N(d+2).
#'
#' This function generates a Monte Carlo sample as described e.g. in the [Global Sensitivity Analysis: The Primer book](https://onlinelibrary.wiley.com/doi/book/10.1002/9780470725184).
#' See also [COINr online documentation](https://bluefoxr.github.io/COINrDoc/sensitivity-analysis.html).
#'
#' @param N The number of sample points per dimension.
#' @param d The dimensionality of the sample
#'
#' @examples \dontrun{
#' X <- SA_sample(100, 3)}
#'
#' @seealso
#' * [sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#' * [SA_estimate()] Estimate sensitivity indices from system output, as a result of input design from SA_sample().
#'
#' @export

SA_sample <- function(N, d){

  # a random (uniform) sample
  Xbase <- matrix(runif(d*N*2), nrow = N, ncol = d*2)
  # get first half
  XA <- Xbase[, 1:d]
  # get second half
  XB <- Xbase[, (d+1):(2*d)]
  # make big matrix (copy matrix d times on the bottom)
  XX <- matrix(rep(t(XA), d), ncol = ncol(XA), byrow = TRUE )

  # now substitute in columns from B into A
  for (ii in 1:d){
    XX[(1 + (ii-1)*N):(ii*N), ii] <- XB[, ii]
  }

  # add original matrices on the beginning
  XX <-  rbind(XA, XB, XX)

  XX
}

#' Estimate sensitivity indices
#'
#' Post process a sample to obtain sensitivity indices. This function takes a univariate output
#' which is generated as a result of running a Monte Carlo sample from SA_sample() through a system.
#' Then it estimates sensitivity indices using this sample.
#'
#' This function is built to be used inside `sensitivity()`. I don't provide an example because of the time taken to
#' do a sensitivity analysis. See [COINr online documentation](https://bluefoxr.github.io/COINrDoc/sensitivity-analysis.html) for more details.
#'
#' @param yy A vector of model output values, as a result of a N(d+2) Monte Carlo design.
#' @param N The number of sample points per dimension.
#' @param d The dimensionality of the sample
#' @param Nboot Number of bootstrap draws for estimates of confidence intervals on sensitivity indices.
#' If this is not specified, bootstrapping is not applied.
#'
#' @importFrom stats var
#'
#' @seealso
#' * [sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#' * [SA_sample()] Input design for estimating sensitivity indices
#'
#' @return A list with variance, first order and total order sensitivity indices.
#'
#' @export

SA_estimate <- function(yy, N, d, Nboot = NULL){

  # put into matrix format: just the ABis
  yyABi <- matrix(yy[(2*N +1):length(yy)], nrow = N)
  # get yA and yB
  yA <- yy[1:N]
  yB <- yy[(N+1) : (2*N)]

  # calculate variance
  varY <- stats::var(c(yA,yB))

  # calculate Si
  Si <- apply(yyABi, 2, function(x){
    mean(yB*(x - yA))/varY
  })

  # calculate ST
  STi <- apply(yyABi, 2, function(x){
    sum((yA - x)^2)/(2*N*varY)
  })

  # make a df
  SensInd <- data.frame(Variable = paste0("V", 1:d),
                        Si = Si,
                        STi = STi)

  ## BOOTSTRAP ## -----

  if (!is.null(Nboot)){

    # Get the "elements" to sample from
    STdiffs <- apply(yyABi, 2, function(x){
      yA - x
    })
    Sidiffs <- apply(yyABi, 2, function(x){
      yB*(x - yA)
    })

    # prep matrices for bootstrap samples
    Si_boot <- matrix(NA, d, Nboot)
    STi_boot <- Si_boot

    # do the bootstrapping bit
    for (iboot in 1:Nboot){

      # calculate Si
      Si_boot[,iboot] <- apply(Sidiffs, 2, function(x){
        mean(sample(x, replace = TRUE))/varY
      })

      # calculate ST
      STi_boot[,iboot] <- apply(STdiffs, 2, function(x){
        sum(sample(x, replace = TRUE)^2)/(2*N*varY)
      })

    }

    # get quantiles of sensitivity indices to add to
    SensInd$Si_q5 <- apply(Si_boot, MARGIN = 1,
                           function(xx) stats::quantile(xx, probs = 0.05, na.rm = TRUE))
    SensInd$Si_q95 <- apply(Si_boot, MARGIN = 1,
                           function(xx) stats::quantile(xx, probs = 0.95, na.rm = TRUE))
    SensInd$STi_q5 <- apply(STi_boot, MARGIN = 1,
                           function(xx) stats::quantile(xx, probs = 0.05, na.rm = TRUE))
    SensInd$STi_q95 <- apply(STi_boot, MARGIN = 1,
                            function(xx) stats::quantile(xx, probs = 0.95, na.rm = TRUE))

  }

  # return outputs
  list(Variance = varY,
       SensInd = SensInd)

}


#' Plot sensitivity indices
#'
#' Plots sensitivity indices as bar or pie charts.
#'
#' To use this function you first need to run sensitivity(). Then enter the resulting list as the
#' `SAresults` argument here. I haven't provided an example because of the time required for performing
#' a sensitivity analysis.
#' See [COINr online documentation](https://bluefoxr.github.io/COINrDoc/sensitivity-analysis.html) for more details.
#'
#' @param SAresults A list of sensitivity/uncertainty analysis results from COINr::sensitivity().
#' @param ptype Type of plot to generate - either "bar", "pie" or "box"
#'
#' @seealso
#' * [sensitivity()] Perform global sensitivity or uncertainty analysis on a COIN
#' * [plotSARanks()] Plot confidence intervals on ranks following a sensitivity analysis
#'
#' @export

plotSA <- function(SAresults, ptype = "bar"){

  # prep data first
  Sdf <- SAresults$Sensitivity
  numcols <- Sdf[-1]
  # set any negative values to zero. By definition they can't be negative.
  numcols[numcols < 0] <- 0
  Sdf[-1] <- numcols

  if(ptype == "bar"){

    # the full bar is STi. It is divided into Si and the remainder, so we need STi - Si
    Sdf <- dplyr::mutate(Sdf, Interactions = .data$STi - .data$Si)
    Sdf$Interactions[Sdf$Interactions < 0] <- 0
    # rename col to improve plot
    colnames(Sdf)[colnames(Sdf) == "Si"] <- "MainEffect"
    # now pivot to get in format for ggplot
    bardf <- tidyr::pivot_longer(Sdf,
                                 cols = c("MainEffect", "Interactions"))

    # make stacked bar plot
    ggplot2::ggplot(bardf, ggplot2::aes(fill=.data$name, y=.data$value, x=.data$Variable)) +
      ggplot2::geom_bar(position="stack", stat="identity") +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = NULL) +
      ggplot2::theme_minimal()


  } else if (ptype == "pie"){

    # we are plotting first order sensitivity indices. So, also estimate interactions.
    Sis <- Sdf[c("Variable", "Si")]
    intsum <- max(c(1 - sum(Sis$Si, na.rm = TRUE), 0))
    Sis <- rbind(Sis, data.frame(Variable = "Interactions", Si = intsum))

    # Basic piechart
    ggplot2::ggplot(Sis, ggplot2::aes(x = "", y = .data$Si, fill = .data$Variable)) +
      ggplot2::geom_bar(stat="identity", width=1, color="white") +
      ggplot2::coord_polar("y", start=0) +
      ggplot2::theme_void() # remove background, grid, numeric labels

  } else if (ptype == "box"){

    Sdf <- tidyr::pivot_longer(Sdf, cols = c("Si", "STi"))
    Sdf$q5 <- ifelse(Sdf$name == "STi", Sdf$STi_q5, Sdf$Si_q5)
    Sdf$q95 <- ifelse(Sdf$name == "STi", Sdf$STi_q95, Sdf$Si_q95)
    Sdf$q5[Sdf$q5 > 1] <- 1
    Sdf$q95[Sdf$q95 > 1] <- 1
    Sdf$value[Sdf$value > 1] <- 1

    ggplot2::ggplot(Sdf, ggplot2::aes(x = .data$Variable, y = .data$value, ymax = .data$q95, ymin = .data$q5)) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::geom_errorbar(width = 0.2) +
      ggplot2::theme_bw() +
      facet_wrap(~name) +
      ggplot2::labs(
        x = NULL,
        y = NULL)

  }

}
