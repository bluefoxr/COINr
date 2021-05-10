#' Sensitivity analysis
#'
#' Performs uncertainty and sensitivity analysis
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
#' @param quietly If FALSE (default), gives progress messages. Set TRUE to suppress these.
#'
#' @importFrom purrr flatten
#' @importFrom stats median quantile runif
#'
#' @examples \dontrun{SAresults <- sensitivity(COIN, v_targ = "Index")}
#'
#' @return Sensitivity analysis results
#'
#' @export

sensitivity <- function(COIN, v_targ, SA_specs, N, SA_type = "UA", NrepWeights = 1000,
                        store_results = "results+params", quietly = FALSE){

  # to add:
  # - extend to multiple target variables
  # - continuous distribution support

  # Rebuild function --------------------------------------------------------
  # We will need a function which rebuilds the index according to specifications in the SA_specs list.

  COIN_rebuild <- function(x, store_results){

    if (!quietly){
      message(paste0("Iteration ",ii," of ",N," ... ", ii*100/N, "% complete" ))
    }

    # make a fresh copy of the COIN
    iCOIN <- COIN
    # make a vector for storing parameter values
    vpara <- matrix(NA, nrow = 1, ncol = npara_noweights) %>% as.data.frame()
    colnames(vpara) <- paranames

    # variable counter (makes indexing easier in a hierarchical list)
    pcount <- 1

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
          pvals <- unlist(plist[[iparam]])

          # we now have to alter the parameter
          # First, get the parameter index according to x
          ix <- ceiling(x[pcount]*length(pvals))

          # Now, select the parameter value
          ipval <- pvals[[ix]]

          # Update the Method
          iCOIN$Method[[fname]][[pname]] <- ipval

          # also store value in vector
          vpara[pcount] <- ipval

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

  # Prep and make design for running thru function ----------------------------------------------

  # The names and number of SA parameters. This is slightly tricky because of the list format
  # of SA_specs. We exclude the weights here as has different structure. If it exists, we add one to the number
  # of parameters, since weights are treated as a single parameter

  paranames <- names(purrr::flatten(SA_specs[-which(names(SA_specs)=="weights")]))
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
  }

  # first, make a data frame to record parameter values in (except weights)
  Xpara <- matrix(NA, nrow = N, ncol = npara_noweights) %>% as.data.frame()
  colnames(Xpara) <- paranames
  # now a matrix to store the results in
  SAres <- matrix(NA, nrow = nrow(COIN$Data$Aggregated), ncol = N)
  # also an empty list for the methods
  SAmethods <- vector(mode = "list", length = N)

  # Run design through function according to options ---------------------------------------------

  if (store_results == "onlyresults"){

    # run the sample through the function
    SAlist <- list(Scores = apply(XX, MARGIN = 1, FUN = COIN_rebuild, "onlyresults"))

  } else if (store_results == "results+params") {

    # Now run the sample through function
    for (ii in 1:N) {
      SAout <- COIN_rebuild(XX[ii,], store_results = "results+params")
      Xpara[ii,] <- SAout$Para
      SAres[, ii] <- SAout$Index
    }

    SAlist <- list(Scores = SAres,
                Parameters = Xpara)

  } else if (store_results == "results+method") {

    # Now run the sample through function
    for (ii in 1:N) {
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
    for (ii in 1:N) {
      SAout <- COIN_rebuild(XX[ii,], store_results = "results+COIN")
      Xpara[ii,] <- SAout$Para
      SAres[, ii] <- SAout$Index
      SAmethods[[ii]] <- SAout$COIN
    }

    SAlist <- list(Scores = SAres,
                Parameters = Xpara,
                COINs = SAmethods)
  }

  # Post process into ranks etc ----------------------------------------

  SAlist$Ranks <- apply(SAlist$Scores, MARGIN = 2,
                        function(xx) rank(-1*xx, na.last = "keep", ties.method = "min"))

  SAlist$RankStats <- data.frame(
    UnitCode = COIN$Data$Aggregated$UnitCode,
    Mean = apply(SAlist$Ranks, MARGIN = 1, mean, na.rm = TRUE),
    Median = apply(SAlist$Ranks, MARGIN = 1, stats::median, na.rm = TRUE),
    Q5 = apply(SAlist$Ranks, MARGIN = 1,
                   function(xx) stats::quantile(xx, probs = 0.05, na.rm = TRUE)),
    Q95 = apply(SAlist$Ranks, MARGIN = 1,
                     function(xx) stats::quantile(xx, probs = 0.95, na.rm = TRUE))
  )

  addUC <- function(mx){
    mx <- as.data.frame(mx)
    colnames(mx) <- paste0("r_",1:ncol(mx))
    cbind(UnitCode = COIN$Data$Aggregated$UnitCode, mx)
  }

  SAlist[c("Scores", "Ranks")] <- lapply(SAlist[c("Scores", "Ranks")], addUC)

  SAlist$Nominal <- data.frame(UnitCode = COIN$Data$Aggregated$UnitCode,
                               Score = COIN$Data$Aggregated[[v_targ]],
                               Rank = rank(-1*COIN$Data$Aggregated[[v_targ]], na.last = "keep", ties.method = "min"))

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
#' @return A list of sets of weights (data frames)
#'
#' @export

noisyWeights <- function(w, noise_specs, Nrep){

  stopifnot(is.data.frame(noise_specs))

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
#' To use this function you first need to run sensitivity().
#'
#' @param SAresults A list of sensitivity/uncertainty analysis results from COINr::sensitivity().
#' @param plot_units A character vector of units to plot. Defaults to all units. You can also set
#' to "top10" to only plot top 10 units, and "bottom10" for bottom ten.
#' @param order_by If set to "nominal", orders the rank plot by nominal ranks
#' (i.e. the original ranks prior to the sensitivity analysis). Otherwise if "median", orders by
#' median ranks.
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
    ggplot2::scale_size_manual(values = c(3, 1, 1)) +
    ggplot2::labs(y = "UnitCode", color = "") +
    ggplot2::guides(shape = FALSE, size = FALSE, color = FALSE) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="top") +
    ggplot2::scale_color_manual(values = c("#83af70", "#e67f83", "#e67f83")) +
    ggplot2::scale_y_discrete(limits = plot_order)

}
