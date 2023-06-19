#' Get effective weights
#'
#' Calculates the "effective weight" of each indicator and aggregate at the index level. The effective weight is calculated
#' as the final weight of each component in the index, and this is due to not just to its own weight, but also to the weights of
#' each aggregation that it is involved in, plus the number of indicators/aggregates in each group. The effective weight
#' is one way of understanding the final contribution of each indicator to the index. See also `vignette("weights")`.
#'
#' This function replaces the now-defunct `effectiveWeight()` from COINr < v1.0.
#'
#' @param coin A coin class object
#' @param out2 Either `"coin"` or `"df"`
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)
#'
#' # get effective weights as data frame
#' w_eff <- get_eff_weights(coin, out2 = "df")
#'
#' head(w_eff)
#'
#' @return Either an iMeta data frame with effective weights as an added column, or an updated coin with effective
#' weights added to `.$Meta$Ind`.
#' @export
get_eff_weights <-  function(coin, out2 = "df"){


  # PREP --------------------------------------------------------------------

  check_coin_input(coin)
  stopifnot(out2 %in% c("df", "coin"))

  # EFF WEIGHTS -------------------------------------------------------------

  # get indicator metadata
  iMeta <- coin$Meta$Ind
  # ditch NAs
  iMeta <- iMeta[!is.na(iMeta$Level),]

  # we need to rescale weights to sum to the weight of the parent
  # needs to be done by working from highest level downwards

  maxlev <- coin$Meta$maxlev

  # index by parent, but highest level has parent = NA which breaks tapply a bit.
  # so assign a fake category just for this operation
  iMeta$Parent[iMeta$Level == maxlev] <- "none"

  # first make all weights sum to 1 inside group
  iMeta_sp <- split(iMeta, iMeta$Parent)
  iMeta_sp <- lapply(iMeta_sp, function(x){
    x$EffWeight <- x$Weight/sum(x$Weight)
    x
  })
  iMeta <- unsplit(iMeta_sp, iMeta$Parent)

  # now we have to work from highest level downwards to make weights sum to parent weight
  # This is done by multiplying weights with parent weights.
  for(lev in (maxlev-1):1){
    # get codes in lev
    codes <- iMeta$iCode[iMeta$Level == lev]
    # get weights of codes
    idx <- match(codes, iMeta$iCode)
    w_lev <- iMeta$EffWeight[idx]
    # get codes of parents
    codes_p <- iMeta$Parent[idx]
    # get weights of parents
    w_par <- iMeta$EffWeight[match(codes_p, iMeta$iCode)]
    # multiply
    iMeta$EffWeight[idx] <- w_lev*w_par
  }

  # OUTPUT ------------------------------------------------------------------

  if(out2 == "df"){
    iMeta[c("iCode", "Level", "Weight", "EffWeight")]
  } else if(out2 == "coin"){
    coin$Meta$Ind$EffWeight <- iMeta$EffWeight[match(coin$Meta$Ind$iCode, iMeta$iCode )]
    coin
  }

}


#' Weight optimisation
#'
#' This function provides optimised weights to agree with a pre-specified vector of "target importances".
#'
#' This is a linear version of the weight optimisation proposed in this paper: \doi{10.1016/j.ecolind.2017.03.056}.
#' Weights are optimised to agree with a pre-specified vector of "importances". The optimised weights are returned back to the coin.
#'
#' See `vignette("weights")` for more details on the usage of this function and an explanation of the underlying
#' method. Note that this function calculates correlations without considering statistical significance.
#'
#' This function replaces the now-defunct `weightOpt()` from COINr < v1.0.
#'
#' @param coin coin object
#' @param itarg a vector of (relative) target importances. For example, `c(1,2,1)` would specify that the second
#' indicator should be twice as "important" as the other two.
#' @param Level The aggregation level to apply the weight adjustment to. This can only be one level.
#' @param cortype The type of correlation to use - can be either `"pearson"`, `"spearman"` or `"kendall"`. See [stats::cor].
#' @param optype The optimisation type. Either `"balance"`, which aims to balance correlations
#' according to a vector of "importances" specified by `itarg` (default), or `"infomax"` which aims to maximise
#' overall correlations.
#' @param toler Tolerance for convergence. Defaults to 0.1 (decrease for more accuracy, increase if convergence problems).
#' @param maxiter Maximum number of iterations. Default 500.
#' @param out2 Where to output the results. If `"coin"` (default for coin input), appends to updated coin,
#' creating a new list of weights in `.$Parameters$Weights`. Otherwise if `"list"` outputs to a list (default).
#' @param dset Name of the aggregated data set found in `coin$Data` which results from calling [Aggregate()].
#' @param weights_to Name to write the optimised weight set to, if `out2 = "coin"`.
#'
#' @importFrom stats optim
#'
#' @examples
#' # build example coin
#' coin <- build_example_coin(quietly = TRUE)
#'
#' # check correlations between level 3 and index
#' get_corr(coin, dset = "Aggregated", Levels = c(3, 4))
#'
#' # optimise weights at level 3
#' l_opt <- get_opt_weights(coin, itarg = "equal", dset = "Aggregated",
#'                         Level = 3, weights_to = "OptLev3", out2 = "list")
#'
#' # view results
#' tail(l_opt$WeightsOpt)
#'
#' l_opt$CorrResultsNorm
#'
#' @return If `out2 = "coin"` returns an updated coin object with a new set of weights in `.$Meta$Weights`, plus
#' details of the optimisation in `.$Analysis`.
#' Else if `out2 = "list"` the same outputs (new weights plus details of optimisation) are wrapped in a list.
#'
#' @export
get_opt_weights <- function(coin, itarg = NULL, dset, Level, cortype = "pearson", optype = "balance",
                      toler = NULL, maxiter = NULL, weights_to = NULL, out2 = "list"){

  # PREP --------------------------------------------------------------------

  check_coin_input(coin)

  stopifnot(optype %in% c("balance", "infomax"),
            out2 %in% c("list", "coin"))

  # number of weights at specified level
  n_w <- sum(coin$Meta$Weights$Original$Level == Level)

  if(optype == "infomax"){
    itarg <- NULL
  }

  # if equal influence requested
  if(!is.null(itarg)){
    if(is.character(itarg)){
      if(itarg == "equal"){
        itarg <- rep(1, n_w)
      } else {
        stop("itarg not recognised - should be either numeric vector or \"equal\" ")
      }
    }
  } else {
    if(optype == "balance"){
      stop("If optype = 'balance' you must specify itarg.")
    }
  }

  if (optype == "balance"){
    if(length(itarg) != n_w){
      stop("itarg is not the correct length for the specified Level")
    }

    itarg <- itarg/sum(itarg)
  }

  # we need to define an objective function. The idea here is to make a function, which when you
  # put in a set of weights, gives you the correlations

  # get original weights
  w0 <- coin$Meta$Weights$Original

  # make a null object for storing correlations in - this will be updated by the function below.
  crs_out <- NULL

  objfunc <- function(w){

    w <- w/sum(w)
    wlist <- w0
    # modify appropriate level to current vector of weights
    wlist$Weight[wlist$Level == Level] <- w
    # re-aggregate using these weights, get correlations
    crs <- weights2corr(coin, dset = dset, w = wlist, Levels = c(Level, coin$Meta$maxlev),
                        cortype = cortype)$cr$Correlation

    if (optype == "balance"){
      # normalise so they sum to 1
      crs_n <- crs/sum(crs)
      # the output is the sum of the squared differences (note, could also log this)
      sqdiff <- log(sum((itarg - crs_n)^2)/length(crs_n))
    } else if (optype == "infomax"){
      # the output is the sum of the correlations *-1
      sqdiff <- sum(crs)*-1
      # assign to crs_n for export outside function
      crs_n <- crs
    }
    # send outside function
    crs_out <<- crs_n
    message("iterating... objective function = ", sqdiff)
    sqdiff

  }

  # defaults for tolerance and max iterations
  if(is.null(toler)){toler <- 0.1}
  if(is.null(maxiter)){maxiter <- 500}


  # get initial values
  if(optype == "balance"){
    init_vals <- itarg
  } else {
    init_vals <- w0$Weight[w0$Level == Level]
  }

  # run optimisation
  optOut <- stats::optim(par = init_vals, fn = objfunc, control = list(
    reltol = toler,
    maxit = maxiter
  ))

  if(optOut$convergence == 0){
    message("Optimisation successful!")
  } else {
    message("Optimisation did not converge for some reason...
         You can try increasing the number of iterations and/or the tolerance.")
  }

  # get optimised weights at level
  wopt <- optOut$par
  # normalise to sum to 1
  wopt <- wopt/sum(wopt)
  # get full list of weights
  wopt_full <- w0
  # modify appropriate level to optimised vector of weights
  wopt_full$Weight[wopt_full$Level == Level] <- wopt

  # results df
  desired <- if(is.null(itarg)){NA}else{itarg}
  df_res <- data.frame(
    Desired = desired,
    Obtained = crs_out,
    OptWeight = wopt
  )

  if (out2 == "coin"){
    if(is.null(weights_to)){
      weights_to <- paste0("OptimsedLev", Level)
    }
    coin$Meta$Weights[[weights_to]] <- wopt_full
    message("Optimised weights written to .$Meta$Weights$", weights_to)

    coin$Analysis$Weights[[weights_to]] <- list(OptResults = optOut,
                                            CorrResultsNorm = df_res)
    coin
  } else {
    list(WeightsOpt = wopt_full,
         OptResults = optOut,
         CorrResultsNorm = df_res)
  }
}


# Recalculate correlations and ranks based on new weights
#
# This is a short cut function which takes a new set of indicator weights, and recalculates the coin results
# based on these weights. It returns a summary of rankings and the correlations between indicators and index.
#
# @param coin coin object
# @param w Full data frame of weights for each level
# @param dset Name of the data set that is created when [Aggregate()] is called. This is used to calculated correlations
# and to extract the results table. Default `"Aggregated"`.
# @param Levels A 2-length vector with two aggregation levels to correlate against each other
# @param cortype Correlation type. Either `"pearson"` (default), `"kendall"` or `"spearman"`. See [stats::cor].
# @param withparent Logical: if `TRUE`, only correlates with the parent, e.g. sub-pillars are only correlated with
# their parent pillars and not others.
#
# @return A list where `.$cr` is a vector of correlations between each indicator and the index, and
# `.$dat` is a data frame of results
#
# @examples
# #
weights2corr <- function(coin, w, dset = "Aggregated", Levels = NULL,
                         cortype = "pearson", withparent = TRUE){

  # PREP --------------------------------------------------------------------
  # note, many checks are done inside lower-level functions called here
  # note2: I have removed the iCodes argument. Not sure that it was ever used.

  if(is.null(Levels)){
    Levels <- c(1, coin$Meta$maxlev)
  }

  if(is.null(coin$Log$Aggregate)){
    stop("You have not yet aggregated your data. This needs to be done first.")
  }


  # GET CORR, RES -----------------------------------------------------------

  # update weights
  coin$Log$Aggregate$w <- w

  # regenerate
  coin2 <- Regen(coin, from = "Aggregate", quietly = TRUE)

  # get correlations
  crtable <- get_corr(coin2, dset = dset, Levels = Levels,
                      cortype = cortype, withparent = withparent, pval = 0)

  # get results
  dfres <- get_results(coin2, dset = dset, tab_type = "Aggs")


  # OUTPUT ------------------------------------------------------------------

  # output list
  # NOTE: missing iCodes1 entry - not sure if this is used for rew8r and need to understand the context.
  list(cr = crtable,
       dat = dfres)

}
