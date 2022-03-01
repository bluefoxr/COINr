#' Get effective weights
#'
#' @param coin A coin class object
#' @param out2 Either `"coin"` or `"df"`
#'
#' @return Either an iMeta data frame with effective weights as an added column, or an updated coin with effective
#' weights added to `.$Meta$Ind`.
#' @export
get_eff_wts <-  function(coin, out2 = "df"){


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
    iMeta
  } else if(out2 == "coin"){
    coin$Meta$Ind <- iMeta
    coin
  }

}




#' Weight optimisation
#'
#' This function provides optimised weights to agree with a pre-specified vector of "target importances".
#'
#' This is a linear version of the weight optimisation proposed in this paper: \doi{10.1016/j.ecolind.2017.03.056}.
#' Weights are optimised to agree with a pre-specified vector of "importances". The optimised weights are returned back to the COIN.
#'
#' See the [chapter in the COINr online documentation](https://bluefoxr.github.io/COINrDoc/weighting-1.html#automatic-re-weighting)
#' for more details.
#'
#' @param COIN COIN object
#' @param itarg a vector of (relative) target importances. For example, `c(1,2,1)` would specify that the second
#' indicator should be twice as "important" as the other two.
#' @param aglev The aggregation level to apply the weight adjustment to.
#' @param cortype The type of correlation to use - can be either `"pearson"`, `"spearman"` or `"kendall"`. See [stats::cor].
#' @param optype The optimisation type. Either `"balance"`, which aims to balance correlations
#' according to a vector of "importances" specified by `itarg` (default), or `"infomax"` which aims to maximise
#' overall correlations. *This latter option is experimental and may not yet work very well*.
#' @param toler Tolerance for convergence. Defaults to 0.001 (decrease for more accuracy, increase if convergence problems).
#' @param maxiter Maximum number of iterations. Default 500.
#' @param out2 Where to output the results. If `"COIN"` (default for COIN input), appends to updated COIN,
#' creating a new list of weights in `.$Parameters$Weights`. Otherwise if `"list"` outputs to a list (default).
#'
#' @importFrom stats optim
#'
#' @examples
#' # build ASEM COIN up to aggregation
#' ASEM <- build_ASEM()
#' # optimise sub-pillar weights to give equal correlations with index
#' ASEM <- weightOpt(ASEM, itarg = "equal", aglev = 3, out2 = "COIN")
#'
#' @seealso
#' * [rew8r()] Interactive app for adjusting weights and seeing effects on correlations
#' * [getPCA()] PCA, including weights from PCA
#'
#' @return If `out2 = "COIN"` returns an updated COIN object with a new set of weights in `.$Parameters$Weights`, plus
#' details of the optimisation in `.$Analysis`.
#' Else if `out2 = "list"` the same outputs (new weights plus details of optimisation) are wrapped in a list.
#'
#' @export

weightOpt <- function(COIN, itarg, aglev, cortype = "pearson", optype = "balance",
                      toler = NULL, maxiter = NULL, out2 = NULL){

  # if equal influence requested
  if(is.character(itarg)){
    if(itarg == "equal"){
      itarg <- rep(1, sum(COIN$Parameters$Weights$Original$AgLevel == aglev))
    } else {
      stop("itarg not recognised - should be either numeric vector or \"equal\" ")
    }
  }

  if(length(itarg) != sum(COIN$Parameters$Weights$Original$AgLevel == aglev) ){
    stop("itarg is not the correct length for the specified aglev")
  }

  itarg <- itarg/sum(itarg)

  # we need to define an objective function. The idea here is to make a function, which when you
  # put in a set of weights, gives you the correlations

  objfunc <- function(w){

    # get full list of weights
    wlist <- COIN$Parameters$Weights$Original
    # modify appropriate level to current vector of weights
    wlist$Weight[wlist$AgLevel == aglev] <- w
    # re-aggregate using these weights, get correlations
    crs <- weights2corr(COIN, wlist, aglevs = c(aglev, COIN$Parameters$Nlevels),
                        cortype = cortype)$cr$Correlation

    if (optype == "balance"){
      # normalise so they sum to 1
      crs <- crs/sum(crs)
      # the output is the sum of the squared differences (note, could also log this)
      sqdiff <- sum((itarg - crs)^2)/length(crs)
    } else if (optype == "infomax"){
      # the output is the sum of the squared differences (note, could also log this)
      sqdiff <- log(sum(1 - crs))/length(crs)
    }
    message("iterating... squared difference = ", sqdiff)
    return(sqdiff)

  }

  # defaults for tolerance and max iterations
  if(is.null(toler)){toler <- 0.001}
  if(is.null(maxiter)){maxiter <- 500}

  # run optimisation
  optOut <- stats::optim(par = itarg, fn = objfunc, control = list(
    reltol = toler,
    maxit = maxiter
  ))

  if(optOut$convergence == 0){
    message("Optimisation successful!")
  } else {
    message("Optimisation did not converge for some reason...
         You can try increasing the number of iterations and/or the tolerance.")
  }

  # get full list of weights
  wopt <- COIN$Parameters$Weights$Original
  # modify appropriate level to optimised vector of weights
  wopt$Weight[wopt$AgLevel == aglev] <- optOut$par

  if(is.null(out2)){out2 <- "list"}

  if (out2 == "COIN"){
    wname <- paste0("OptimsedLev", aglev)
    COIN$Parameters$Weights[[wname]] <- wopt
    COIN$Analysis$Weights[[wname]] <- list(OptResults = optOut,
                                           CorrResults = data.frame(
                                             Desired = itarg,
                                             Obtained = crs <- weights2corr(COIN, wopt, aglevs = c(aglev, COIN$Parameters$Nlevels),
                                                                            cortype = cortype)$cr$Correlation,
                                             OptWeight = optOut$par
                                           ))
    return(COIN)
  } else {
    return(list(WeightsOpt = wopt,
                OptResults = optOut,
                CorrResults = data.frame(
                  Desired = itarg,
                  Obtained = crs <- weights2corr(COIN, wopt, aglevs = c(aglev, COIN$Parameters$Nlevels),
                                                 cortype = cortype)$cr$Correlation,
                  OptWeight = optOut$par
                )))
  }

}
