#' Regenerate COIN object
#'
#' Function to regenerate the results of the COIN, using the methodological parameters stored in
#' `.$Method`. This function calls the construction functions of COINr in the order that they are
#' found in `.$Method`, along with any custom code found in `.$Method$Custom`.
#'
#' Note that while sets of weights will be passed to the regenerated COIN, anything in `.$Analysis` will be removed and will have to be recalculated.
#'
#' For more details on regeneration of COINs, comparisons and adjustments, see the [online documentation](https://bluefoxr.github.io/COINrDoc/adjustments-and-comparisons.html).
#'
#' @param COINold COIN object containing specifications on how to regenerate.
#' @param quietly Logical: if `TRUE` suppresses all messages from COINr functions (warnings may still occur though).
#'
#' @return An updated COIN object, with all data sets recalculated according to specifications in `.$Method`. Weight sets will be passed through.
#'
#' @examples
#' ASEM <- build_ASEM()
#' # Make a copy
#' ASEMAltNorm <- ASEM
#' # Edit .$Method
#' ASEMAltNorm$Method$normalise$ntype <- "borda"
#' # Regenerate
#' ASEMAltNorm <- regen(ASEMAltNorm, quietly = TRUE)
#'
#' @seealso
#' * [compTable()] compare two different COINs
#' * [compTableMulti()] compare multiple COINs
#'
#' @export

regen <- function(COINold, quietly = FALSE){

  if(!is.COIN(COINold)){
    stop("This function only works on a COIN.")
  }

  if (quietly){
    # Assemble always comes first.
    COIN <- assemble(IndData = COINold$Input$Original$IndData,
                        IndMeta = COINold$Input$Original$IndMeta,
                        AggMeta = COINold$Input$Original$AggMeta,
                        include = COINold$Method$assemble$include,
                        exclude = COINold$Method$assemble$exclude,
                        preagg = COINold$Method$assemble$preagg,
                        use_year = COINold$Method$assemble$use_year,
                        impute_latest = COINold$Method$assemble$impute_latest) %>% suppressMessages()

    # copy weights from old COIN (otherwise any additional will be deleted)
    # first, save the new original weights (may have added/deleted indicators)
    rescueorig <- COIN$Parameters$Weights$Original
    # overwrite new weights with old (all weight sets)
    COIN$Parameters$Weights <- COINold$Parameters$Weights
    # re-add the new original weights as we don't want to disturb these.
    COIN$Parameters$Weights$Original <- rescueorig

    # optional custom operation
    if (exists("Custom",COINold$Method)){
      if (exists("after_assemble", COINold$Method$Custom)){
        eval(COINold$Method$Custom$after_assemble) %>% suppressMessages()
      }
    }

    # Now we need to find out what order the operations were done in.
    # Presume that it is the same order as in the Method folder.
    fOps <- names(COINold$Method)

    # Each of these represents a COINr construction function. We will go through these in order.
    # If there is any custom code, this will be inserted in the appropriate place.

    for (ii in 1:length(fOps)){

      # the current step
      fi <- fOps[ii]

      # if we find custom folder, go to next (these are dealt with inside each if below)
      if ((fi == "Custom")|(fi == "assemble")){next}

      # Now we have to see which function to run.
      if(fi=="checkData"){

        # run checkData
        COIN <- checkData(COIN, dset = COINold$Method$checkData$dset,
                             ind_thresh = COINold$Method$checkData$ind_thresh,
                             zero_thresh = COINold$Method$checkData$zero_thresh,
                             unit_screen = COINold$Method$checkData$unit_screen,
                             Force = COINold$Method$checkData$Force,
                             out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_checkData", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_checkData) %>% suppressMessages()
          }
        }

      } else if (fi=="denominate"){

        # run denominate
        COIN <- denominate(COIN, dset = COINold$Method$denominate$dset,
                              specby = COINold$Method$denominate$specby,
                              denomby = COINold$Method$denominate$denomby,
                              denominators = COINold$Method$denominate$denominators,
                              scaledenoms = COINold$Method$denominate$scaledenoms,
                              out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_denominate", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_denominate) %>% suppressMessages()
          }
        }

      } else if (fi=="impute"){

        # run impute
        COIN <- impute(COIN, imtype = COINold$Method$impute$imtype,
                          dset = COINold$Method$impute$dset,
                          groupvar = COINold$Method$impute$groupvar,
                          #byyear = COINold$Method$impute$byyear,
                          EMaglev = COINold$Method$impute$EMaglev,
                          out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_impute", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_impute) %>% suppressMessages()
          }
        }

      } else if (fi=="treat"){

        # run treat
        COIN <- treat(COIN, dset = COINold$Method$treat$dset,
                         winmax = COINold$Method$treat$winmax,
                         winchange = COINold$Method$treat$winchange,
                         deflog = COINold$Method$treat$deflog,
                         boxlam = COINold$Method$treat$boxlam,
                         t_skew = COINold$Method$treat$t_skew,
                         t_kurt = COINold$Method$treat$t_kurt,
                         individual = COINold$Method$treat$individual,
                         indiv_only = COINold$Method$treat$indiv_only,
                         bypass_all = COINold$Method$treat$bypass_all) %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_treat", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_treat) %>% suppressMessages()
          }
        }

      } else if (fi=="normalise"){

        # run normalise
        COIN <- normalise(COIN, ntype = COINold$Method$normalise$ntype,
                             npara = COINold$Method$normalise$npara,
                             dset = COINold$Method$normalise$dset,
                             directions = COINold$Method$normalise$directions,
                             individual = COINold$Method$normalise$individual,
                             indiv_only = COINold$Method$normalise$indiv_only,
                             out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_normalise", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_normalise) %>% suppressMessages()
          }
        }

      } else if (fi=="aggregate"){

        # run aggregate
        COIN <- aggregate(COIN, agtype = COINold$Method$aggregate$agtype,
                             agweights = COINold$Method$aggregate$agweights,
                             dset = COINold$Method$aggregate$dset,
                             agtype_bylevel = COINold$Method$aggregate$agtype_bylevel,
                             agfunc = COINold$Method$aggregate$agfunc,
                             out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_aggregate", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_aggregate) %>% suppressMessages()
          }
        }

      } else {
        stop("Method type not recognised...")
      }

    }

    ## END QUIET OUTPUT

  } else {

    ## BEGIN VERBOSE OUTPUT

    # Assemble always comes first.
    COIN <- assemble(IndData = COINold$Input$Original$IndData,
                        IndMeta = COINold$Input$Original$IndMeta,
                        AggMeta = COINold$Input$Original$AggMeta,
                        include = COINold$Method$assemble$include,
                        exclude = COINold$Method$assemble$exclude,
                        use_year = COINold$Method$assemble$use_year,
                        impute_latest = COINold$Method$assemble$impute_latest)

    # copy weights from old COIN (otherwise any additional will be deleted)
    # first, save the new original weights (may have added/deleted indicators)
    rescueorig <- COIN$Parameters$Weights$Original
    # overwrite new weights with old (all weight sets)
    COIN$Parameters$Weights <- COINold$Parameters$Weights
    # re-add the new original weights as we don't want to disturb these.
    COIN$Parameters$Weights$Original <- rescueorig

    # optional custom operation
    if (exists("Custom",COINold$Method)){
      if (exists("after_assemble", COINold$Method$Custom)){
        eval(COINold$Method$Custom$after_assemble)
      }
    }

    # Now we need to find out what order the operations were done in.
    # Presume that it is the same order as in the Method folder.
    fOps <- names(COINold$Method)

    # Each of these represents a COINr construction function. We will go through these in order.
    # If there is any custom code, this will be inserted in the appropriate place.

    for (ii in 1:length(fOps)){

      # the current step
      fi <- fOps[ii]

      # if we find custom folder, go to next (these are dealt with inside each if below)
      if ((fi == "Custom")|(fi == "assemble")){next}

      # Now we have to see which function to run.
      if(fi=="checkData"){

        # run checkData
        COIN <- checkData(COIN, dset = COINold$Method$checkData$dset,
                             ind_thresh = COINold$Method$checkData$ind_thresh,
                             unit_screen = COINold$Method$checkData$unit_screen,
                             Force = COINold$Method$checkData$Force,
                             out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_checkData", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_checkData)
          }
        }

      } else if (fi=="denominate"){

        # run denominate
        COIN <- denominate(COIN, dset = COINold$Method$denominate$dset,
                              specby = COINold$Method$denominate$specby,
                              denomby = COINold$Method$denominate$denomby,
                              denominators = COINold$Method$denominate$denominators,
                              out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_denominate", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_denominate)
          }
        }

      } else if (fi=="impute"){

        # run impute
        COIN <- impute(COIN, imtype = COINold$Method$impute$imtype,
                          dset = COINold$Method$impute$dset,
                          groupvar = COINold$Method$impute$groupvar,
                          #byyear = COINold$Method$impute$byyear,
                          EMaglev = COINold$Method$impute$EMaglev,
                          out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_impute", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_impute)
          }
        }

      } else if (fi=="treat"){

        # run treat
        COIN <- treat(COIN, dset = COINold$Method$treat$dset,
                         winmax = COINold$Method$treat$winmax,
                         winchange = COINold$Method$treat$winchange,
                         deflog = COINold$Method$treat$deflog,
                         boxlam = COINold$Method$treat$boxlam,
                         t_skew = COINold$Method$treat$t_skew,
                         t_kurt = COINold$Method$treat$t_kurt,
                         individual = COINold$Method$treat$individual,
                         indiv_only = COINold$Method$treat$indiv_only)

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_treat", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_treat)
          }
        }

      } else if (fi=="normalise"){

        # run normalise
        COIN <- normalise(COIN, ntype = COINold$Method$normalise$ntype,
                             npara = COINold$Method$normalise$npara,
                             dset = COINold$Method$normalise$dset,
                             directions = COINold$Method$normalise$directions,
                             individual = COINold$Method$normalise$individual,
                             indiv_only = COINold$Method$normalise$indiv_only,
                             out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_normalise", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_normalise)
          }
        }

      } else if (fi=="aggregate"){

        # run aggregate
        COIN <- aggregate(COIN, agtype = COINold$Method$aggregate$agtype,
                             agweights = COINold$Method$aggregate$agweights,
                             dset = COINold$Method$aggregate$dset,
                             agtype_bylevel = COINold$Method$aggregate$agtype_bylevel,
                             agfunc = COINold$Method$aggregate$agfunc,
                             out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("after_aggregate", COINold$Method$Custom)){
            eval(COINold$Method$Custom$after_aggregate)
          }
        }

      } else {
        stop("Method type not recognised...")
      }

    }

  }



  return(COIN)

}


#' Add and remove indicators
#'
#' A shortcut function to add and remove indicators. This will make the relevant changes
#' and recalculate the index if asked. Adding and removing is done relative to the current set of
#' indicators used in calculating the index results. Any indicators that are added must of course be
#' present in `.$Input$Original` (in both `IndData` and `IndMeta`).
#'
#' @param COIN COIN object
#' @param add A character vector of indicator codes to add (must be present in the original input data)
#' @param drop A character vector of indicator codes to remove (must be present in the original input data)
#' @param regen Logical (default): if `TRUE`, automatically regenerates the results based on the new specs
#' Otherwise, just updates the `.$Method$assemble` parameters. This latter might be useful if you want to
#' Make other changes before re-running using the [regen()] function.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # build ASEM example
#' ASEM <- build_ASEM()
#' # remove one indicator and regenerate results
#' ASEM2 <- indChange(ASEM, drop = "UNVote", regen = TRUE)
#' # compare the differences
#' CT <- compTable(ASEM, ASEM2, dset = "Aggregated", isel = "Index")
#'
#' @seealso
#' * [regen()] regenerate a COIN
#' * [compTable()] compare two different COINs
#' * [compTableMulti()] compare multiple COINs
#'
#' @return An updated COIN, with regenerated results if `regen = TRUE`.
#'
#' @export

indChange <- function(COIN, add = NULL, drop = NULL, regen = FALSE){

  # The logic here is to edit the COIN$Method parameters, then perform a rerun

  # ADDING INDICATORS
  if(!is.null(add)){
    # assume when we add, we take the existing indicators and add specified ones
    COIN$Method$assemble$include <- c(COIN$Parameters$IndCodes, add)
    COIN$Method$assemble$exclude <- NULL
  }

  # DROPPING INDICATORS
  if(!is.null(drop)){
    # when we drop an indicator, this is relative to the existing COIN object, not to the
    # original input data. Therefore, the best approach is to use the "include" option again.
    # use the existing vector of ind codes, minus the indicators to drop
    COIN$Method$assemble$include <- setdiff(COIN$Parameters$IndCodes, drop)
    COIN$Method$assemble$exclude <- NULL
  }

  # REGEN if asked (nicely)
  if(regen==TRUE){
    COIN <- regen(COIN, quietly = TRUE)
    message("COIN has been regenerated using new specs.")
  } else {
    message("COIN parameters changed but results NOT updated. Use COINr::regen() to regenerate
results or set regen = TRUE in indChange().")
  }

  COIN
}

