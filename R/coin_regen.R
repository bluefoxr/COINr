#' Regenerate COIN object
#'
#' Function to regenerate the results of the COIN, using the methodological parameters stored in
#' .$Method. This function calls the construction functions of COINr in the order that they are
#' found in .$Method, along with any custom code found in .$Method$Custom.
#'
#' @param COINold COIN object containing specs on how to regenerate
#' @param quietly Logical: if TRUE suppresses all messages from COINr functions (warnings may still occur though)
#'
#' @return An updated COIN object, recalculated
#'
#' @export

regen <- function(COINold, quietly = FALSE){

  if (quietly){
    # Assemble always comes first.
    COINnew <- assemble(IndData = COINold$Input$Original$IndData,
                        IndMeta = COINold$Input$Original$IndMeta,
                        AggMeta = COINold$Input$Original$AggMeta,
                        include = COINold$Method$Assemble$include,
                        exclude = COINold$Method$Assemble$exclude) %>% suppressMessages()

    # optional custom operation
    if (exists("Custom",COINold$Method)){
      if (exists("AfterAssemble", COINold$Method$Custom)){
        eval(COINold$Method$Custom$AfterAssemble) %>% suppressMessages()
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
      if ((fi == "Custom")|(fi == "Assemble")){next}

      # Now we have to see which function to run.
      if(fi=="Screening"){

        # run checkData
        COINnew <- checkData(COINnew, dset = COINold$Method$Screening$dset,
                             ind_thresh = COINold$Method$Screening$ind_thresh,
                             unit_screen = COINold$Method$Screening$unit_screen,
                             Force = COINold$Method$Screening$Force,
                             out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterScreening", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterScreening) %>% suppressMessages()
          }
        }

      } else if (fi=="Denomination"){

        # run denominate
        COINnew <- denominate(COINnew, dset = COINold$Method$Denomination$dset,
                              specby = COINold$Method$Denomination$specby,
                              denomby = COINold$Method$Denomination$denomby,
                              denominators = COINold$Method$Denomination$denominators,
                              out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterDenomination", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterDenomination) %>% suppressMessages()
          }
        }

      } else if (fi=="Imputation"){

        # run impute
        COINnew <- impute(COINnew, imtype = COINold$Method$Imputation$imtype,
                          dset = COINold$Method$Imputation$dset,
                          groupvar = COINold$Method$Imputation$groupvar,
                          byyear = COINold$Method$Imputation$byyear,
                          EMaglev = COINold$Method$Imputation$EMaglev,
                          out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterImputation", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterImputation) %>% suppressMessages()
          }
        }

      } else if (fi=="Treatment"){

        # run treat
        COINnew <- treat(COINnew, dset = COINold$Method$Treatment$dset,
                         winmax = COINold$Method$Treatment$winmax,
                         winchange = COINold$Method$Treatment$winchange,
                         deflog = COINold$Method$Treatment$deflog,
                         boxlam = COINold$Method$Treatment$boxlam,
                         t_skew = COINold$Method$Treatment$t_skew,
                         t_kurt = COINold$Method$Treatment$t_kurt,
                         individual = COINold$Method$Treatment$individual,
                         indiv_only = COINold$Method$Treatment$indiv_only) %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterTreatment", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterTreatment) %>% suppressMessages()
          }
        }

      } else if (fi=="Normalisation"){

        # run normalise
        COINnew <- normalise(COINnew, ntype = COINold$Method$Normalisation$ntype,
                             npara = COINold$Method$Normalisation$npara,
                             dset = COINold$Method$Normalisation$dset,
                             directions = COINold$Method$Normalisation$directions,
                             individual = COINold$Method$Normalisation$individual,
                             indiv_only = COINold$Method$Normalisation$indiv_only,
                             out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterNormalisation", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterNormalisation) %>% suppressMessages()
          }
        }

      } else if (fi=="Aggregation"){

        # run aggregate
        COINnew <- aggregate(COINnew, agtype = COINold$Method$Aggregation$agtype,
                             agweights = COINold$Method$Aggregation$agweights,
                             dset = COINold$Method$Aggregation$dset,
                             agtype_bylevel = COINold$Method$Aggregation$agtype_bylevel,
                             agfunc = COINold$Method$Aggregation$agfunc,
                             out2 = "COIN") %>% suppressMessages()

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterAggregation", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterAggregation) %>% suppressMessages()
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
    COINnew <- assemble(IndData = COINold$Input$Original$IndData,
                        IndMeta = COINold$Input$Original$IndMeta,
                        AggMeta = COINold$Input$Original$AggMeta,
                        include = COINold$Method$Assemble$include,
                        exclude = COINold$Method$Assemble$exclude)

    # optional custom operation
    if (exists("Custom",COINold$Method)){
      if (exists("AfterAssemble", COINold$Method$Custom)){
        eval(COINold$Method$Custom$AfterAssemble)
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
      if ((fi == "Custom")|(fi == "Assemble")){next}

      # Now we have to see which function to run.
      if(fi=="Screening"){

        # run checkData
        COINnew <- checkData(COINnew, dset = COINold$Method$Screening$dset,
                             ind_thresh = COINold$Method$Screening$ind_thresh,
                             unit_screen = COINold$Method$Screening$unit_screen,
                             Force = COINold$Method$Screening$Force,
                             out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterScreening", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterScreening)
          }
        }

      } else if (fi=="Denomination"){

        # run denominate
        COINnew <- denominate(COINnew, dset = COINold$Method$Denomination$dset,
                              specby = COINold$Method$Denomination$specby,
                              denomby = COINold$Method$Denomination$denomby,
                              denominators = COINold$Method$Denomination$denominators,
                              out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterDenomination", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterDenomination)
          }
        }

      } else if (fi=="Imputation"){

        # run impute
        COINnew <- impute(COINnew, imtype = COINold$Method$Imputation$imtype,
                          dset = COINold$Method$Imputation$dset,
                          groupvar = COINold$Method$Imputation$groupvar,
                          byyear = COINold$Method$Imputation$byyear,
                          EMaglev = COINold$Method$Imputation$EMaglev,
                          out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterImputation", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterImputation)
          }
        }

      } else if (fi=="Treatment"){

        # run treat
        COINnew <- treat(COINnew, dset = COINold$Method$Treatment$dset,
                         winmax = COINold$Method$Treatment$winmax,
                         winchange = COINold$Method$Treatment$winchange,
                         deflog = COINold$Method$Treatment$deflog,
                         boxlam = COINold$Method$Treatment$boxlam,
                         t_skew = COINold$Method$Treatment$t_skew,
                         t_kurt = COINold$Method$Treatment$t_kurt,
                         individual = COINold$Method$Treatment$individual,
                         indiv_only = COINold$Method$Treatment$indiv_only)

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterTreatment", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterTreatment)
          }
        }

      } else if (fi=="Normalisation"){

        # run normalise
        COINnew <- normalise(COINnew, ntype = COINold$Method$Normalisation$ntype,
                             npara = COINold$Method$Normalisation$npara,
                             dset = COINold$Method$Normalisation$dset,
                             directions = COINold$Method$Normalisation$directions,
                             individual = COINold$Method$Normalisation$individual,
                             indiv_only = COINold$Method$Normalisation$indiv_only,
                             out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterNormalisation", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterNormalisation)
          }
        }

      } else if (fi=="Aggregation"){

        # run aggregate
        COINnew <- aggregate(COINnew, agtype = COINold$Method$Aggregation$agtype,
                             agweights = COINold$Method$Aggregation$agweights,
                             dset = COINold$Method$Aggregation$dset,
                             agtype_bylevel = COINold$Method$Aggregation$agtype_bylevel,
                             agfunc = COINold$Method$Aggregation$agfunc,
                             out2 = "COIN")

        # optional custom operation
        if (exists("Custom",COINold$Method)){
          if (exists("AfterAggregation", COINold$Method$Custom)){
            eval(COINold$Method$Custom$AfterAggregation)
          }
        }

      } else {
        stop("Method type not recognised...")
      }

    }

  }



  return(COINnew)

}


#' Add and remove indicators
#'
#' A shortcut function to add and remove indicators. This will make the relevant changes
#' and recalculate the index if asked. Adding and removing is done relative to the current set of
#' indicators used in calculating the index results. Any indicators that are added must of course be
#' present in .$Input$Original (in both IndData and IndMeta).
#'
#' @param COIN COIN object (assumed to be fully built)
#' @param add A character vector of indicator codes to add (must be present in the original input data)
#' @param drop A character vector of indicator codes to remove (must be present in the original input data)
#' @param regen Logical (default): if TRUE, automatically regenerates the results based on the new specs
#' Otherwise, just updates the .$Method$Assemble parameters. This latter might be useful if you want to
#' Make other changes before re-running using the regen function.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples \dontrun{COIN <- indChange(COIN, add = "Ind1", drop = "Ind3")}
#'
#' @return An updated COIN, with regenerated results if regen = TRUE
#'
#' @export

indChange <- function(COIN, add = NULL, drop = NULL, regen = FALSE){

  # The logic here is to edit the COIN$Method parameters, then perform a rerun

  # ADDING INDICATORS
  if(!is.null(add)){
    # assume when we add, we take the existing indicators and add specified ones
    COIN$Method$Assemble$include <- c(COIN$Parameters$IndCodes, add)
    COIN$Method$Assemble$exclude <- NULL
  }

  # DROPPING INDICATORS
  if(!is.null(drop)){
    # when we drop an indicator, this is relative to the existing COIN object, not to the
    # original input data. Therefore, the best approach is to use the "include" option again.
    # use the existing vector of ind codes, minus the indicators to drop
    COIN$Method$Assemble$include <- setdiff(COIN$Parameters$IndCodes, drop)
    COIN$Method$Assemble$exclude <- NULL
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

