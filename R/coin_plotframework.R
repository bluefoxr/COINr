#' Interactive sunburst plot of index structure
#'
#' Plots the structure of the index using a sunburst plot using **plotly**. Output can be used as an interactive plot
#' in html documents, e.g. via R Markdown.
#'
#' Note that this plot is sensitive to the *order* of the elements. If you use assemble() and input
#' a COIN, this plot should work automatically. If you input a list, you should make sure that the indicator metadata
#' is ordered by descending order of the hierarchy (i.e. highest level, working downwards).
#'
#' @param COIN COIN object, or list with first entry is the indicator metadata, second entry is the aggregation metadata
#'
#' @importFrom dplyr select starts_with pull ends_with
#' @importFrom plotly plot_ly
#' @importFrom matrixStats rowProds
#' @importFrom purrr map_dfr
#'
#' @examples \dontrun{
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # plot framework
#' plotframework(ASEM)}
#'
#' @return Interactive sunburst plot. This can be edited further with **plotly** commands.
#'
#' @export

plotframework <- function(COIN){

  # get effective weights, labels and parents
  outW <- effectiveWeight(COIN)

  fig <- plotly::plot_ly(
    labels = outW$LabelsParents$Labels,
    parents = outW$LabelsParents$Parents,
    values = outW$EffectiveWeights,
    type = 'sunburst',
    branchvalues = 'total'
  )

  fig

}


#' Effective weights
#'
#' This calculates the effective weights of each element in the indicator hierarchy. This is
#' useful for understanding e.g. the true weight of each indicator in the framework and is also
#' used in `plotframework()`.
#'
#' @param COIN COIN object, or list with first entry is the indicator metadata, second entry is the aggregation metadata
#'
#' @importFrom dplyr select starts_with pull ends_with
#' @importFrom plotly plot_ly
#' @importFrom matrixStats rowProds
#' @importFrom purrr map_dfr
#'
#' @examples \dontrun{
#' # build ASEM COIN
#' ASEM <- assemble(IndData = ASEMIndData, IndMeta = ASEMIndMeta, AggMeta = ASEMAggMeta)
#' # get effective weights
#' effwts <- effectiveWeight(ASEM)}
#'
#' @return A list with effective weights, as well as a data frame with labels and parents for the
#' sunburst plot.
#'
#' @export

effectiveWeight <- function(COIN){

  # the structure is in the metadata and framework data frames
  if ("COIN" %in% class(COIN)){ # COIN obj

    metad<-COIN$Input$IndMeta
    fwk <- COIN$Input$AggMeta

  } else if ("list" %in% class(COIN) & length(COIN)==2) { # we expect a list with 2 elements in it

    metad<-COIN[[1]]
    fwk<-COIN[[2]]

  } else { # no clue what it is

    stop("Input data type not recognised. Should be COIN object or list of indmeta/aggmeta data frames.")
  }

  # isolate columns with aggregation labels in them, and indictaor labels
  agg_cols <- metad %>% dplyr::select(dplyr::starts_with("Agg"))

  agg_cols <- cbind(Code=metad$IndCode, agg_cols)
  #agg_cols <- agg_cols %>% add_column(Code=metad$IndCode, .before = 1)
  # get weights for aggregation groups
  #wt_cols <- fwk %>% dplyr::select(dplyr::ends_with("Weight"))

  # preallocate some variables
  lbls <- NULL
  prnts <- NULL
  wts <- NULL
  for (ii in 1:ncol(agg_cols)){

    # add labels
    lbls <- c(lbls,dplyr::pull(agg_cols,ii))

    # add weights, but adjust so sum to 1 inside their aggregation group. Slightly fiddly.

    if (ii==1){ # indicators

      wcol <- metad$IndWeight # the indicator weights
      prnt <- dplyr::pull(agg_cols,ii+1) # the column specifying parent agg groups
      prntu <- unique(prnt) # the parent agg groups, duplicates removed

      # now loop over aggregation groups
      wts_level <- NULL # preallocating for just the weights in the level
      for (jj in 1:length(prntu)){
        wgroup <- wcol[prnt == prntu[jj]] # only the weights in the agg group
        wts_level <- c(wts_level,wgroup/sum(wgroup, na.rm = T)) # normalise to 1 and add to weight vector
      }
      wts <- c(wts,wts_level) # have to make sure weights add up to 1 at every level

    } else { # aggregation groups

      # wcol <- dplyr::pull(wt_cols,ii-1) # the aggregation level weights
      # wcol <- wcol[!is.na(wcol)]
      wcol <- fwk$Weight[fwk$AgLevel==ii] # the indicator weights
      agg_col <- dplyr::pull(agg_cols,ii) # the column with the aggregation group labels in
      if (ii<ncol(agg_cols)){
        prnt <- dplyr::pull(agg_cols,ii+1) # the column specifying parent agg groups
      } else {
        prnt <- rep(1,length(agg_col)) # if we reached the root, use a dummy vector
      }

      chld_prnt <- unique(data.frame(agg_col,prnt)) # put together and remove duplicates

      prntu <- unique(prnt) # the parent agg groups, duplicates removed

      wts_level <- NULL # preallocating for just the weights in the level
      for (jj in 1:length(prntu)){
        wgroup <- wcol[chld_prnt$prnt == prntu[jj]] # only the weights in the agg group
        wts_level <- c(wts_level,wgroup/sum(wgroup, na.rm = T)) # normalise to 1 and add to weight vector
      }
      wts <- c(wts,wts_level) # have to make sure weights add up to 1 at every level
    }

    # add parents
    if (ii==ncol(agg_cols)){
      prnts <- c(prnts,rep("",nrow(agg_cols)))
    } else {
      prnts<-c(prnts,dplyr::pull(agg_cols,ii+1))
    }
  }

  # now we have to go back through again to get the effective weights in the index...
  wts_eff <- NULL # preallocate
  for (jj in 1:ncol(agg_cols)){
    # get level plus all parent levels
    pcols <- dplyr::select(agg_cols,jj:ncol(agg_cols)) %>% unique()
    # sust names for weights of each element: this is the agg cols but with names substituted with normalised weights
    pcols_w <- purrr::map_dfr(pcols, ~wts[match(.x, unique(lbls))])
    # get effective weights by multiplying columns
    wts_eff <- c(wts_eff,matrixStats::rowProds(as.matrix(pcols_w)))
  }

  lbls_prnts <- data.frame(Labels = lbls,
                           Parents = prnts) %>% unique() # remove repeated rows

  # also get effective weights as structured list, this is more useful outside of plotframework()
  if ("COIN" %in% class(COIN)){ # COIN obj

    wts_eff_list <- COIN$Parameters$Weights$Original
    wts_eff_list$Weight <- wts_eff
    colnames(wts_eff_list)[3] <- "EffectiveWeight"

    return(list(EffectiveWeights = wts_eff,
                LabelsParents = lbls_prnts,
                EffectiveWeightsList = wts_eff_list))

  } else {

    return(list(EffectiveWeights = wts_eff,
                LabelsParents = lbls_prnts))
  }

}



