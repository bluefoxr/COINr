#' Normalise indicator data sets
#'
#' A dataset of indicators is normalised using one of several methods. This function also supports custom normalisation.
#'
#' @param COINobj Either the COIN object, or a data frame of indicator data
#' @param ntype The type of normalisation method. Either "minmax", "zscore", or "custom".
#' @param npara Supporting object for ntype.
#' @param inames Character vector of indicator names, indicating which columns to normalise. Use this if you only want to normalise certain columns, or you are inputting a data frame with some columns which are not to be normalised (e.g. country names, groups, ...)
#' @param dset The data set to normalise
#' @param directions A vector specifying the direction assigned to each indicator.
#' Needs to be the same length as the number of indicators, or the number of indicators in inames, if specified.
#'
#' @importFrom purrr "map2"
#' @importFrom purrr "modify"
#'
#' @examples \dontrun{df_norm <- coin_normalise(COINobj, ntype="minmax", npara = c(0,1))}
#'
#' @return An updated COIN object with .$Data$Normalised added.
#'
#' @export

coin_normalise <- function(COINobj, ntype="minmax", npara = NULL, inames = NULL,
                           dset = "Raw", directions = NULL){

  # First. check to see what kind of input we have.
  out <- coin_aux_objcheck(COINobj, dset = dset, inames = inames)
  ind_data_only <- out$ind_data_only
  ind_names <- out$ind_names

  if (is.null(directions) & out$otype == "COINobj"){ # if no directions are explicitly specified, but COINobj input

    if (exists("Direction", COINobj$Input$IndMeta)){ # check if available in COIN obj
      directions <- COINobj$Input$IndMeta$Direction
    } else { # if not, just set everything positive
      directions <- rep(1,length(ind_names))
      warning("No indicator directions found in COIN object. Using positive directions for all indicators.")
    }

  } else if (is.null(directions) & out$otype == "df"){

    directions <- rep(1,length(ind_names))
    warning("No indicator directions specified. Using positive directions for all indicators.")

  } # otherwise, directions will be taken from the function input.

  # nifty map2 implementation here. Multiply each column of ind_data_only by corresponding column of directions
  # This def works with minmax, check if works with other methods.
  ind_data_only <- map2(ind_data_only, directions, ~ .x*.y)

  if (ntype == "minmax"){

    if (is.null(npara)){ # default parameters
      npara <- c(0,100)
    }
    datamod<-modify(ind_data_only,~{ (.x-min(.x, na.rm = TRUE))/(max(.x, na.rm = TRUE)-min(.x, na.rm = TRUE))*(npara[2]-npara[1]) + npara[1]} )

  } else if (ntype == "zscore"){

    if (is.null(npara)){ # default parameters
      npara <- c(0,1)
    }
    datamod<-modify(ind_data_only,~{(.x-mean(.x, na.rm = TRUE))/sd(.x, na.rm = TRUE)})

  } else if (ntype == "custom"){

    datamod = tryCatch({
      modify(ind_data_only,npara)
    }, error = function(e) {
      stop("Error: custom function not valid for some reason.")
    })

  }

  if (is.data.frame(COINobj)){ # Data frame

    COINobj[ind_names] <- datamod

  } else {

    dataout <- out$ind_data
    dataout[out$ind_names] <- datamod
    COINobj$Data$Normalised <- dataout

    # Write inputs to Method
    COINobj$Method$Normalisation$ntype <- ntype
    COINobj$Method$Normalisation$npara <- npara
    COINobj$Method$Normalisation$inames <- out$ind_names
    COINobj$Method$Normalisation$dset <- dset
    COINobj$Method$Normalisation$directions <- directions
  }

  return(COINobj)

}
