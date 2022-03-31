
#' Convert a numeric sample to parameter values
#'
#' Converts a numeric sample `x`, which should have values between 0 and 1, to a corresponding vector or list of
#' parameter values, based on `distribution`.
#'
#' The `distribution` argument specifies how to map `x` to parameter values and can be used in two different ways,
#' depending on `dist_type`. If `dist_type = "discrete"`, then `distribution` should be a vector or list of alternative
#' parameter values (or objects). Each entry of `x` is mapped to an entry from `distribution` by treating `distribution`
#' as a discrete uniform distribution over its entries.
#'
#' If `dist_type = "continuous"`, `distribution` is assumed to be a continuous uniform distribution, such that
#' `distribution` is a 2-length numeric vector with the first value being the lower bound, and the second value the
#' upper bound. For example, if `distribution = c(5, 10)`, then `x` will be mapped onto a continuous uniform distribution
#' bounded by 5 and 10.
#'
#' @param distribution The distribution to sample using `x` - see details.
#' @param dist_type Either `"discrete"` or `"continuous"` - see details.
#' @param checks Logical: if `TRUE` runs some checks on inputs, set to `FALSE` to increase speed slightly.
#' @param x A numeric vector with values between 0 and 1
#'
#' @return
#' @export
#'
#' @examples
#' #
sample_2_para <- function(x, distribution, dist_type = "discrete", checks = TRUE){

  if(checks){
    stopifnot(is.numeric(x),
              any(x >= 0),
              any(x <= 1),
              is.character(dist_type),
              length(dist_type) == 1,
              dist_type %in% c("discrete", "continuous"))
  }

  # specs can be a set of discrete alternatives, or else a uniform distribution
  if(dist_type == "discrete"){

    # the number of discrete alternatives
    n_alt <- length(distribution)
    # convert x to indexes of the discrete parameters
    i_para <- cut(x, n_alt, 1:n_alt)
    # now get the output vector/list
    l_out <- distribution[i_para]

  } else {

    # here we assume a uniform distribution
    if(checks){
      stopifnot(is.numeric(distribution),
                length(distribution) == 2,
                distribution[2] > distribution[1])
    }

    # we simply scale x up to the interval covered by the distribution
    l_out <- x*(distribution[2] - distribution[1]) + distribution[1]

  }

  # output
  l_out


}


#' Edit objects inside a coin
#'
#' Changes the object found at `address` to `new_value`.
#'
#' @param coin A coin
#' @param address A string specifying the location in the coin of the object to edit. This should begin with `"$"`, omitting the coin itself
#' in the address. E.g. if you target `coin$x$y$z` enter `"$x$y$z"`.
#' @param new_value The new value to assign at `address`.
#' @param checks Logical: if `TRUE`, runs some basic checks, otherwise omitted if `FALSE`. Setting `FALSE` may speed
#' things up a bit in sensitivity analysis, for example.
#'
#' @return An updated coin
#' @export
#'
#' @examples
#' #
edit_coin <- function(coin, address, new_value, checks = TRUE){

  # checks
  if(checks){
    check_coin_input(coin)
    stopifnot(is.character(address),
              length(address) == 1,
              substr(address,1,1) == "$")
  }

  # this is the call to evaluate, as a string
  expr_str <- paste0("coin", address, " <- new_value")
  # evaluate the call
  eval(str2lang(expr_str))

  # output
  coin

}
