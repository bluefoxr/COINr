# GENERAL UTILITY FUNCTIONS

#' Not in operator
#'
#' For convenience, rather than always `!(x, %in% y)`
#'
#' @param x A scalar or vector
#' @param y A scalar or vector
#'
#' @return TRUE if x is not in y, FALSE otherwise
'%nin%' <- function(x,y){
  !('%in%'(x,y))
}


#' rbind two lists with different names into a data frame
#'
#' Performs an `rbind()` operation on two named lists or vectors that do not need to share the same names, but
#' will match the names and fill any missing cols with `NA`s.
#'
#' @param x1 A named list or named vector
#' @param x2 Another named list or named vector
#'
#' @examples
#' #
#'
#' @return Data frame
#'
#' @export
rbind_fill <- function(x1, x2){

  if(is.null(names(x1)) || is.null(names(x2))){
    stop("names of x1 or x2 is NULL")
  }

  # make to dfs
  x1 <- as.data.frame(as.list(x1))
  x2 <- as.data.frame(as.list(x2))

  # fill with NAs
  x1[setdiff(names(x2), names(x1))] <- NA
  x2[setdiff(names(x1), names(x2))] <- NA

  rbind(x1, x2)

}


#' Remove empty components from list
#'
#' Short cut for removing any empty components of a list
#'
#' @param l A list
#'
#' @examples
#' #
#'
#' @return List with empty bits removed
tidy_list <- function(l){
  l[lengths(l) > 0]
}


#' Check availability of function
#'
#' Checks if a function is available, and returns an error if not.
#'
#' @param f_name
#'
#' @return Nothing or error
check_fname <- function(f_name){
  if(!(exists(f_name, mode = "function"))){
    stop("function '", f_name, "' not found. must be an accessible function.")
  }
}

#' Set default arg
#'
#' A shortcut
#'
#' @param x The argument
#' @param x_default The default to set
#'
#' @return the parameter
set_default <- function(x, x_default){
  if(is.null(x)){
    x_default
  } else {
    x
  }
}


#' Make correlation matrix long
#'
#' Only for correlation matrices: make long to avoid reshape2 package or similar.
#'
#' @param X a square correlation matrix
#'
#' @return A long format data frame
lengthen <- function(X){

  # make df
  X <- as.data.frame(X)

  # stack and add names
  X1 <- cbind(stack(X), rownames(X))
  names(X1) <- c("Value", "V2", "V1")
  rev(X1)

}
