# UTILITY FUNCTIONS

#' Write function arguments to log
#'
#' This used inside `build_*` functions. It takes the coin object as an input, then writes the arguments of the
#' current function into the `.$Log` list of the coin object. This is then used as a record of the operations used
#' to build the coin, and can be edited.
#'
#' @param coin A coin class object
#'
#' @examples
#' #
#'
#' @return Updated GII2 object with function arguments written to `.$Log`
write_log <- function(coin){

  # get calling function name and its arguments
  func_args <- as.list(sys.frame(-1))
  func_name <- deparse(as.list(sys.call(-1))[[1]])

  # tweak list first
  func_args <- func_args[!(names(func_args) %in% c("coin", "*tmp*"))]

  # check that we are getting function arguments and nothing else
  if(!all(names(func_args) %in% names(formals(func_name)))){
    stop(paste0("Mismatch between function arguments of ", func_name, " and attempt to write to .$Log."))
  }

  # write to coin
  coin$Log[[func_name]] <- func_args
  coin

}


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
