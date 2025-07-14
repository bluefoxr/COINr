#' Get the default BiocParallel backend for this package
#'
#' @export
#' @return A BiocParallelParam object.
getPackageBPPARAM <- function() {
  .pkg_env$bpparam
}

#' Set the default BiocParallel backend for this package
#'
#' @param param A BiocParallelParam object.
#' @export
#' @return The previously set BiocParallelParam object (invisibly).
setPackageBPPARAM <- function(param) {
  if (!inherits(param, "BiocParallelParam")) {
    stop("The provided object is not a BiocParallelParam object.")
  }
  old_param <- .pkg_env$bpparam
  .pkg_env$bpparam <- param
  invisible(old_param)
}
