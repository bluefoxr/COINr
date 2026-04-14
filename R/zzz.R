# .onAttach <- function(...) {
#   packageStartupMessage("COINr syntax has significantly changed. See vignette('v1') for details.
# This message will be removed in future updates.")
# }

.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  if (.Platform$OS.type == "windows") {
    # Default to SnowParam on Windows
    .pkg_env$bpparam <- BiocParallel::SnowParam()
  } else {
    # Default to MulticoreParam on Unix-like systems
    .pkg_env$bpparam <- BiocParallel::MulticoreParam()
  }
}
