#' ASEM raw panel data
#'
#' This is an artificially-generated set of panel data (multiple observations of indicators over time) that
#' is included to build the example "purse" class, i.e. to build composite indicators over time. This will
#' eventually be replaced with a better example, i.e. a real data set.
#'
#' This data set is in the new v1.0 format.
#'
#' @format A data frame with 255 rows and 60 variables.
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEM_iData_p"

#' ASEM raw indicator data
#'
#' A data set containing raw values of indicators for 51 countries, groups and denominators. See the ASEM Portal
#' for further information and detailed description of each indicator. See also `vignette("coins")` for the format
#' of this data.
#'
#' This data set is in the new v1.0 format.
#'
#' @format A data frame with 51 rows and 60 variables.
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEM_iData"

#' ASEM indicator metadata
#'
#' This contains all metadata for ASEM indicators, including names, weights, directions, etc. See the ASEM Portal
#' for further information and detailed description of each indicator.
#' See also `vignette("coins")` for the format
#' of this data.
#'
#' This data set is in the new v1.0 format.
#'
#' @format A data frame with 68 rows and 9 variables
#'
#' @source \url{https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#aggregation-metadata}
"ASEM_iMeta"

#' World denomination data
#'
#' A small selection of common denominator indicators, which includes GDP, Population, Area, GDP per capita
#' and income group. All data sourced from the World Bank as of Feb 2021 (data is typically from 2019). Note that this is
#' intended as example data, and it would be a good idea to use updated data from the World Bank when needed. In this
#' data set, country names have been altered slightly so as to include no accents - this is simply to make it more
#' portable between distributions.
#'
#' @format A data frame with 249 rows and 7 variables.
#'
#' @source \url{https://data.worldbank.org/}
"WorldDenoms"

#' ASEM COIN (COINr < v1.0)
#'
#' This is an "old format" "COIN" object which is stored for testing purposes.
#' It is generated using the COINr6 package (only available on GitHub) using
#' `COINr6::build_ASEM()`
#'
#' @format A "COIN" class object
#'
#' @source \url{https://github.com/bluefoxr/COINr6}
"ASEM_COIN"
