#' ASEM raw indicator data
#'
#' A dataset containing raw values of indicators for 51 countries, groups and denominators. See the ASEM Portal
#' for further information and detailed description of each indicator, and COINr documentation for the formatting
#' of this data set.
#'
#' @format A data frame with 51 rows and 60 variables:
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEMIndData"

#' ASEM indicator metadata
#'
#' This contains all metadata for ASEM indicators, including names, weights, directions, etc. See the ASEM Portal
#' for further information and detailed description of each indicator, and COINr documentation for the formatting
#' of this data set.
#'
#' @format A data frame with 49 rows and 7 variables:
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEMIndMeta"

#' ASEM aggregate metadata
#'
#' This contains all the metadata for the aggregate groups, including the names, weights and codes.
#'
#' @format A data frame with 8 rows and 9 variables:
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEMAggMeta"

#' World denomination data
#'
#' A small selection of common denominator indicators, which includes GDP, Population, Area, GDP per capita
#' and income group. All data sourced from the World Bank as of Feb 2020 (data is typically from 2019).
#'
#' @format A data frame with 249 rows and 7 variables.
#'
#' @source \url{https://data.worldbank.org/}
"WorldDenoms"
