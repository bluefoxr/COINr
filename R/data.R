#' ASEM raw indicator data
#'
#' A data set containing raw values of indicators for 51 countries, groups and denominators. See the ASEM Portal
#' for further information and detailed description of each indicator, and
#' [COINr documentation](https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#indicator-data) for the formatting
#' of this data set.
#'
#' This data set is in the new v0.8 format.
#'
#' @format A data frame with 51 rows and 60 variables.
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEM_iData"

#' ASEM indicator metadata
#'
#' This contains all metadata for ASEM indicators, including names, weights, directions, etc. See the ASEM Portal
#' for further information and detailed description of each indicator, and
#' [COINr documentation](https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#indicator-metadata) for the formatting
#' of this data set.
#'
#' This data set is in the new v0.8 format.
#'
#' @format A data frame with 68 rows and 9 variables
#'
#' @source \url{https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#aggregation-metadata}
"ASEM_iMeta"

#' ASEM raw indicator data
#'
#' A data set containing raw values of indicators for 51 countries, groups and denominators. See the ASEM Portal
#' for further information and detailed description of each indicator, and
#' [COINr documentation](https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#indicator-data) for the formatting
#' of this data set.
#'
#' @format A data frame with 51 rows and 60 variables.
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEMIndData"

#' ASEM indicator metadata
#'
#' This contains all metadata for ASEM indicators, including names, weights, directions, etc. See the ASEM Portal
#' for further information and detailed description of each indicator, and
#' [COINr documentation](https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#indicator-metadata) for the formatting
#' of this data set.
#'
#' @format A data frame with 49 rows and 9 variables
#'
#' @source \url{https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#aggregation-metadata}
"ASEMIndMeta"

#' ASEM aggregate metadata
#'
#' This contains all the metadata for the aggregate groups, including the names, weights and codes. See the ASEM Portal
#' for further information and detailed description of each indicator, and
#' [COINr documentation](https://bluefoxr.github.io/COINrDoc/coins-the-currency-of-coinr.html#indicator-data) for the formatting
#' of this data set.
#'
#' @format A data frame with 8 rows and 9 variables:
#'
#' @source \url{https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository}
"ASEMAggMeta"

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
