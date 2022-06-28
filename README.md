<!-- README.md is generated from README.Rmd. Please edit that file -->

# COINr <img src="man/figures/COINr_logo.png" width="121px" height="140px" align="right" style="padding-left:10px;background-color:white;" />

<!-- badges: start -->

[![CRAN-update](https://www.r-pkg.org/badges/version-ago/COINr)](https://cran.r-project.org/package=COINr)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/COINr)](https://CRAN.R-project.org/package=COINr)
<!-- badges: end -->
=======
# COINr <img src="man/images/COINr_logo.png" width="121px" height="140px" align="right" style="padding-left:10px;background-color:white;" />

## ANNOUNCEMENT

COINr is undergoing some major updates which will introduce very **significant changes to the syntax** of the package. This is is a one-off change, after which the package will become stable and backwards-compatible.

The new features of COINr will include:

* Full support for panel data (composite indicators over time)
* Much more robust plus many fewer package dependencies, so easier to install
* New static plots
* Improved consistent and aligned syntax
* More flexible core functions
* Interactive apps will be moved to a separate (non-CRAN) package which can be installed as an add-on

The updated package will be released around May 2022. Apologies for the disruption this will cause but the new features should be worth it. You can always roll back to the older version of COINr if you prefer.

Peace.

## About

COINr is a high-level R package which is the first fully-flexible
development and analysis environment for composite indicators and
scoreboards. The main features can be summarised as features for
*building*, features for *analysis* and features for *visualisation and
presentation*.

**Building features**:

-   Flexible and fast development of composite indicators with no limits
    on aggregation levels, numbers of indicators, highly flexible set of
    methodological choices.
-   Denomination by other indicators (including built in world
    denominators data set)
-   Screening units by data requirements
-   Imputation of missing data, by a variety of methods
-   Data treatment using Winsorisation and nonlinear transformations
-   Normalisation by more than ten methods, either for all indicators or
    for each individually
-   Weighting using either manual weighting, PCA weights or correlation
    optimised weights.
-   Aggregation of indicators using a variety of methods which can be
    different for each aggregation level.

**Analysis features:**

-   Detailed indicator statistics, and data availability within
    aggregation groups
-   Multivariate analysis, including quick functions for PCA, and a
    detailed correlation analysis and visualisation
-   Easy “what if” analysis - very quickly checking the effects of
    adding and removing indicators, changing weights, methodological
    variations
-   Full global uncertainty and sensitivity analysis which can check the
    impacts of uncertainties in weighting and many methodological
    choices

**Visualisation and presentation:**

-   Statistical plots of indicators - histograms, violin plots, dot
    plots, scatter plots and more
-   Bar charts, stacked bar charts and tables for presenting indicator
    data and making comparisons between units
-   Correlation plots for visualising correlations between indicators
    and between aggregation levels

COINr also allows fast import from the [COIN
Tool](https://knowledge4policy.ec.europa.eu/composite-indicators/coin-tool_en)
and fast export to Excel.

In short, COINr aims to allow composite indicators to be developed and
prototyped very quickly and in a structured fashion. As of v1.0 it has
support for panel data.

## Installation

COINr is on CRAN and can be installed by running:

``` r
# Install released version from CRAN
install.packages("COINr")
```

The development version, which may be slightly more up-to-date, can be
installed from GitHub:

``` r
# Install development version from GitHub
devtools::install_github("bluefoxr/COINr")
```

This should directly install the package from Github, without any other
steps. You may be asked to update packages. This might not be strictly
necessary, so you can also try skipping this step.

## Getting started

A good place to get started is COINr’s “Overview” vignette. Try
`vignette("overview")`.

The most thorough documentation is available at [COINr’s
website](https://bluefoxr.github.io/COINr/) (developed using pkgdown).
This contains all package documentation in an easy-to-navigate format.
All documentation available here is also available by browsing COINr
vignettes: see `vignette(package = "COINr")`.

## Recent updates

COINr has been recently updated to v1.0, skipping a few version numbers.
This has brought in many new features, some discarded features, less
dependencies and more robust underlying code. The syntax has also been
changed to make the package more consistent. See `vignette("v1")` to
learn about these changes if you were using COINr prior to v1.0.

COINr documentation was previously contained in an [online
book](https://bluefoxr.github.io/COINrDoc/). This is still available,
and although the principles of composite indicators there are still all
valid, the code refers strictly to COINr \< v.1.0.

If you prefer to roll back to the old COINr, you can still install it as
a separate package called “COINr6”. This is available on GitHub:

``` r
remotes::install_github("bluefoxr/COINr6")
```
