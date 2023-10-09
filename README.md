
<!-- README.md is generated from README.Rmd. Please edit that file -->

# COINr <img src="man/figures/COINr_logo.png" width="133px" height="154px" align="right" style="padding-left:10px;background-color:white;" />

<!-- badges: start -->

[![CRAN-update](https://www.r-pkg.org/badges/version-ago/COINr)](https://cran.r-project.org/package=COINr)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/COINr)](https://CRAN.R-project.org/package=COINr)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.04567/status.svg)](https://doi.org/10.21105/joss.04567)
[![R-CMD-check](https://github.com/bluefoxr/COINr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bluefoxr/COINr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bluefoxr/COINr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bluefoxr/COINr?branch=master)
<!-- badges: end -->

**Full documentation is available at [COINr’s
website](https://bluefoxr.github.io/COINr/)**

COINr is a high-level R package which offers a fully-flexible
development and analysis environment for composite indicators and
scoreboards. It is inspired by, and broadly follows the methodology of
the [JRC/OECD Handbook on Composite
Indicators](https://publications.jrc.ec.europa.eu/repository/handle/JRC47008)
and the [Competence Centre for Composite Indicators and
Scoreboards](https://knowledge4policy.ec.europa.eu/composite-indicators_en),
which also supported the development of the package (see
[Acknowledgements](#acknowledgements)).

The main features can be summarised as features for *building*, features
for *analysis* and features for *visualisation and presentation*.

**Building features**:

- Flexible and fast development of composite indicators with no limits
  on aggregation levels, numbers of indicators, highly flexible set of
  methodological choices.
- Denomination by other indicators
- Screening units by data requirements
- Imputation of missing data, by a variety of methods
- Data treatment using Winsorisation and nonlinear transformations
- Normalisation (scaling) using a variety of methods
- Weighting using either manual weighting, PCA weights or
  correlation-optimised weights.
- Aggregation of indicators using a variety of methods which can be
  different for each aggregation level.

**Analysis features:**

- Detailed indicator statistics, and data availability within
  aggregation groups
- Multivariate analysis, including quick functions for PCA, and a
  detailed correlation analysis and visualisation
- Easy “what if” analysis - very quickly checking the effects of adding
  and removing indicators, changing weights, methodological variations
- Full global uncertainty and sensitivity analysis which can check the
  impacts of uncertainties in weighting and many methodological choices

**Visualisation and presentation:**

- Statistical plots of indicators - histograms, violin plots, dot plots,
  scatter plots and more
- Bar charts, stacked bar charts and tables for presenting indicator
  data and making comparisons between units
- Correlation plots for visualising correlations between indicators and
  between aggregation levels

COINr also allows fast import from the [COIN
Tool](https://knowledge4policy.ec.europa.eu/composite-indicators/coin-tool_en)
and fast export to Excel.

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

COINr needs a little reading and learning to understand properly. But
once you have done that, it can be very powerful for developing
composite indicators.

A good place to get started is COINr’s “Overview” vignette. Try
`vignette("overview")`.

The most thorough documentation is available at [COINr’s
website](https://bluefoxr.github.io/COINr/) (developed using pkgdown).
This contains all package documentation in an easy-to-navigate format.
All documentation available here is also available by browsing COINr
vignettes: see `vignette(package = "COINr")`.

## Tutorials

COINr was presented at the European Commission’s [training week on
composite indicators and
scoreboards](https://knowledge4policy.ec.europa.eu/composite-indicators/2023-jrc-week-composite-indicators-scoreboards_en#videos)
in September 2023, including a short one hour tutorial:

- See the presentation video
  [here](https://webcast.ec.europa.eu/2023-jrc-week-on-composite-indicators-and-scoreboards-2023-09-28)
  (COINr presentation starts around 16:28 but if you are new to
  composite indicators it is well worth following the full training)
- See the tutorial document
  [here](https://bluefoxr.github.io/COINr-Demo-2023/COINr_demo.html) and
  the code in the [GitHub repo
  here](https://github.com/bluefoxr/COINr-Demo-2023).

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

# Help and issues

For general help with COINr, the best place to look is the package’s
documentation which is available either via the command line
(`vignette(package = "COINr")`) or by checking individual function
documentation (`?function_name`). All documentation is also conveniently
available online at [COINr’s
website](https://bluefoxr.github.io/COINr/).

If you find any problems with the package, including bugs or
suggestions, either open a GitHub issue here, or else contact me by
email.

Finally, contributions to the package are most welcome. This should be
done by cloning the repo, making your modifications, and then opening a
pull request. You could also contact me in advance to discuss changes
and extensions. Any changes (especially new functions) should be
accompanied by unit tests, and all existing tests should run without
errors or warnings. To do this, run:

``` r
devtools::test()
```

# Citing COINr

If you have found COINr helpful, we are grateful if you cite the
package. COINr is citable by a paper in the Journal of Open Source
Software which you can find
[here](https://joss.theoj.org/papers/10.21105/joss.04567) (with citation
information).

In R you can also generate the citation info using
`citation(package = "COINr")`, or see the same info on the [COINr web
page](https://bluefoxr.github.io/COINr/authors.html#citation).

# Acknowledgements

COINr was initially developed under contract for the European
Commission’s Joint Research Centre until 2021, and this enabled the bulk
of the initial development, as well as many helpful discussions with
colleagues from the [Competence Centre for Composite Indicators and
Scoreboards](https://knowledge4policy.ec.europa.eu/composite-indicators_en),
and this is gratefully acknowledged. Since then, the package has been
continuously modified and improved while working on other projects and
following user feedback.
