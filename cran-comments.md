**Full documentation for COINr is available here:** 
https://bluefoxr.github.io/COINrDoc/


# About COINr

COINr is a high-level R package which is the first fully-flexible development and analysis environment for composite indicators and scoreboards. The main features can be summarised as features for *building*, features for *analysis* and features for *visualisation and presentation*.

**Building features**:

* Flexible and fast development of composite indicators with no limits on aggregation levels, numbers of indicators, highly flexible set of methodological choices.
* Denomination by other indicators (including built in world denominators data set)
* Screening units by data requirements
* Imputation of missing data, by a variety of methods
* Data treatment using Winsorisation and nonlinear transformations
* Normalisation by more than ten methods, either for all indicators or for each individually
* Weighting using either manual weighting, PCA weights or correlation optimised weights. COINr also includes a reweighting app which explores the effects of weights on correlations.
* Aggregation of indicators using a variety of methods which can be different for each aggregation level.

**Analysis features:**

* Detailed indicator statistics, and data availability within aggregation groups
* Multivariate analysis, including quick functions for PCA, and a detailed correlation analysis and visualisation
* Easy "what if" analysis - very quickly checking the effects of adding and removing indicators, changing weights, methodological variations
* Full global uncertainty and sensitivity analysis which can check the impacts of uncertainties in weighting and many methodological choices

**Visualisation and presentation:**

* Statistical plots of indicators - histograms, violin plots, dot plots, scatter plots and more, including interactive html plots and an app for exploring indicator data
* Bar charts, stacked bar charts, maps, tables and radar charts for presenting indicator data and making comparisons between units
* Static and interactive correlation plots for visualising correlations between indicators and between aggregation levels
* An interactive app for visualising and presenting initial results
* Automatic generation of unit reports (e.g. country reports) using customisable R markdown templates

COINr also allows fast import from the [COIN Tool](https://knowledge4policy.ec.europa.eu/composite-indicators/coin-tool_en) and fast export to Excel.

In short, COINr aims to allow composite indicators to be developed and prototyped very quickly and in a structured fashion, with the results immediately available and able to be explored interactively. Although it is built in R, it is a high-level package that aims to make command simple and intuitive, with the hard work performed behind the scenes, therefore it is also accessible to less experienced R users.

# Installation

COINr is not yet on CRAN but can be installed via Github. First, install the `devtools` package if you don't already have it, then run:

```{r InstallCOINr, eval=FALSE}
devtools::install_github("bluefoxr/COINr")
```

This should directly install the package from Github, without any other steps. You may be asked to update packages. This might not be strictly necessary, so you can also skip this step.
