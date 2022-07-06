---
title: 'COINr: An R package for developing composite indicators'
tags:
- R
- composite indicators
- indicators
- policy
date: 6 July 2022
affiliations:
- name: Freelance consultant, Ispra, Italy
  index: 1
- name: European Commission, Joint Research Centre, Italy
  index: 2
- name: European Innovation Council and SMEs Executive Agency, Belgium
  index: 3
authors:
- name: William Becker
  orcid: 0000-0002-6467-4472
  corresponding: yes
  affiliation: 1
- name: Giulio Caperna
  affiliation: 2
- name: Maria Del Sorbo
  affiliation: 3
- name: Hedvig Norlén
  affiliation: 1
- name: Eleni Papadimitriou
  affiliation: 2
- name: Michaela Saisana
  affiliation: 2
bibliography: paper.bib
---

# Summary

Composite indicators are aggregations of indicators that aim to measure complex, multi-dimensional and typically socio-economic concepts such as sustainable development [@HDI2020], innovation [@dutta2020global], globalisation [@becker2021exploring], gender equality [@EM2030] and many more. Composite indicators are very widely used in policy-making and by international organisations, but are equally well-covered in academic literature [@el2019building; @stefana2021composite; @linden2021_weighting]. They are often used to rank and benchmark countries or regions to help direct policy making, but are also frequently used for advocacy [@etter2019corporate].

The construction of a composite indicator includes a number of statistical and data processing steps. The COINr package, introduced in this article, aims to provide a harmonised development environment for composite indicators that includes all common operations from indicator selection, data treatment and imputation up to aggregation, presentation of results and full global sensitivity analysis. **COINr** enables fast but accurate development and visualisation, quick exploration of methodological variations, and encourages transparency and reproducibility.

# Statement of need

## Existing tools

Although it is hard to say for sure which tools are mostly used for constructing composite indicators, from the experience of the authors, the majority of CIs are built using Excel, although in some cases the data processing may be done partially or entirely in R, Python or similar.

Some dedicated tools exist however: in Excel, the *COIN Tool* is a spreadsheet-based system which allows users to build and analyse a composite indicator [@COINTool]. In Matlab, there are some packages addressing specific parts of index development: the *CIAO* package uses a nonlinear regression and optimisation approach to tune weights to agree with expert opinions [@CIAOtool]. In R there is an existing package for composite indicator development, called \pkg{compind} [@compindPackage]. This has some sophisticated tools for weighting, particularly relating to data envelopment analysis approaches, as well as a number of aggregation functions. However, this is arguably more a toolbox of useful functions for constructing composite indicators, and gives no special consideration to hierarchical structures, uncertainty and sensitivity analysis, and so on.

In Python there is a library called *CIF* which gives a number of tools for building composite indicators, from loading data to aggregation and visualisation. This is focused in particular on Business Cycle Analysis. Finally, there is a recently launched web-based tool called the *MCDA Index Tool* \citep{cinelli2021mcda}. This is mostly focused on multi-criteria decision analysis, and doesn't include different levels of aggregation. Nonetheless, for the purposes of MCDA, and certain types of indexes, it is a very useful application.

## Why COINr

\pkg{COINr} is a significant step beyond existing composite indicator tools in many respects. COINr wraps all composite indicator data, analysis and methodological choices into a single S3 class object called a "coin". This enables a neat and structured environment, simplifies the syntax of functions, and also allows comparisons between different versions of the same index, as well as full global sensitivity analysis. COINr also supports time-indexed data, represented by the "purse" class (a time-indexed collection of coins).

All major COINr functions have methods for coins, and many have methods for purses, data frames and numerical vectors. This means that COINr can be used either as an integrated development environment via coins and purses, but equally as a toolbox of functions for other related purposes.

COINr also offers a far wider range of functions and methodological options than any existing package. It not only includes a range of options for treating, imputing, normalising and aggregating indicator data (among others), but also has a suite of analysis tools to check data availability and perform correlation/multivariate analysis. Moreover, it has many options for plotting and visualising data using wrapper functions for **ggplot2**. Many core COINr functions are written with hooks to link with other packages, for example allowing other imputation or aggregation packages to be used with coins.

In short, \pkg{COINr} aims to be a flexible, fast and comprehensive development environment for composite indicators. This enables users to develop composite indicators more quickly, more accurately, and encourages reproducibility and transparency.

# Features

**COINr** is extensively documented with many vignettes and examples, all of which can be easily browsed at its **pkgdown** [website](https://bluefoxr.github.io/COINr/). Here, a brief overview of features is given.

**Building features**:

* Flexible and fast development of composite indicators with no limits on aggregation levels, numbers of indicators, highly flexible set of methodological choices.
* Denomination by other indicators
* Screening units by data requirements
* Imputation of missing data, by a variety of methods
* Data treatment using Winsorisation and nonlinear transformations
* Normalisation (scaling) using a variety of methods 
* Weighting using either manual weighting, PCA weights or correlation-optimised weights.
* Aggregation of indicators using a variety of methods which can be different for each aggregation level.
* Support for panel data (time-indexed data) for constructing multi-year composite indicators

**Analysis features:**

* Detailed indicator statistics, and data availability within aggregation groups
* Multivariate analysis, including quick functions for PCA, and a detailed correlation analysis and visualisation
* Easy "what if" analysis - very quickly checking the effects of adding and removing indicators, changing weights, methodological variations
* Full global uncertainty and sensitivity analysis which can check the impacts of uncertainties in weighting and many methodological choices

**Visualisation and presentation:**

* Statistical plots of indicators - histograms, violin plots, dot plots, scatter plots and more
* Bar charts, stacked bar charts and tables for presenting indicator data and making comparisons between units
* Correlation plots for visualising correlations between indicators and between aggregation levels

COINr also allows fast import from the [COIN Tool](https://knowledge4policy.ec.europa.eu/composite-indicators/coin-tool_en) and fast export to Excel.

# Acknowledgements

COINr was initally developed under contract for the European Commission's Joint Research Centre, and this is gratefully acknowledged for enabling the bulk of the initial design.

# References
