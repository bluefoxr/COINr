# COINr 1.1.12

- Enable custom operations on coins and purses: new generic `Custom()` with methods
for coins and purses.
- Add possibility to impute panel data using "constant" method from `stats::aprox()`.

# COINr 1.1.11

- Allow retention of units with no data in split to purse
- Allow NAs in group vector for `i_median_grp()`
- Enable linear interpolation in panel data with `impute_panel()`
- Updated documentation on aggregation and imputation functions

# COINr 1.1.9

- Fixed results tables which rounded before converting to ranks, resulting in precision errors

# COINr 1.1.8

- Added possibility to set normalisation parameters in `iMeta`, added more explanation and examples in the normalisation vignette.
- Fixed bug correlation directions

# COINr 1.1.7

I have skipped a number of version number iterations. Here's what's changed since last time:

- Add some helper functions to convert codes to names
- Add flip axes to bar chart
- Add option to skip SA address checks
- Option to use raw data in results tables
- Small bug fixes
- Catch more errors on coin construction


# COINr 1.1.2.9000

Dev version of COINr until next CRAN submission!

* `Treat()` now catches errors - data frame/coin/purse methods 

# COINr 1.1.2

Lots of small fixes and feature extensions, including

* More robust write to log, can deal with `COINr::func_Name` type calls
* Catch spaces and numbers in iCodes
* Tweaks to correlation tables
* Reordering for correlation plots
* Better input checks
* Extra stats
* Bi-direction log transformation for negative skewed outlier treatment
* Subgroups of units for bar charts

# COINr 1.1

Some new functions and bug fixes:

* Removed defunct function messages and startup message, defunct functions are now removed from the namespace
* New `get_trends()` function for analysing time trends
* Option to export purse class to Excel
* Expand unit test coverage to 80%
* Quite a lot of small bug fixes
* Improved documentation in many places
* Improved colouring for some plots
* Enabled distance to target normalisation plus examples in vignette
* COINr is now citable by a [JOSS paper](https://doi.org/10.21105/joss.04567)

# COINr 1.0.0

Major update to COINr. The main changes are as follows:

* Syntax changed to be more consistent
* New streamlined "coin" class replaces older "COIN" class
* New "purse" class to deal with panel data (time dependent data)
* Removed interactive functions based on plotly and shiny
* Much fewer package dependencies
* Underlying code rewritten to be more robust
* Improved functionality and flexibility in many functions
* COINr 0.6.1.9000 has been archived as a separate package called "COINr6" - this can be installed to ensure older code will still run.

The full changes are too extensive to write here. See `vignette("v1")` also available online [here](https://bluefoxr.github.io/COINr/articles/v1.html) for a more complete description of changes.

Note that these changes are disruptive but greatly improve the package and make it more maintainable for the future. This is a one-off major overhaul, after which backwards compatibility will be ensured.

# COINr 0.6.2

* Fixed bug in `aggregate()` which was wrongly assigning weights in some circumstances (note this is a fairly
significant bug - worth re-running your results as it may have affected them)
* Added colour option to `plotframework()`
* Fixed bug in `compareDF()` (resulting from tibbles vs data frames)
* Fixed bug in `sensitivity()` causing trouble with running SA with no weights uncertainty specified

# COINr 0.6.1

* Removed dependency on new native pipe so now runs again on R > 4.0

# COINr 0.6.0

* New plot function `plotIndDot()` for plotting single indicators with possibility to label units
* Added `print` method for COIN class. More methods to come.
* Extended support for panel data in `assemble()`. This can now output multiple COINs structured into a tibble. The idea of working with multiple COINs will be extended in future versions.
* Improved bug trapping in Winsorisation function
* Added data availability thresholds during aggregation - see `aggregate()` function.
* Better NA colour for `plotCorr()` plus fixed variable ordering
* Further colour options in `plotCorr()`
* Variable/aggregate ordering in `plotCorr()` now matches grouping in higher levels
* Add option to do PCA not in groups in `getPCA()`
* Colour options for `plotSARanks()`
* Added dominance pairs calculation to `ourankMatrix()`
* Stopped max tab length exceeding in Excel export.
* [Online book](https://bluefoxr.github.io/COINrDoc/) now pretty much up to date with COINr 0.6.

# COINr 0.5.5

* COINr now requires R version 4.0.0 or above - this is following some issues on some distributions during CRAN checks.
* Improved documentation and bug catching in `aggregate()`
* New function `replaceDF()` which is a simple utility for multiple values in one go for a data frame.
* New function `removeElements()` which successively removes indicators or aggregates one at a time and summarises the effects.

# COINr 0.5.4

* Fixed various small issues that were flagged by CRAN
* New function `extractYear` which allows panel data to be passed to `assemble()` and (optionally) imputed by latest year.
* Fixed bug in `plorCorr()` and `getCorr()`
* Changed `WorldDenoms` data set slightly to only use ASCII coding - this involved unfortunately removing a few accents on country names which is necessary to avoid warning notes when submitting to CRAN.
* Added goalpost normalisation method, as in European Skills Index
* Fixed small bug in `compareDF()`

# COINr 0.5.3

Fixes following comments from CRAN.

* Patched up holes in the documentation
* Function examples now run where possible
* Shiny apps only run now in interactive mode

# COINr 0.5.2

This is the first release submitted to CRAN. Changes include:

* Documentation has been thoroughly cleaned up, including coherent examples for each function.
* Small bugs have been checked and fixed.
* A vignette is now available
