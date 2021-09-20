# COINr 0.5.5.9000

* Improved bug trapping in Winsorisation function

# COINr 0.5.5.9000

* Added data availability thresholds during aggregation - see `aggregate()` function.

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
