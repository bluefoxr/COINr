# COINr 0.5.4.9000

*This is a development version that is not yet on CRAN*

* Improved documentation and bug catching in `aggregate()`
* New function `replaceDF()` which is a simple utility for multiple values in one go for a data frame.

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
