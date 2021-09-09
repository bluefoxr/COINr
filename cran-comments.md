## Resubmission

This is a resubmission following further CRAN comments. In this version I have:

* Ensured that any files are always written to the temporary directory
* Removed any marked UTF-8 strings
* Inserted an escape for a function that knits R Markdown docs. Now, if Pandoc is not installed, it exits with a warning.
* Removed the use of the new pipe ( |> ) operator in the vignette, since that requires a newer version of R than stated

# Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'William Becker <william.becker@bluefoxdata.eu>'
  
  New submission

0 errors √ | 0 warnings √ | 1 note x
