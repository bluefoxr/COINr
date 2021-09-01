## Resubmission

This is a resubmission following CRAN comments. In this version I have:

* Removed `\dontrun` from all examples. Now all examples should run, and those that involve write use
the temporary directory and then delete files afterwards (functions are `coin2Excel()` and `getUnitReport()`). Hopefully this is OK.

* Exceptions to the previous point, which are now wrapped in `\donttest`:
    - `sensitivity()` This function by definition needs to regenerate results many times. Even a very reduced example takes about ten seconds or so on `devtools::check_win_devel()` and is flagged
    - `plotSA()` and `plotSAranks()` which both plot the output of `sensitivity()`. For the same reason, even simplified examples exceed the allowed time.
  These examples are wrapped in `\donttest` to avoid exceeding the testing time limit. However, the examples work fine otherwise.

* One further exception is `COINToolIn()`. This function reads in a pre-formatted Excel file called the "COIN Tool". The problem is that this file type is quite large (about 12Mb unzipped), so the example for this function downloads one of these "COIN Tool" files from the internet, unzips, and reads it into R. This works fine on my computer (Windows) but fails for Linux, which is something to do with the download.file() function and how it behaves on different distributions. In other words, this is not a problem of COINr per se, but more just how to download files across platforms. According to the `download.file()` documentation "Setting the `method` should be left to the end user.". I am not sure how to make this download work in Linux since I can't reproduce the error. I have put this example inside `\dontrun` and made a comment in the example that `download.file()` may need the `method` argument set to a value appropriate to the platform. I hope this is OK.
    
* Wrapped Shiny apps and examples in `if(interactive())`

* Ensured that all functions have a `\value`

* Fixed broken example

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
