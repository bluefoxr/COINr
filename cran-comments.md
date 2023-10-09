## Resubmission

This is a resubmission to update to version 1.1.7. Thank you for your time in checking it over.

## R CMD check results

After Rhub and Winbuilder checks I get the following NOTEs, all of which I think can be safely disregarded:

Found the following (possibly) invalid URLs:
  URL: https://onlinelibrary.wiley.com/doi/book/10.1002/9780470725184
    From: man/SA_sample.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://support.posit.co/hc/en-us/articles/219949047-Installing-older-versions-of-packages
    From: inst/doc/v1.html
    Status: 403
    Message: Forbidden
    
This seems to be the usual issue of automatic link checking.

* checking HTML version of manual ... [21s] NOTE
Skipping checking math rendering: package 'V8' unavailable

I think this package is not available on R Hub, otherwise on other platforms it works fine.

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
  
This is also an R-Hub issue as I understand it.

* checking for detritus in the temp directory ... NOTE
  'lastMiKTeXException'
Found the following files/directories:

This is also an R-Hub issue as I understand it.

## Downstream dependencies

There are no downstream dependencies for this package.
