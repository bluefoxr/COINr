## Resubmission

This is a resubmission to update from version 1.0 to version 1.1. I have fixed some small problems from a submission earlier today: some dead vignette links and a updated licence file format, also a redirected URL.

## R CMD check results

Otherwise, there was 1 note regarding possibly invalid URLs. I have checked this on the R package development mailing list and the conclusion was that this is due to the automatic checks run by R. All URLs are valid, but Cloudflare and other content distribution companies block these types of automatic checks, hence the note.

NOTE

Found the following (possibly) invalid URLs:
  URL: https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/
    From: inst/doc/coins.html
          inst/doc/overview.html
    Status: 403
    Message: Forbidden
  URL: https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/repository
    From: man/ASEM_iData.Rd
          man/ASEM_iData_p.Rd
    Status: 403
    Message: Forbidden
  URL: https://composite-indicators.jrc.ec.europa.eu/sites/default/files/COIN_Tool_v1_LITE_exampledata.xlsm
    From: inst/doc/other_functions.html
    Status: 403
    Message: Forbidden
  URL: https://onlinelibrary.wiley.com/doi/book/10.1002/9780470725184
    From: man/SA_sample.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://support.rstudio.com/hc/en-us/articles/219949047-Installing-older-versions-of-packages
    From: inst/doc/v1.html
    Status: 403
    Message: Forbidden
    
## Downstream dependencies

There are no downstream dependencies for this package.
