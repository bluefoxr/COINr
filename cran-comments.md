## Resubmission

This is a resubmission, which comprises a major update, hence the jump in numbering from 0.6.1 to 1.0.0.

Updates are described in NEWS.md, and there is a vignette describing the upgrades in more detail at `vignette("v1")`.

## R CMD check results

There was 1 note regarding possibly valid URLs. I have checked this on the R package development mailing list and the conclusion was that this is due to the automatic checks run by R. All URLs are valid, but Cloudflare and other content distribution companies block these types of automatic checks, hence the note.

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
  URL: https://doi.org/10.1111/j.1467-985X.2005.00350.x
    From: man/get_sensitivity.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1111/j.1467-985X.2012.01059.x
    From: inst/doc/weights.html
    Status: 503
    Message: Service Unavailable
  URL: https://onlinelibrary.wiley.com/doi/book/10.1002/9780470725184
    From: man/SA_sample.Rd
    Status: 503
    Message: Service Unavailable
    
## Downstream dependencies

There are no downstream dependencies for this package.
