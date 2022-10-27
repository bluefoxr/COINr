## Resubmission

This is a resubmission to update from version 1.0 to version 1.1.

## R CMD check results

There was 1 note regarding possibly invalid URLs. I have checked this on the R package development mailing list and the conclusion was that this is due to the automatic checks run by R. All URLs are valid, but Cloudflare and other content distribution companies block these types of automatic checks, hence the note.

NOTE

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1111/j.1467-985X.2005.00350.x
    From: inst/doc/sensitivity.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1111/j.1467-985X.2012.01059.x
    From: inst/doc/weights.html
    Status: 503
    Message: Service Unavailable
  URL: https://en.wikipedia.org/wiki/Copeland%27s_method
    From: inst/doc/aggregate.html
    Status: 503
    Message: Service Unavailable
  URL: https://en.wikipedia.org/wiki/Pareto_principle
    From: inst/doc/sensitivity.html
    Status: 503
    Message: Service Unavailable
  URL: https://en.wikipedia.org/wiki/Pythagorean_means
    From: inst/doc/aggregate.html
    Status: 503
    Message: Service Unavailable
  URL: https://joss.theoj.org/papers/187b1759658c96177f8d17f3b55b90a0 (moved to https://joss.theoj.org/papers/10.21105/joss.04567)
    From: README.md
    Status: 200
    Message: OK
  URL: https://onlinelibrary.wiley.com/doi/book/10.1002/9780470725184
    From: man/SA_sample.Rd
    Status: 503
    Message: Service Unavailable
    
## Downstream dependencies

There are no downstream dependencies for this package.
