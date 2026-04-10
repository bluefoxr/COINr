# Compound annual growth rate

Given a variable `y` indexed by a time vector `x`, calculates the
compound annual growth rate. Note that CAGR assumes that the `x` refer
to years. Also it is only calculated using the first and latest observed
values.

## Usage

``` r
CAGR(y, x)
```

## Arguments

- y:

  A numeric vector

- x:

  A numeric vector of the same length as `y`, indexing `y` in time. No
  `NA` values are allowed in `x`. This vector is assumed to be years,
  otherwise the result must be interpreted differently.

## Value

A scalar value (CAGR)

## Examples

``` r
# random points over 10 years
x <- 2011:2020
y <- runif(10)

CAGR(y, x)
#> [1] 0.2852101
```
