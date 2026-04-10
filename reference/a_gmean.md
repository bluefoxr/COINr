# Weighted geometric mean

Weighted geometric mean of a vector. `NA` are skipped by default.

## Usage

``` r
a_gmean(x, w = NULL)
```

## Arguments

- x:

  A numeric vector of positive values.

- w:

  A vector of weights, which should have length equal to `length(x)`.
  Weights are relative and will be re-scaled to sum to 1. If `w` is not
  specified, defaults to equal weights.

## Value

The geometric mean, as a numeric value.

## Details

This function replaces the now-defunct `geoMean()` from COINr \< v1.0.

## Examples

``` r
# a vector of values
x <- 1:10
# a vector of weights
w <- runif(10)
# weighted geometric mean
a_gmean(x,w)
#> [1] 5.287435
```
