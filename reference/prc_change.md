# Percentage change of time series

Calculates the percentage change in a time series from the initial
value. The time series is defined by `y` the response variable, indexed
by `x`, the time variable. The `per` argument can optionally be used to
scale the result according to a period of time. E.g. if the units of `x`
are years, setting `x = 10` will measure the percentage change per
decade.

## Usage

``` r
prc_change(y, x, per = 1)
```

## Arguments

- y:

  A numeric vector

- x:

  A numeric vector of the same length as `y`, indexing `y` in time. No
  `NA` values are allowed in `x`.

- per:

  Numeric value to scale the change according to a period of time. See
  description.

## Value

Percentage change as a scalar value.

## Details

This function operates in two ways, depending on the number of data
points. If `x` and `y` have two non-`NA` observations, percentage change
is calculated using the first and last values. If three or more points
are available, a linear regression is used to estimate the average
percentage change. If fewer than two points are available, the
percentage change cannot be estimated and `NA` is returned.

If all `y` values are equal, it will return a change of zero.

## Examples

``` r
# a time vector
x <- 2011:2020

# some random points
y <- runif(10)

# find percentage change per decade
prc_change(y, x, 10)
#> [1] 14.0989
```
