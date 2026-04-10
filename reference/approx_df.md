# Interpolate time-indexed data frame

Given a numeric data frame `Y` with rows indexed by a time vector `tt`,
interpolates at time values specified by the vector `tt_est`. If
`tt_est` is not in `tt`, will create new rows in the data frame
corresponding to these interpolated points.

## Usage

``` r
approx_df(Y, tt, tt_est = NULL, ...)
```

## Arguments

- Y:

  A data frame with all numeric columns

- tt:

  A time vector with length equal to `nrow(Y)`, indexing the rows in
  `Y`.

- tt_est:

  A time vector of points to interpolate in `Y`. If `NULL`, will attempt
  to interpolate all points in `Y` (you may need to adjust the `rule`
  argument of
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) here).
  Note that points not specified in `tt_est` will not be interpolated.
  `tt_est` does not need to be a subset of `tt`.

- ...:

  Further arguments to pass to
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) other than
  `x`, `y` and `xout`.

## Value

A list with:

- `.$tt` the vector of time points, including time values of
  interpolated points

- `.$Y` the corresponding interpolated data frame

Both outputs are sorted by `tt`.

## Details

This is a wrapper for
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html), with some
differences. In the first place,
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) is applied
to each column of `Y`, using `tt` each time as the corresponding time
vector indexing `Y`. Interpolated values are generated at points
specified in `tt_est` but these are appended to the existing data
(whereas [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)
will only return the interpolated points and nothing else). Further
arguments to [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html)
can be passed using the `...` argument.

## Examples

``` r
# a time vector
tt <- 2011:2020

# two random vectors with some missing values
y1 <- runif(10)
y2 <- runif(10)
y1[2] <- y1[5] <- NA
y2[3] <- y2[5] <- NA
# make into df
Y <- data.frame(y1, y2)

# interpolate for time = 2012
Y_int <- approx_df(Y, tt, 2012)
Y_int$Y
#>             y1        y2
#> 1  0.894915818 0.1376303
#> 2  0.830897179 0.0622309
#> 3  0.766878539        NA
#> 4  0.414541042 0.6655709
#> 5           NA        NA
#> 6  0.511333802 0.9136983
#> 7  0.743476679 0.8937201
#> 8  0.888419045 0.0897850
#> 9  0.612988593 0.5389992
#> 10 0.003176101 0.4564761

# notice Y_int$y2 is unchanged since at 2012 it did not have NA value
stopifnot(identical(Y_int$Y$y2, y2))

# interpolate at value not in tt
approx_df(Y, tt, 2015.5)
#> $tt
#>  [1] 2011.0 2012.0 2013.0 2014.0 2015.0 2015.5 2016.0 2017.0 2018.0 2019.0
#> [11] 2020.0
#> 
#> $Y
#>             y1        y2
#> 1  0.894915818 0.1376303
#> 2           NA 0.0622309
#> 3  0.766878539        NA
#> 4  0.414541042 0.6655709
#> 5           NA        NA
#> 6  0.487135612 0.8516664
#> 7  0.511333802 0.9136983
#> 8  0.743476679 0.8937201
#> 9  0.888419045 0.0897850
#> 10 0.612988593 0.5389992
#> 11 0.003176101 0.4564761
#> 
```
