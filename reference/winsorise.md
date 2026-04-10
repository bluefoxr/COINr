# Winsorise a vector

Follows a "standard" Winsorisation approach: points are successively
Winsorised in order to bring skew and kurtosis thresholds within
specified limits. Specifically, aims to bring absolute skew to below a
threshold (default 2.25) and kurtosis below another threshold (default
3.5).

## Usage

``` r
winsorise(
  x,
  na.rm = FALSE,
  winmax = 5,
  skew_thresh = 2,
  kurt_thresh = 3.5,
  force_win = FALSE
)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

- winmax:

  Maximum number of points to Winsorise. Default 5. Set `NULL` to have
  no limit.

- skew_thresh:

  A threshold for absolute skewness (positive). Default 2.25.

- kurt_thresh:

  A threshold for kurtosis. Default 3.5.

- force_win:

  Logical: if `TRUE`, forces winsorisation up to winmax (regardless of
  skew/kurt). Default `FALSE`. Note - this option should be used with
  care because the direction of Winsorisation is based on the direction
  of skew. Successively Winsorising can switch the direction of skew and
  hence the direction of Winsorisation, which may not produce the
  expected behaviour.

## Value

A list containing winsorised data, number of winsorised points, and the
individual points that were treated.

## Details

Winsorisation here is defined as reassigning the point with the
highest/lowest value with the value of the next highest/lowest point.
Whether to Winsorise at the high or low end of the scale is decided by
the direction of the skewness of `x`.

This function replaces the now-defunct `coin_win()` from COINr \< v1.0.

## Examples

``` r
# numbers between 1 and 10
x <- 1:10

# two outliers
x <- c(x, 30, 100)

# winsorise
l_win <- winsorise(x, skew_thresh = 2, kurt_thresh = 3.5)

# see treated vector, number of winsorised points and details
l_win
#> $x
#>  [1]  1  2  3  4  5  6  7  8  9 10 30 30
#> 
#> $nwin
#> [1] 1
#> 
#> $treated
#>  [1] ""      ""      ""      ""      ""      ""      ""      ""      ""     
#> [10] ""      ""      "winhi"
#> 
```
