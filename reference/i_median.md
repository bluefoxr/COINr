# Impute by median

Replaces `NA`s in a numeric vector with the median of the non-`NA`
values.

## Usage

``` r
i_median(x)
```

## Arguments

- x:

  A numeric vector

## Value

A numeric vector

## Examples

``` r
x <- c(1,2,3,4, NA)
i_median(x)
#> [1] 1.0 2.0 3.0 4.0 2.5
```
