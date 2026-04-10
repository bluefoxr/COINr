# Impute by mean

Replaces `NA`s in a numeric vector with the mean of the non-`NA` values.

## Usage

``` r
i_mean(x)
```

## Arguments

- x:

  A numeric vector

## Value

A numeric vector

## Examples

``` r
x <- c(1,2,3,4, NA)
i_mean(x)
#> [1] 1.0 2.0 3.0 4.0 2.5
```
