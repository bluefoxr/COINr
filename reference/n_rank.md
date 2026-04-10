# Normalise using ranks

This is simply a wrapper for
[`base::rank()`](https://rdrr.io/r/base/rank.html). Higher scores will
give higher ranks.

## Usage

``` r
n_rank(x, ties.method = "min")
```

## Arguments

- x:

  A numeric vector

- ties.method:

  This argument is passed to
  [`base::rank()`](https://rdrr.io/r/base/rank.html) - see there for
  details.

## Value

Numeric vector

## Examples

``` r
x <- runif(20)
n_rank(x)
#>  [1] 10 13 19  7 18  1 11 15  4 16 12 20  2  3  9 14 17  6  5  8
```
