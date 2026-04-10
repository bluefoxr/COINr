# Normalise using Borda scores

Calculates Borda scores as `rank(x) - 1`.

## Usage

``` r
n_borda(x, ties.method = "min")
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
n_borda(x)
#>  [1] 13 14  5 10 11 19 17 16 12  4  7  8  9  3 18  2  0  6 15  1
```
