# Normalise using percentile ranks

Calculates percentile ranks of a numeric vector using "sport" ranking.
Ranks are calculated by
[`base::rank()`](https://rdrr.io/r/base/rank.html) and converted to
percentile ranks. The `ties.method` can be changed - this is directly
passed to [`base::rank()`](https://rdrr.io/r/base/rank.html).

## Usage

``` r
n_prank(x, ties.method = "min")
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
n_prank(x)
#>  [1] 0.63157895 0.94736842 0.31578947 0.84210526 0.57894737 0.26315789
#>  [7] 0.00000000 0.52631579 0.05263158 0.36842105 1.00000000 0.78947368
#> [13] 0.10526316 0.68421053 0.47368421 0.89473684 0.15789474 0.73684211
#> [19] 0.42105263 0.21052632
```
