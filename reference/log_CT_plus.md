# Log transform a vector (skew corrected)

Performs a log transform on a numeric vector, but with consideration for
the direction of the skew. The aim here is to reduce the absolute value
of skew, regardless of its direction.

## Usage

``` r
log_CT_plus(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A log-transformed vector of data, and treatment details wrapped in a
list.

## Details

Specifically:

If the skew of `x` is positive, this performs a modified "COIN Tool log"
transform: `log(x-min(x) + a)`, where `a <- 0.01*(max(x)-min(x))`.

If the skew of `x` is negative, it performs an equivalent transformation
`-log(xmax + a - x)`.

## Examples

``` r
x <- runif(20)
log_CT(x)
#> $x
#>  [1] -2.33322431 -1.10205672 -2.11657261 -0.05067424 -2.09356526 -0.69439126
#>  [7] -0.61638357 -0.12902507 -0.52536746 -0.82439783 -1.58766094 -2.49398977
#> [13] -0.91471655 -0.30103037 -0.10457294 -1.16292732 -1.35864733 -1.14340081
#> [19] -1.37426862 -4.66579476
#> 
#> $treated
#>  [1] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#>  [9] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#> [17] "log_CT" "log_CT" "log_CT" "log_CT"
#> 
```
