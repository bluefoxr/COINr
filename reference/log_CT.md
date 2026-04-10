# Log-transform a vector

Performs a log transform on a numeric vector.

## Usage

``` r
log_CT(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A log-transformed vector of data, and treatment details wrapped in a
list.

## Details

Specifically, this performs a modified "COIN Tool log" transform:
`log(x-min(x) + a)`, where `a <- 0.01*(max(x)-min(x))`.

## Examples

``` r
x <- runif(20)
log_CT(x)
#> $x
#>  [1] -0.6681907 -0.1184511 -0.4325236 -4.7335717 -1.5590959 -0.4148121
#>  [7] -0.1699390 -1.2475760 -2.4914565 -0.1374938 -3.3558296 -0.3627144
#> [13] -0.5260884 -0.3740855 -0.8693052 -1.0538399 -3.1902726 -0.1204197
#> [19] -0.8079876 -0.9659410
#> 
#> $treated
#>  [1] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#>  [9] "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT" "log_CT"
#> [17] "log_CT" "log_CT" "log_CT" "log_CT"
#> 
```
