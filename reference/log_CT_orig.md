# Log-transform a vector

Performs a log transform on a numeric vector.

## Usage

``` r
log_CT_orig(x, na.rm = FALSE)
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

Specifically, this performs a "COIN Tool log" transform:
`log(x-min(x) + 1)`.

## Examples

``` r
x <- runif(20)
log_CT_orig(x)
#> $x
#>  [1] 0.26106880 0.13377197 0.04872626 0.56382726 0.41675020 0.61501995
#>  [7] 0.60405120 0.61016402 0.57866577 0.26823977 0.63328451 0.50755923
#> [13] 0.49378267 0.02664096 0.12658061 0.38604158 0.27812172 0.00000000
#> [19] 0.49880842 0.57556918
#> 
#> $treated
#>  [1] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#>  [6] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#> [11] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#> [16] "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig" "log_CT_orig"
#> 
```
