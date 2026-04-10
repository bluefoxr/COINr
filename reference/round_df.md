# Round down a data frame

Tiny function just to round down a data frame for display in a table,
ignoring non-numeric columns.

## Usage

``` r
round_df(df, decimals = 2)
```

## Arguments

- df:

  A data frame to input

- decimals:

  The number of decimal places to round to (default 2)

## Value

A data frame, with any numeric columns rounded to the specified amount.

## Details

This function replaces the now-defunct `roundDF()` from COINr \< v1.0.

## Examples

``` r
round_df( as.data.frame(matrix(runif(20),10,2)), decimals = 3)
#>       V1    V2
#> 1  0.813 0.900
#> 2  0.550 0.273
#> 3  0.070 0.081
#> 4  0.062 0.029
#> 5  0.978 0.531
#> 6  0.571 0.910
#> 7  0.817 0.042
#> 8  0.602 0.442
#> 9  0.736 0.780
#> 10 0.799 0.242
```
