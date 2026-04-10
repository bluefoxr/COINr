# Round a data frame to specified significant figures

Tiny function just to round down a data frame by significant figures for
display in a table, ignoring non-numeric columns.

## Usage

``` r
signif_df(df, digits = 3)
```

## Arguments

- df:

  A data frame to input

- digits:

  The number of decimal places to round to (default 3)

## Value

A data frame, with any numeric columns rounded to the specified amount.

## Examples

``` r
signif_df( as.data.frame(matrix(runif(20),10,2)), digits = 3)
#>       V1    V2
#> 1  0.562 0.627
#> 2  0.537 0.725
#> 3  0.304 0.867
#> 4  0.333 0.605
#> 5  0.603 0.992
#> 6  0.973 0.866
#> 7  0.885 0.770
#> 8  0.195 0.535
#> 9  0.668 0.184
#> 10 0.910 0.891

```
