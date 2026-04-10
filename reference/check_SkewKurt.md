# Check skew and kurtosis of a vector

Logical test: if `abs(skewness) < skew_thresh` OR
`kurtosis < kurt_thresh`, returns `TRUE`, else `FALSE`

## Usage

``` r
check_SkewKurt(x, na.rm = FALSE, skew_thresh = 2, kurt_thresh = 3.5)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

- skew_thresh:

  A threshold for absolute skewness (positive). Default 2.25.

- kurt_thresh:

  A threshold for kurtosis. Default 3.5.

## Value

A list with `.$Pass` is a Logical, where `TRUE` is pass, `FALSE` is
fail, and `.$Details` is a sub-list with skew and kurtosis values.

## Examples

``` r
set.seed(100)
x <- runif(20)
# this passes
check_SkewKurt(x)
#> $Pass
#> [1] TRUE
#> 
#> $Skew
#> [1] 0.1935009
#> 
#> $Kurt
#> [1] -0.7407903
#> 
# if we add an outlier, doesn't pass
check_SkewKurt(c(x, 1000))
#> $Pass
#> [1] FALSE
#> 
#> $Skew
#> [1] 4.582568
#> 
#> $Kurt
#> [1] 20.99995
#> 
```
