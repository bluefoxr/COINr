# Calculate skewness

Calculates skewness of the values of a numeric vector. This uses the
same definition of skewness as the "skewness()" function in the "e1071"
package where `type == 2`, which is equivalent to the definition of
skewness used in Excel.

## Usage

``` r
skew(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A skewness value (scalar).

## Examples

``` r
x <- runif(20)
skew(x)
#> [1] -0.3006503
```
