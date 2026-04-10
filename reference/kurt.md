# Calculate kurtosis

Calculates kurtosis of the values of a numeric vector. This uses the
same definition of kurtosis as as the "kurtosis()" function in the e1071
package, where `type == 2`, which is equivalent to the definition of
kurtosis used in Excel.

## Usage

``` r
kurt(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A kurtosis value (scalar).

## Examples

``` r
x <- runif(20)
kurt(x)
#> [1] -1.488466
```
