# Box Cox transformation

Simple Box Cox, with no optimisation of lambda.

## Usage

``` r
boxcox(x, lambda, makepos = TRUE, na.rm = FALSE)
```

## Arguments

- x:

  A vector or column of data to transform

- lambda:

  The lambda parameter of the Box Cox transform

- makepos:

  If `TRUE` (default) makes all values positive by subtracting the
  minimum and adding 1.

- na.rm:

  If `TRUE`, `NA`s will be removed: only relevant if `makepos = TRUE`
  which invokes [`min()`](https://rdrr.io/r/base/Extremes.html).

## Value

A vector of length `length(x)` with transformed values.

## Details

This function replaces the now-defunct `BoxCox()` from COINr \< v1.0.

## Examples

``` r
# example data
x <- runif(30)
# Apply Box Cox
xBox <- boxcox(x, lambda = 2)
# plot one against the other
plot(x, xBox)

```
