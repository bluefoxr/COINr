# Impute a numeric vector

Imputes missing values in a numeric vector using a function `f_i`. This
function should return a vector identical to `x` except for `NA` values,
which can be replaced. The function `f_i` is not required to replace
*all* `NA` values.

## Usage

``` r
# S3 method for class 'numeric'
Impute(x, f_i = NULL, f_i_para = NULL, ...)
```

## Arguments

- x:

  A numeric vector, possibly with `NA` values to be imputed.

- f_i:

  A function that imputes missing values in a numeric vector. See
  description and details.

- f_i_para:

  Optional further arguments to be passed to `f_i()`

- ...:

  arguments passed to or from other methods.

## Value

An imputed numeric vector of the same length of `x`.

## Details

This calls the function `f_i()`, with optionally further arguments
`f_i_para`, to impute any missing values found in `x`. By default,
`f_i = "i_mean()"`, which simply imputes `NA`s with the mean of the
non-`NA` values in `x`.

COINr has several built-in imputation functions of the form `i_*()` for
vectors which can be called by
[`Impute()`](https://bluefoxr.github.io/COINr/reference/Impute.md). See
the [online
documentation](https://bluefoxr.github.io/COINr/articles/imputation.html#data-frames)
for more details.

You could also use one of the imputation functions directly (such as
[`i_mean()`](https://bluefoxr.github.io/COINr/reference/i_mean.md)).
However, this function offers a few extra advantages, such as checking
the input and output formats, and making sure the resulting imputed
vector agrees with the input. It will also skip imputation entirely if
there are no `NA`s at all.

## Examples

``` r
# a vector with a missing value
x <- 1:10
x[3] <- NA
x
#>  [1]  1  2 NA  4  5  6  7  8  9 10

# impute using median
# this calls COINr's i_median() function
Impute(x, f_i = "i_median")
#>  [1]  1  2  6  4  5  6  7  8  9 10
```
