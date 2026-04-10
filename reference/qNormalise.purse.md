# Quick normalisation of a purse

This is a wrapper function for
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md),
which offers a simpler syntax but less flexibility. It normalises data
sets within a purse using a specified function `f_n` which is used to
normalise each indicator, with additional function arguments passed by
`f_n_para`. By default, `f_n = "n_minmax"` and `f_n_para` is set so that
the indicators are normalised using the min-max method, between 0 and
100.

## Usage

``` r
# S3 method for class 'purse'
qNormalise(
  x,
  dset,
  f_n = "n_minmax",
  f_n_para = list(l_u = c(0, 100)),
  directions = NULL,
  global = TRUE,
  ...
)
```

## Arguments

- x:

  A purse

- dset:

  Name of data set to normalise

- f_n:

  Name of a normalisation function (as a string) to apply to each
  indicator. Default `"n_minmax"`.

- f_n_para:

  Any further arguments to pass to `f_n`, as a named list.

- directions:

  An optional data frame containing the following columns:

  - `iCode` The indicator code, corresponding to the column names of the
    data frame

  - `Direction` numeric vector with entries either `-1` or `1` If
    `directions` is not specified, the directions will be taken from the
    `iMeta` table in the coin, if available.

- global:

  Logical: if `TRUE`, normalisation is performed "globally" across all
  coins, by using e.g. the max and min of each indicator in any coin.
  This effectively makes normalised scores comparable between coins
  because they are all scaled using the same parameters. Otherwise if
  `FALSE`, coins are normalised individually.

- ...:

  arguments passed to or from other methods.

## Value

An updated purse with normalised data sets

## Details

Essentially, this function is similar to
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
but brings parameters into the function arguments rather than being
wrapped in a list. It also does not allow individual normalisation.

Normalisation can either be performed independently on each coin, or
over the entire panel data set simultaneously. See the discussion in
[`Normalise.purse()`](https://bluefoxr.github.io/COINr/reference/Normalise.purse.md)
and
[`vignette("normalise")`](https://bluefoxr.github.io/COINr/articles/normalise.md).

## Examples

``` r
# build example purse
purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)

# normalise using min-max, globally
purse <- qNormalise(purse, dset = "Raw", global = TRUE)
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
```
