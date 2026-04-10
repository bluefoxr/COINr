# Treat a purse of coins for outliers

This function calls
[`Treat.coin()`](https://bluefoxr.github.io/COINr/reference/Treat.coin.md)
for each coin in the purse. See the documentation of that function for
details. See also
[`vignette("treat")`](https://bluefoxr.github.io/COINr/articles/treat.md).

## Usage

``` r
# S3 method for class 'purse'
Treat(
  x,
  dset,
  global_specs = NULL,
  indiv_specs = NULL,
  combine_treat = FALSE,
  write_to = NULL,
  disable = FALSE,
  ...
)
```

## Arguments

- x:

  A purse object

- dset:

  The data set to treat in each coin.

- global_specs:

  Default specifications. See details in
  [`Treat.coin()`](https://bluefoxr.github.io/COINr/reference/Treat.coin.md).

- indiv_specs:

  Individual specifications. See details in
  [`Treat.coin()`](https://bluefoxr.github.io/COINr/reference/Treat.coin.md).

- combine_treat:

  By default, if `f1` fails to pass `f_pass`, then `f2` is applied to
  the original `x`, rather than the treated output of `f1`. If
  `combine_treat = TRUE`, `f2` will instead be applied to the output of
  `f1`, so the two treatments will be combined.

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Treated"`.

- disable:

  Logical: if `TRUE` will disable data treatment completely and write
  the unaltered data set. This option is mainly useful in sensitivity
  and uncertainty analysis (to test the effect of turning imputation
  on/off).

- ...:

  arguments passed to or from other methods.

## Value

An updated purse with new treated data sets added at `.$Data$Treated` in
each coin, plus analysis information at `.$Analysis$Treated`

## Examples

``` r
# See `vignette("treat")`.
```
