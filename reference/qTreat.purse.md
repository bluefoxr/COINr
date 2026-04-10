# Quick outlier treatment of a purse

A simplified version of
[`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md) which
allows direct access to the default parameters. This has less
flexibility, but is an easier interface and probably more convenient if
the objective is to use the default treatment process but with some
minor adjustments.

## Usage

``` r
# S3 method for class 'purse'
qTreat(
  x,
  dset,
  winmax = 5,
  skew_thresh = 2,
  kurt_thresh = 3.5,
  f2 = "log_CT",
  ...
)
```

## Arguments

- x:

  A purse

- dset:

  Name of data set to treat for outliers in each coin

- winmax:

  Maximum number of points to Winsorise for each indicator. Default 5.

- skew_thresh:

  Absolute skew threshold - default 2.

- kurt_thresh:

  Kurtosis threshold - default 3.5.

- f2:

  Function to call if Winsorisation does not bring skew and kurtosis
  within limits. Default `"log_CT"`.

- ...:

  arguments passed to or from other methods.

## Value

An updated purse

## Details

This function simply applies the same data treatment to each coin. See
documentation for
[`Treat.coin()`](https://bluefoxr.github.io/COINr/reference/Treat.coin.md),
[`qTreat.coin()`](https://bluefoxr.github.io/COINr/reference/qTreat.coin.md)
and
[`vignette("treat")`](https://bluefoxr.github.io/COINr/articles/treat.md).

## Examples

``` r
#
```
