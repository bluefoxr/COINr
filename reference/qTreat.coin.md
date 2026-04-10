# Quick outlier treatment of a coin

A simplified version of
[`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md) which
allows direct access to the default parameters. This has less
flexibility, but is an easier interface and probably more convenient if
the objective is to use the default treatment process but with some
minor adjustments.

## Usage

``` r
# S3 method for class 'coin'
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

  A coin

- dset:

  Name of data set to treat for outliers

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

An updated coin with treated data set at `.$Data$Treated`.

## Details

This function treats each indicator in the data set targeted by `dset`
using the following process:

- First, it checks whether skew and kurtosis are within the specified
  limits of `skew_thresh` and `kurt_thresh`

- If the indicator is not within the limits, it applies the
  [`winsorise()`](https://bluefoxr.github.io/COINr/reference/winsorise.md)
  function, with maximum number of winsorised points specified by
  `winmax`.

- If winsorisation does not bring the indicator within the skew/kurtosis
  limits, it is instead passed to `f2`, which is a second outlier
  treatment function, default
  [`log_CT()`](https://bluefoxr.github.io/COINr/reference/log_CT.md).

The arguments of
[`qTreat()`](https://bluefoxr.github.io/COINr/reference/qTreat.md) are
passed to
[`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md).

See [`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md)
documentation for more details, and
[`vignette("treat")`](https://bluefoxr.github.io/COINr/articles/treat.md).

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# treat with winmax = 3
coin <- qTreat(coin, dset = "Raw", winmax = 3)
#> Written data set to .$Data$Treated
```
