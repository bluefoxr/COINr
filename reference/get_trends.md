# Get time trends

Get time trends from a purse object. This function extracts a panel data
set from a purse, and calculates trends for each indicator/unit pair
using a specified function `f_trend`. For example, if
`f_trend = "CAGR"`, this extracts the time series for each
indicator/unit pair and passes it to
[`CAGR()`](https://bluefoxr.github.io/COINr/reference/CAGR.md).

## Usage

``` r
get_trends(
  purse,
  dset,
  uCodes = NULL,
  iCodes = NULL,
  Time = NULL,
  use_latest = NULL,
  f_trend = "CAGR",
  interp_at = NULL,
  adjust_directions = FALSE
)
```

## Arguments

- purse:

  A purse object

- dset:

  Name of the data set to extract, passed to
  [`get_data.purse()`](https://bluefoxr.github.io/COINr/reference/get_data.purse.md)

- uCodes:

  Optional subset of unit codes to extract, passed to
  [`get_data.purse()`](https://bluefoxr.github.io/COINr/reference/get_data.purse.md)

- iCodes:

  Optional subset of indicator/aggregate codes to extract, passed to
  [`get_data.purse()`](https://bluefoxr.github.io/COINr/reference/get_data.purse.md)

- Time:

  Optional vector of time points to extract, passed to
  [`get_data.purse()`](https://bluefoxr.github.io/COINr/reference/get_data.purse.md)

- use_latest:

  A positive integer which specifies to use only the latest "n" data
  points. If this is specified, it overrides `Time`. If e.g.
  `use_latest = 5`, will use the latest five observations, working
  backwards from the latest non-`NA` point.

- f_trend:

  Function that returns a metric describing the trend of the time
  series. See details.

- interp_at:

  Option to linearly interpolate missing data points in each time
  series. Must be specified as a vector of time values where to apply
  interpolation. If `interp_at = "all"`, will attempt to interpolate at
  every time point. Uses linear interpolation - note that any `NA`s
  outside of the range of observed values will not be estimated, i.e.
  this does not *extrapolate* beyond the range of data. See
  [`approx_df()`](https://bluefoxr.github.io/COINr/reference/approx_df.md).

- adjust_directions:

  Logical: if `TRUE`, trend metrics are adjusted according to
  indicator/aggregate directions input in `iMeta` (i.e. if the
  corresponding direction is -1, the metric will be multiplied by -1).

## Value

A data frame in long format, with trend metrics for each indicator/unit
pair, plus data availability statistics.

## Details

This function requires a purse object as an input. The data set is
selected using
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md),
such that a subset of the data set can be analysed using the `uCodes`,
`iCodes` and `Time` arguments. The latter is useful especially if only a
subset of the time series should be analysed.

The function `f_trend` is a function that, given a time series, returns
a trend metric. This must follow a specific format. It must of course be
available to call, and *must* have arguments `y` and `x`, which are
respectively a vector of values and a vector indexing the values in
time. See
[`prc_change()`](https://bluefoxr.github.io/COINr/reference/prc_change.md)
and [`CAGR()`](https://bluefoxr.github.io/COINr/reference/CAGR.md) for
examples. The function *must* return a single value (not a vector with
multiple entries, or a list). The function can return either numeric or
character values.

## Examples

``` r
#
```
