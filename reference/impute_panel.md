# Impute panel data

Given a data frame of panel data, with a time-index column `time_col`
and a unit ID column `unit_col`, imputes other columns using the entry
from the latest available time point.

## Usage

``` r
impute_panel(
  iData,
  time_col = NULL,
  unit_col = NULL,
  cols = NULL,
  imp_type = NULL,
  max_time = NULL
)
```

## Arguments

- iData:

  A data frame of indicator data, containing a time index column
  `time_col`, a unit code column `unit_col`, and other numerical columns
  to be imputed.

- time_col:

  The name of a column found in `iData` to be used as the time index
  column. Must point to a numeric column.

- unit_col:

  The name of a column found in `iData` to be used as the unit code/ID
  column. Must point to a character column.

- cols:

  Optionally, a character vector of names of columns to impute. If
  `NULL` (default), all columns apart from `time_col` and `unit_col`
  will be imputed where possible.

- imp_type:

  One of `"latest"` `"constant"`, `"linear"` or `"linear-constant"`. In
  the first case, missing points are imputed with the last non-`NA`
  observation for each time series, up to `max_time`. For `"constant"`
  or `"linear"`, missing points are imputed using
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html), passing
  `"constant"` or `"linear"` to the `method` argument, and points
  outside of the range of observed values are replaced with the nearest
  non-`NA` point. This is equivalent to `rule = 2` in
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) for each
  time series. The difference between `"latest"` and `"constant"` is
  that the latter allows control over the maximum number of time points
  to impute backwards (using `max_time`) whereas the former doesn't.
  Additionally, `"constant"` will impute outside of the observed range
  of values at the beginning of the time series, whereas `"latest"`
  won't. Finally, the `"linear-constant"` option will apply linear
  imputation where possible, but will revert to the "constant" method
  for any time series with only one observation, which would otherwise
  throw an error for "linear".

- max_time:

  The maximum number of time points to look backwards to impute from.
  E.g. if `max_time = 1`, if an `NA` is found at time \\t\\, it will
  only look for a replacement value at \\t-1\\ but not in any time
  points before that. By default, searches all time points available.

## Value

A list containing:

- `.$iData_imp`: An `iData` format data frame with missing data imputed
  using previous time points (where possible).

- `.$DataT`: A data frame in the same format as `iData`, where each
  entry shows which time point each data point came from.

## Details

This presumes that there are multiple observations for each unit code,
i.e. one per time point. It then searches for any missing values in the
target year, and replaces them with the equivalent points from previous
time points. It will replace using the most recently available point or
using linear interpolation: see `imp_type` argument.

## Examples

``` r
# Copy example panel data
iData_p <- ASEM_iData_p

# we introduce two NAs: one for NZ in 2022 in LPI indicator
iData_p$LPI[iData_p$uCode == "NZ" & iData_p$Time == 2022] <-  NA
# one for AT, also in 2022, but for Flights indicator
iData_p$Flights[iData_p$uCode == "AT" & iData_p$Time == 2022] <- NA

# impute: target only the two columns where NAs introduced
l_imp <- impute_panel(iData_p, cols = c("LPI", "Flights"))
# get imputed df
iData_imp <- l_imp$iData_imp

# check the output is what we expect: both NAs introduced should now have 2021 values
iData_imp$LPI[iData_imp$uCode == "NZ" & iData_imp$Time == 2022] ==
  ASEM_iData_p$LPI[ASEM_iData_p$uCode == "NZ" & ASEM_iData_p$Time == 2021]
#> logical(0)

iData_imp$Flights[iData_imp$uCode == "AT" & iData_imp$Time == 2022] ==
  ASEM_iData_p$Flights[ASEM_iData_p$uCode == "AT" & ASEM_iData_p$Time == 2021]
#> logical(0)
```
