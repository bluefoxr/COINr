# Compare multiple coins

Given multiple coins as a list, generates a rank comparison of a single
indicator or aggregate which is specified by the `dset` and `iCode`
arguments (passed to
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)).
The indicator or aggregate targeted must be available in all the coins
in `coins`.

## Usage

``` r
compare_coins_multi(
  coins,
  dset,
  iCode,
  also_get = NULL,
  tabtype = "Values",
  ibase = 1,
  sort_table = TRUE,
  compare_by = "ranks"
)
```

## Arguments

- coins:

  A list of coins. If names are provided, these will be used in the
  tables returned by this function.

- dset:

  The name of a data set found in `.$Data`. See
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).

- iCode:

  A column name of the data set targeted by `dset`. See
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).

- also_get:

  Optional metadata columns to attach to the table: see
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).
  If this is not specified, the results of each coin will be merged
  using the `uCode`s within each coin. If this is specified, results
  will be merged additionally using the metadata columns. This means
  that coins must share the same metadata columns that are returned as a
  result of `also_get`.

- tabtype:

  The type of table to generate. One of:

  - `"Values"`: returns a data frame of rank values for each coin
    provided, plus ISO3 column

  - `"Diffs"`: returns a data frame of rank differences between the base
    coin and each other coin (see `ibase`)

  - `"AbsDiffs"`: as `"Diffs"` but absolute rank differences are
    returned

  - `"All"`: returns all of the three previous rank tables, as a list of
    data frames

- ibase:

  The index of the coin to use as a base comparison (default first coin
  in list)

- sort_table:

  If TRUE, sorts by the base COIN (`ibase`) (default).

- compare_by:

  Either `"ranks"` which produces a comparison using ranks, or else
  `"scores"`, which instead uses scores. Note that scores may be very
  different if the methodology is different from one coin to another,
  e.g. for different normalisation methods.

## Value

Data frame unless `tabtype = "All"`, in which case a list of three data
frames is returned.

## Details

By default, the ranks of the target indicator/aggregate of each coin
will be merged using the `uCode`s within each coin. Optionally,
specifying `also_get` (passed to
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md))
will additionally merge using the metadata columns. This means that
coins must share the same metadata columns that are returned as a result
of `also_get`.

This function replaces the now-defunct `compTableMulti()` from COINr \<
v1.0.

## Examples

``` r
# see vignette("adjustments")
```
