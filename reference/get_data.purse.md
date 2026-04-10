# Get subsets of indicator data

This retrieves data from a purse. It functions in a similar way to
[`get_data.coin()`](https://bluefoxr.github.io/COINr/reference/get_data.coin.md)
but has the additional `Time` argument to allow selection based on the
point(s) in time.

## Usage

``` r
# S3 method for class 'purse'
get_data(
  x,
  dset,
  iCodes = NULL,
  Level = NULL,
  uCodes = NULL,
  use_group = NULL,
  Time = NULL,
  also_get = NULL,
  ...
)
```

## Arguments

- x:

  A purse class object

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- iCodes:

  Optional indicator codes to retrieve. If `NULL` (default), returns all
  iCodes found in the selected data set. Can also refer to indicator
  groups. See details.

- Level:

  Optionally, the level in the hierarchy to extract data from. See
  details.

- uCodes:

  Optional unit codes to filter rows of the resulting data set. Can also
  be used in conjunction with groups. See details.

- use_group:

  Optional group to filter rows of the data set. Specified as
  `list(Group_Var = Group)`, where `Group_Var` is a Group\_ column that
  must be present in the selected data set, and `Group` is a specified
  group inside that grouping variable. This filters the selected data to
  only include rows from the specified group. Can also be used in
  conjunction with `uCodes` – see details.

- Time:

  Optional time index to extract from a subset of the coins present in
  the purse. Should be a vector containing one or more entries in
  `x$Time` or `NULL` to return all (default).

- also_get:

  A character vector specifying any columns to attach to the data set
  that are *not* indicators or aggregates. These will be e.g. `uName`,
  groups, denominators or columns labelled as "Other" in `iMeta`. These
  columns are stored in `.$Meta$Unit` to avoid repetition. Set
  `also_get = "all"` to attach all columns, or set `also_get = "none"`
  to return only numeric columns, i.e. no `uCode` column.

- ...:

  arguments passed to or from other methods.

## Value

A data frame of indicator data indexed by a "Time" column.

## Details

Note that

## Examples

``` r
# build full example purse
purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)

# get specified indicators for specific years, for specified units
get_data(purse, dset = "Raw",
         iCodes = c("Lang", "Forest"),
         uCodes = c("AUT", "CHN", "DNK"),
         Time = c(2019, 2020))
#>     Time uCode       Lang   Forest
#> 52  2019   AUT 14.9590871 4.510501
#> 86  2019   CHN  0.9909247 4.007399
#> 58  2019   DNK 19.5377677 6.626489
#> 103 2020   AUT 16.2053167 5.150510
#> 137 2020   CHN  1.2525769 4.322223
#> 109 2020   DNK 19.3664297 5.323871
```
