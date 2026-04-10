# Check iMeta

Checks the format of `iMeta` input to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
This performs a series of thorough checks to make sure that `iMeta`
agrees with the specifications. This also includes checks to make sure
the structure makes sense, there are no duplicates, and other things.
`iMeta` must pass this check to build a new coin.

## Usage

``` r
check_iMeta(iMeta, quietly = FALSE)
```

## Arguments

- iMeta:

  A data frame of indicator metadata. See details.

- quietly:

  Set `TRUE` to suppress message if input is valid.

## Value

Message if everything ok, else error messages.

## Details

Required columns for `iMeta` are:

- `Level`: Level in aggregation, where 1 is indicator level, 2 is the
  level resulting from aggregating indicators, 3 is the result of
  aggregating level 2, and so on. Set to `NA` for entries that are not
  included in the index (groups, denominators, etc).

- `iCode`: Indicator code, alphanumeric. Must not start with a number or
  contain blank spaces.

- `Parent`: Group (`iCode`) to which indicator/aggregate belongs in
  level immediately above. Each entry here should also be found in
  `iCode`. Set to `NA` only for the highest (Index) level (no parent),
  or for entries that are not included in the index (groups,
  denominators, etc).

- `Direction`: Numeric, either -1 or 1

- `Weight`: Numeric weight, will be rescaled to sum to 1 within
  aggregation group. Set to `NA` for entries that are not included in
  the index (groups, denominators, etc).

- `Type`: The type, corresponding to `iCode`. Can be either `Indicator`,
  `Aggregate`, `Group`, `Denominator`, or `Other`.

Optional columns that are recognised in certain functions are:

- `iName`: Name of the indicator: a longer name which is used in some
  plotting functions.

- `Unit`: the unit of the indicator, e.g. USD, thousands, score, etc.
  Used in some plots if available.

- `Target`: a target for the indicator. Used if normalisation type is
  distance-to-target.

The `iMeta` data frame essentially gives details about each of the
columns found in `iData`, as well as details about additional data
columns eventually created by aggregating indicators. This means that
the entries in `iMeta` must include *all* columns in `iData`, *except*
the three special column names: `uCode`, `uName`, and `Time`. In other
words, all column names of `iData` should appear in `iMeta$iCode`,
except the three special cases mentioned. The `iName` column optionally
can be used to give longer names to each indicator which can be used for
display in plots.

`iMeta` also specifies the structure of the index, by specifying the
parent of each indicator and aggregate. The `Parent` column must refer
to entries that can be found in `iCode`. Try `View(ASEM_iMeta)` for an
example of how this works.

`Level` is the "vertical" level in the hierarchy, where 1 is the bottom
level (indicators), and each successive level is created by aggregating
the level below according to its specified groups.

`Direction` is set to 1 if higher values of the indicator should result
in higher values of the index, and -1 in the opposite case.

The `Type` column specifies the type of the entry: `Indicator` should be
used for indicators at level 1. `Aggregate` for aggregates created by
aggregating indicators or other aggregates. Otherwise set to `Group` if
the variable is not used for building the index but instead is for
defining groups of units. Set to `Denominator` if the variable is to be
used for scaling (denominating) other indicators. Finally, set to
`Other` if the variable should be ignored but passed through. Any other
entries here will cause an error.

Note: this function requires the columns above as specified, but extra
columns can also be added without causing errors.

## Examples

``` r
check_iMeta(ASEM_iMeta)
#> iMeta checked and OK.
```
