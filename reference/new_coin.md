# Create a new coin

Creates a new "coin" class object, or a "purse" class object
(time-indexed collection of coins). A purse class object is created if
panel data is supplied. Coins and purses are the main object classes
used in COINr, although a number of functions also support other classes
such as data frames and vectors.

## Usage

``` r
new_coin(
  iData,
  iMeta,
  exclude = NULL,
  split_to = NULL,
  level_names = NULL,
  retain_all_uCodes_on_split = FALSE,
  quietly = FALSE
)
```

## Arguments

- iData:

  The indicator data and metadata of each unit

- iMeta:

  Indicator metadata

- exclude:

  Optional character vector of any indicator codes (`iCode`s) to exclude
  from the coin(s).

- split_to:

  This is used to split panel data into multiple coins, a so-called
  "purse". Should be either `"all"`, or a subset of entries in
  `iData$Time`. See Details.

- level_names:

  Optional character vector of names of levels. Must have length equal
  to the number of levels in the hierarchy
  (`max(iMeta$Level, na.rm = TRUE)`).

- retain_all_uCodes_on_split:

  Logical: if panel data is input and split to a purse using `split_to`,
  this controls how units with no data at certain time points are
  handled. If set `FALSE`, then unit at time t with no data in any
  indicators will be removed completely from the coin for that time
  point. If `TRUE`, all units will be included in every time point. The
  latter option may be useful if you impute over time.

- quietly:

  If `TRUE`, suppresses all messages

## Value

A "coin" object or a "purse" object.

## Details

A coin object is fundamentally created by passing two data frames to
`new_coin()`: `iData` which specifies the data points for each unit and
indicator, as well as other optional variables; and `iMeta` which
specifies details about each indicator/variable found in `iData`,
including its type, name, position in the index, units, and other
properties.

These data frames need to follow fairly strict requirements regarding
their format and consistency. Run
[`check_iData()`](https://bluefoxr.github.io/COINr/reference/check_iData.md)
and
[`check_iMeta()`](https://bluefoxr.github.io/COINr/reference/check_iMeta.md)
to validate your data frames, and these should generate helpful error
messages when things go wrong.

It is worth reading a little about coins and purses to use COINr. See
[`vignette("coins")`](https://bluefoxr.github.io/COINr/articles/coins.md)
for more details.

### `iData`

`iData` should be a data frame with required column `uCode` which gives
the code assigned to each unit (alphanumeric, not starting with a
number). All other columns are defined by corresponding entries in
`iMeta`, with the following special exceptions:

- `Time` is an optional column which allows panel data to be input,
  consisting of e.g. multiple rows for each `uCode`: one for each `Time`
  value. This can be used to split a set of panel data into multiple
  coins (a so-called "purse") which can be input to COINr functions.

- `uName` is an optional column which specifies a longer name for each
  unit. If this column is not included, unit codes (`uCode`) will be
  used as unit names where required.

### `iMeta`

Required columns for `iMeta` are:

- `Level`: Level in aggregation, where 1 is indicator level, 2 is the
  level resulting from aggregating indicators, 3 is the result of
  aggregating level 2, and so on. Set to `NA` for entries that are not
  included in the index (groups, denominators, etc).

- `iCode`: Indicator code, alphanumeric. Must not start with a number.

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

### Other arguments

The `exclude` argument can be used to exclude specified indicators. If
this is specified, `.$Data$Raw` will be built excluding these
indicators, as will all subsequent build operations. However the full
data set will still be stored in `.$Log$new_coin`. The codes here should
correspond to entries in the `iMeta$iCode`. This option is useful e.g.
in generating alternative coins with different indicator sets, and can
be included as a variable in a sensitivity analysis.

The `split_to` argument allows panel data to be used. Panel data must
have a `Time` column in `iData`, which consists of some numerical time
variable, such as a year. Panel data has multiple observations for each
`uCode`, one for each unique entry in `Time`. The `Time` column is
required to be numerical, because it needs to be possible to order it.
To split panel data, specify `split_to = "all"` to split to a single
coin for each of the unique entries in `Time`. Alternatively, you can
pass a vector of entries in `Time` which allows to split to a subset of
the entries to `Time`.

Splitting panel data results in a so-called "purse" class, which is a
data frame of COINs, indexed by `Time`. See
[`vignette("coins")`](https://bluefoxr.github.io/COINr/articles/coins.md)
for more details.

This function replaces the now-defunct `assemble()` from COINr \< v1.0.

## Examples

``` r
# build a coin using example data frames
ASEM_coin <- new_coin(iData = ASEM_iData,
                      iMeta = ASEM_iMeta,
                      level_names = c("Indicator", "Pillar", "Sub-index", "Index"))
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
# view coin contents
ASEM_coin
#> --------------
#> A coin with...
#> --------------
#> Input:
#>   Units: 51 (AUS, AUT, BEL, ...)
#>   Indicators: 49 (Goods, Services, FDI, ...)
#>   Denominators: 4 (Area, Energy, GDP, ...)
#>   Groups: 4 (GDP_group, GDPpc_group, Pop_group, ...)
#> 
#> Structure:
#>   Level 1 Indicator: 49 indicators (FDI, ForPort, Goods, ...) 
#>   Level 2 Pillar: 8 groups (ConEcFin, Instit, P2P, ...) 
#>   Level 3 Sub-index: 2 groups (Conn, Sust) 
#>   Level 4 Index: 1 groups (Index) 
#> 
#> Data sets:
#>   Raw (51 units)

# build example purse class
ASEM_purse <- new_coin(iData = ASEM_iData_p,
                       iMeta = ASEM_iMeta,
                       split_to = "all",
                       quietly = TRUE)
# view purse contents
ASEM_purse
#> -----------------------------
#> A purse with... 5 coins 
#> -----------------------------
#> 
#>  Time n_Units n_Inds n_dsets
#>  2018      51     49       1
#>  2019      51     49       1
#>  2020      51     49       1
#>  2021      51     49       1
#>  2022      51     49       1
#> 
#> -----------------------------------
#> Sample from first coin (2018):
#> -----------------------------------
#> 
#> Input:
#>   Units: 51 (AUS, AUT, BEL, ...)
#>   Indicators: 49 (Goods, Services, FDI, ...)
#>   Denominators: 4 (Area, Energy, GDP, ...)
#>   Groups: 4 (GDP_group, GDPpc_group, Pop_group, ...)
#> 
#> Structure:
#>   Level 1 : 49 indicators (FDI, ForPort, Goods, ...) 
#>   Level 2 : 8 groups (ConEcFin, Instit, P2P, ...) 
#>   Level 3 : 2 groups (Conn, Sust) 
#>   Level 4 : 1 groups (Index) 
#> 
#> Data sets:
#>   Raw (51 units)

# see vignette("coins") for further info
```
