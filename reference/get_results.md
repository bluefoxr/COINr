# Results summary tables

Generates fast results tables, either attached to the coin or as a data
frame.

## Usage

``` r
get_results(
  coin,
  dset,
  tab_type = "Summ",
  also_get = NULL,
  use = "scores",
  order_by = NULL,
  nround = 2,
  use_group = NULL,
  dset_indicators = NULL,
  out2 = "df"
)
```

## Arguments

- coin:

  The coin object, or a data frame of indicator data

- dset:

  Name of data set in `.$Data`

- tab_type:

  The type of table to generate. Either `"Summ"` (a single indicator
  plus rank), `"Aggs"` (all aggregated scores/ranks above indicator
  level), or `"Full"` (all scores/ranks plus all group, denominator
  columns).

- also_get:

  Names of further columns to attach to table.

- use:

  Either `"scores"` (default), `"ranks"`, or `"groupranks"`. For the
  latter, `use_group` must be specified.

- order_by:

  A code of the indicator or aggregate to sort the table by. If not
  specified, defaults to the highest aggregate level, i.e. the index in
  most cases. If `use_group` is specified, rows will also be sorted by
  the specified group.

- nround:

  The number of decimal places to round numerical values to. Defaults to
  2.

- use_group:

  An optional grouping variable. If specified, the results table
  includes this group column, and if `use = "groupranks"`, ranks will be
  returned with respect to the groups in this column.

- dset_indicators:

  Optional data set from which to take only indicator (level 1) data
  from. This can be set to `"Raw"` for example, so that all aggregates
  come from the aggregated data set, and the indicators come from the
  raw data set. This can make more sense in presenting results in many
  cases, so that the "real" indicator data is visible.

- out2:

  If `"df"`, outputs a data frame (tibble). Else if `"coin"` attaches to
  `.$Results` in an updated coin.

## Value

If `out2 = "df"`, the results table is returned as a data frame. If
`out2 = "coin"`, this function returns an updated coin with the results
table attached to `.$Results`.

## Details

Although results are available in a coin in `.$Data`, the format makes
it difficult to quickly present results. This function generates results
tables that are suitable for immediate presentation, i.e. sorted by
index or other indicators, and only including relevant columns. Scores
are also rounded by default, and there is the option to present scores
or ranks.

See also
[`vignette("results")`](https://bluefoxr.github.io/COINr/articles/results.md)
for more info.

This function replaces the now-defunct `getResults()` from COINr \<
v1.0.

## Examples

``` r
# build full example coin
coin <- build_example_coin(quietly = TRUE)

# get results table
df_results <- get_results(coin, dset = "Aggregated", tab_type = "Aggs")

head(df_results)
#>   uCode Rank Index  Conn  Sust Physical ConEcFin Political Instit   P2P Environ
#> 1   CHE    1 68.38 62.79 73.98    63.41    32.87     74.78  85.94 56.93   77.21
#> 2   NLD    2 64.82 61.94 67.70    64.87    46.62     84.55  71.74 41.92   58.73
#> 3   DNK    3 64.80 57.10 72.51    50.52    30.43     78.13  75.35 51.04   69.83
#> 4   NOR    4 64.47 57.44 71.50    58.18    25.70     80.03  89.46 33.80   70.90
#> 5   BEL    5 63.54 63.99 63.09    71.97    48.22     80.84  75.60 43.32   53.01
#> 6   SWE    6 63.00 53.87 72.12    51.51    23.87     80.98  74.51 38.50   66.37
#>   Social SusEcFin
#> 1  87.85    56.88
#> 2  86.58    57.78
#> 3  86.40    61.29
#> 4  87.00    56.59
#> 5  86.17    50.09
#> 6  87.74    62.27
```
