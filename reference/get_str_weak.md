# Generate strengths and weaknesses for a specified unit

Generates a table of strengths and weaknesses for a selected unit, based
on ranks, or ranks within a specified grouping variable.

## Usage

``` r
get_str_weak(
  coin,
  dset,
  usel = NULL,
  topN = 5,
  bottomN = 5,
  withcodes = TRUE,
  use_group = NULL,
  unq_discard = NULL,
  min_discard = TRUE,
  report_level = NULL,
  with_units = TRUE,
  adjust_direction = NULL,
  sig_figs = 3
)
```

## Arguments

- coin:

  A coin

- dset:

  The data set to extract indicator data from, to use as strengths and
  weaknesses.

- usel:

  A selected unit code

- topN:

  The top N indicators to report

- bottomN:

  The bottom N indicators to report

- withcodes:

  If `TRUE` (default), also includes a column of indicator codes.
  Setting to `FALSE` may be more useful in generating reports, where
  codes are not helpful.

- use_group:

  An optional grouping variable to use for reporting in-group ranks.
  Specifying this will report the ranks of the selected unit within the
  group of `use_group` to which it belongs.

- unq_discard:

  Optional parameter for handling discrete indicators. Some indicators
  may be binary variables of the type "yes = 1", "no = 0". These may be
  picked up as strengths or weaknesses, when they may not be wanted to
  be highlighted, since e.g. maybe half of units will have a zero or a
  one. This argument takes a number between 0 and 1 specifying a unique
  value threshold for ignoring indicators as strengths. E.g. setting
  `prc_unq_discard = 0.2` will ensure that only indicators with at least
  20% unique values will be highlighted as strengths or weaknesses. Set
  to `NULL` to disable (default).

- min_discard:

  If `TRUE` (default), discards any strengths which correspond to the
  minimum rank for the given indicator. See details.

- report_level:

  Aggregation level to report parent codes from. For example, setting
  `report_level = 2` (default) will add a column to the strengths and
  weaknesses tables which reports the aggregation group from level 2, to
  which each reported indicator belongs.

- with_units:

  If `TRUE` (default), includes indicator units in output tables.

- adjust_direction:

  If `TRUE`, will adjust directions of indicators according to the
  "Direction" column of `IndMeta`. By default, this is `TRUE` *if*
  `dset = "Raw"`, and `FALSE` otherwise.

- sig_figs:

  Number of significant figures to round values to. If `NULL` returns
  values as they are.

## Value

A list containing a data frame `.$Strengths`, and a data frame
`.$Weaknesses`. Each data frame has columns with indicator code, name,
rank and value (for the selected unit).

## Details

This currently only works at the indicator level. Indicators with `NA`
values for the selected unit are ignored. Strengths and weaknesses mean
the `topN`-ranked indicators for the selected unit. Effectively, this
takes the rank that the selected unit has in each indicator, sorts the
ranks, and takes the top N highest and lowest.

This function must be used with a little care: indicators should be
adjusted for their directions before use, otherwise a weakness might be
counted as a strength, and vice versa. Use the `adjust_direction`
parameter to help here.

A further useful parameter is `unq_discard`, which also filters out any
indicators with a low number of unique values, based on a specified
threshold. Also `min_discard` which filters out any indicators which
have the minimum rank.

The best way to use this function is to play around with the settings a
little bit. The reason being that in practice, indicators have very
different distributions and these can sometimes lead to unexpected
outcomes. An example is if you have an indicator with 50% zero values,
and the rest non-zero (but unique). Using the sport ranking system, all
units with zero values will receive a rank which is equal to the number
of units divided by two. This then might be counted as a "strength" for
some units with overall low scores. But a zero value can hardly be
called a strength. This is where the `min_discard` function can help
out.

Problems such as these mainly arise when e.g. generating a large number
of country profiles.

This function replaces the now-defunct `getStrengthNWeak()` from COINr
\< v1.0.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# get strengths and weaknesses for ESP
get_str_weak(coin, dset = "Raw", usel = "ESP")
#> $Strengths
#>        Code                                     Name    Pillar Rank Value
#> 1 CostImpEx                    Cost to export/import    Instit    1   0.0
#> 2    TIRcon              Signatory of TIR Convention    Instit    1   1.0
#> 3   Tourist     Tourist arrivals at national borders       P2P    2  75.3
#> 4   Flights International flights passenger capacity  Physical    3 171.0
#> 5    UNVote                      UN voting alignment Political    3  43.1
#>               Unit
#> 1      Current USD
#> 2 (1 (yes)/0 (no))
#> 3 Number of people
#> 4   Thousand seats
#> 5            Score
#> 
#> $Weaknesses
#>      Code                                                         Name   Pillar
#> 1    NEET Proportion of youth not in education, employment or training SusEcFin
#> 2    Lang                                       Common languages users      P2P
#> 3  Forest                                              Net forest loss  Environ
#> 4    TBTs                                  Technical barriers to trade   Instit
#> 5 PubDebt                           Public debt as a percentage of GDP SusEcFin
#>   Rank   Value                                     Unit
#> 1   33   13.30                                  Percent
#> 2   38    5.16                                    Score
#> 3   40    6.93                                  Percent
#> 4   40 1210.00 Number of measures (initiated, in force)
#> 5   43   99.30                                  Percent
#> 
```
