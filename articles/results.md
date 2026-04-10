# Presenting Results

This is a short vignette which explains some functions in COINr for
extracting results from the coin. Once the coin is fully built, up to
the point of aggregation, an immediate task is to see what the main
results are. In composite indicators, the main starting point is often
the ranking of units based on the highest level of aggregation, i.e. the
index.

While the aggregated data set (the data set created by
[`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md))
has all the aggregate scores in it, it requires a little manipulation to
see it in an easy to read format. To help with this, the
[`get_results()`](https://bluefoxr.github.io/COINr/reference/get_results.md)
function

``` r
library(COINr)

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

The output of
[`get_results()`](https://bluefoxr.github.io/COINr/reference/get_results.md)
is a table sorted by the highest level of aggregation (here, the index),
and with the the columns arranged so that the highest level of
aggregation is first, working down to lower levels. The function has
several arguments, including `also_get` (names of further columns to
attach to the table, such as groups, denominators), `tab_type`
(controlling which columns to output), `use` (whether to show scores or
ranks), and `order_by` (which column to use to sort the table).

A useful feature is to return ranks of units inside groups. For example,
rather than returning scores we can return ranks within GDP per capita
groups:

``` r
# get results table
df_results <- get_results(coin, dset = "Aggregated", tab_type = "Aggs", use_group = "GDPpc_group", use = "groupranks")

# see first few entries in "XL" group
head(df_results[df_results$GDPpc_group == "XL", ])
#>   uCode GDPpc_group Index Conn Sust Physical ConEcFin Political Instit P2P
#> 1   CHE          XL     1    2    1        3        5         8      2   2
#> 2   NLD          XL     2    3    6        2        4         2     10   6
#> 3   DNK          XL     3    6    2        8        6         6      6   5
#> 4   NOR          XL     4    5    4        4        8         4      1  11
#> 6   SWE          XL     5   10    3        7       10         3      8  10
#> 7   AUT          XL     6    7    5       11        7         5      4   4
#>   Environ Social SusEcFin
#> 1       1      1        8
#> 2       8      4        7
#> 3       3      5        5
#> 4       2      3       10
#> 6       5      2        3
#> 7       4      9        2
```

``` r
# see first few entries in "L" group
head(df_results[df_results$GDPpc_group == "L", ])
#>    uCode GDPpc_group Index Conn Sust Physical ConEcFin Political Instit P2P
#> 5    BEL           L     1    1    2        1        2         4      7   3
#> 10   MLT           L     2    2    5        3        1        12      4   1
#> 11   SVN           L     3    4    1        2        6         9      6   5
#> 15   GBR           L     4    3    3        4        8         5      5   7
#> 17   CZE           L     5    5    4        9        5         8     10   6
#> 19   ESP           L     6    7    7        7       10         2      3   9
#>    Environ Social SusEcFin
#> 5       10      1        9
#> 10       2     10        3
#> 11       5      3        2
#> 15       3      2        8
#> 17      12      9        1
#> 19       6      6       10
```

Another function of interest zooms in on a single unit. The
[`get_unit_summary()`](https://bluefoxr.github.io/COINr/reference/get_unit_summary.md)
function returns a summary of a units scores and ranks at specified
levels. Typically we can use this to look at a unit’s index scores and
scores for the aggregates:

``` r
get_unit_summary(coin, usel = "IND", Levels = c(4,3,2), dset = "Aggregated")
#>         Code                         Name Score Rank
#> 1      Index     Sustainable Connectivity 39.79   45
#> 2       Conn                 Connectivity 28.60   44
#> 3       Sust               Sustainability 50.99   43
#> 4   Physical                     Physical 18.78   48
#> 5   ConEcFin Economic and Financial (Con)  6.38   49
#> 6  Political                    Political 51.90   33
#> 7     Instit                Institutional 59.84   37
#> 8        P2P             People to People  6.07   47
#> 9    Environ                Environmental 75.34    6
#> 10    Social                       Social 22.74   51
#> 11  SusEcFin Economic and Financial (Sus) 54.88   36
```

This is a summary for “IND” (India) at levels 4 (index), 3 (sub-index)
and 2 (pillar). It shows the score and rank.

A final function here is
[`get_str_weak()`](https://bluefoxr.github.io/COINr/reference/get_str_weak.md).
This gives the “strengths and weaknesses” of a unit, in terms of its
indicators with the highest and lowest ranks. This can be particularly
useful in “country profiles”, for example.

``` r
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
```

The default output is five strengths and five weaknesses. The direction
of the indicators is adjusted - see the `adjust_direction` parameter. A
number of other parameters can also be adjusted which help to guide the
tables to give sensible values, for example excluding indicators with
binary values. See the function documentation for more details.
