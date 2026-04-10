# Regenerate a coin

Regenerates the `.$Data` entries in a coin by rerunning the construction
functions according to the specifications in `.$Log`. This effectively
regenerates the results. Different variations of coins can be quickly
achieved by editing the saved arguments in `.$Log` and regenerating.

## Usage

``` r
# S3 method for class 'coin'
Regen(x, from = NULL, quietly = TRUE, ...)
```

## Arguments

- x:

  A coin class object

- from:

  Optional: a construction function name. If specified, regeneration
  begins from this function, rather than re-running all functions.

- quietly:

  If `TRUE` (default), messages are suppressed during building.

- ...:

  arguments passed to or from other methods.

## Value

Updated coin object with regenerated results (data sets).

## Details

The `from` argument allows partial regeneration, starting from a
specified function. This can be helpful to speed up regeneration in some
cases. However, keep in mind that if you change a `.$Log` argument from
a function that is run before the point that you choose to start running
from, it will not affect the results.

Note that while sets of weights will be passed to the regenerated COIN,
anything in `.$Analysis` will be removed and will have to be
recalculated.

See also
[`vignette("adjustments")`](https://bluefoxr.github.io/COINr/articles/adjustments.md)
for more info on regeneration.

## Examples

``` r
# build full example coin
coin <- build_example_coin(quietly = TRUE)

# copy coin
coin2 <- coin

# change to prank function (percentile ranks)
# we don't need to specify any additional parameters (f_n_para) here
coin2$Log$Normalise$global_specs <- list(f_n = "n_prank")

# regenerate
coin2 <- Regen(coin2)

# compare index, sort by absolute rank difference
compare_coins(coin, coin2, dset = "Aggregated", iCode = "Index",
              sort_by = "Abs.diff", decreasing = TRUE)
#>    uCode coin.1 coin.2 Diff Abs.diff
#> 43   PRT     27     17   10       10
#> 29   LAO     48     39    9        9
#> 33   MLT     10     19   -9        9
#> 14   EST     22     16    6        6
#> 21   IDN     43     49   -6        6
#> 13   ESP     19     24   -5        5
#> 19   HRV     18     23   -5        5
#> 30   LTU     16     11    5        5
#> 35   MNG     44     48   -4        4
#> 17   GBR     15     12    3        3
#> 25   JPN     34     31    3        3
#> 32   LVA     23     20    3        3
#> 40   PAK     50     47    3        3
#> 3    BEL      5      7   -2        2
#> 4    BGD     46     44    2        2
#> 8    CHN     49     51   -2        2
#> 20   HUN     20     22   -2        2
#> 23   IRL     12     14   -2        2
#> 26   KAZ     47     45    2        2
#> 28   KOR     31     33   -2        2
#> 31   LUX      8     10   -2        2
#> 37   NLD      2      4   -2        2
#> 41   PHL     38     40   -2        2
#> 42   POL     26     28   -2        2
#> 47   SVK     24     26   -2        2
#> 48   SVN     11      9    2        2
#> 2    AUT      7      6    1        1
#> 5    BGR     30     29    1        1
#> 6    BRN     40     41   -1        1
#> 9    CYP     29     30   -1        1
#> 10   CZE     17     18   -1        1
#> 11   DEU      9      8    1        1
#> 12   DNK      3      2    1        1
#> 22   IND     45     46   -1        1
#> 24   ITA     28     27    1        1
#> 27   KHM     37     36    1        1
#> 34   MMR     41     42   -1        1
#> 36   MYS     39     38    1        1
#> 38   NOR      4      3    1        1
#> 39   NZL     33     34   -1        1
#> 45   RUS     51     50    1        1
#> 46   SGP     14     15   -1        1
#> 49   SWE      6      5    1        1
#> 50   THA     42     43   -1        1
#> 51   VNM     36     37   -1        1
#> 1    AUS     35     35    0        0
#> 7    CHE      1      1    0        0
#> 15   FIN     13     13    0        0
#> 16   FRA     21     21    0        0
#> 18   GRC     32     32    0        0
#> 44   ROU     25     25    0        0
```
