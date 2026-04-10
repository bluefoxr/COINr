# Compare two coins

Compares two coin class objects using a specified `iCode` (column of
data) from specified data sets.

## Usage

``` r
compare_coins(
  coin1,
  coin2,
  dset,
  iCode,
  also_get = NULL,
  compare_by = "ranks",
  sort_by = NULL,
  decreasing = FALSE
)
```

## Arguments

- coin1:

  A coin class object

- coin2:

  A coin class object

- dset:

  A data set that is found in `.$Data`.

- iCode:

  The name of a column that is found in `.$Data[[dset]]`.

- also_get:

  Optional metadata columns to attach to the table: see
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).

- compare_by:

  Either `"ranks"` which produces a comparison using ranks, or else
  `"scores"`, which instead uses scores. Note that scores may be very
  different if the methodology is different from one coin to another,
  e.g. for different normalisation methods.

- sort_by:

  Optionally, a column name of the output data frame to sort rows by.
  Can be either `"coin.1"`, `"coin.2"`, `"Diff"`, `"Abs.diff"` or
  possibly a column name imported using `also_get`.

- decreasing:

  Argument to pass to [`order()`](https://rdrr.io/r/base/order.html):
  how to sort.

## Value

A data frame of comparison information.

## Details

This function replaces the now-defunct `compTable()` from COINr \< v1.0.

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
