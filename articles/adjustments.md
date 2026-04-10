# Adjustments and Comparisons

One of the most powerful features of COINr is the possibility to copy,
adjust and compare coins. A coin is structured list that represents a
composite indicator. Since it is an R object like any other, it can be
copied and modified, and alternative versions can be easily compared.
This generally requires four steps:

1.  Make a copy of the coin
2.  Adjust the coin
3.  Regenerate the coin
4.  Compare coins

These will be explained in the following sections.

## Regeneration

The first three points on the list above will be addressed here. We must
begin by explaining the “Log” of a coin. In COINr, some functions are
distinguished as “building functions”. These functions start with a
capital letter (with one exception), and have the following defining
features:

1.  When a building function is run, it creates a new data set in
    `.$Data`.
2.  When a building function is run, it records its function arguments
    in `.$Log`.

Building functions are the following:

| Function                                                                   | Description                                                |
|:---------------------------------------------------------------------------|:-----------------------------------------------------------|
| [`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)     | Initialise a coin object given indicator data and metadata |
| [`Screen()`](https://bluefoxr.github.io/COINr/reference/Screen.md)         | Screen units based on data availability rules              |
| [`Denominate()`](https://bluefoxr.github.io/COINr/reference/Denominate.md) | Denominate/scale indicators by other indicators            |
| [`Impute()`](https://bluefoxr.github.io/COINr/reference/Impute.md)         | Impute missing data                                        |
| [`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md)           | Treat outliers and skewed distributions                    |
| [`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)   | Normalise indicators onto a common scale                   |
| [`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md)   | Aggregate indicators using weighted mean                   |

Let’s explain the concept of the “Log” now with an example. We will
build the example coin manually, then look inside the coin’s Log list:

``` r
library(COINr)

# create new coin by calling new_coin()
coin <- new_coin(ASEM_iData, ASEM_iMeta,
                 level_names = c("Indicator", "Pillar", "Sub-index", "Index"))
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw

# look in log
str(coin$Log, max.level = 2)
#> List of 2
#>  $ new_coin :List of 6
#>   ..$ iData                     :'data.frame':   51 obs. of  60 variables:
#>   ..$ iMeta                     :'data.frame':   68 obs. of  10 variables:
#>   ..$ exclude                   : NULL
#>   ..$ level_names               : chr [1:4] "Indicator" "Pillar" "Sub-index" "Index"
#>   ..$ retain_all_uCodes_on_split: logi FALSE
#>   ..$ quietly                   : logi FALSE
#>  $ can_regen: logi TRUE
```

Looking in the log, we can see that it is a list with an entry
“new_coin”, which contains exactly the arguments that we passed to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md):
`iData`, `iMeta`, the level names, and two other arguments which are the
default values of the function. There is also another logical variable
called `can_regen` which is for internal use only.

This demonstrates that when we call a building function, its arguments
are stored in the coin. To show another example, if we apply the
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
function:

``` r
# normalise
coin <- Normalise(coin, dset = "Raw")
#> Written data set to .$Data$Normalised

# view log
str(coin$Log, max.level = 2)
#> List of 3
#>  $ new_coin :List of 6
#>   ..$ iData                     :'data.frame':   51 obs. of  60 variables:
#>   ..$ iMeta                     :'data.frame':   68 obs. of  10 variables:
#>   ..$ exclude                   : NULL
#>   ..$ level_names               : chr [1:4] "Indicator" "Pillar" "Sub-index" "Index"
#>   ..$ retain_all_uCodes_on_split: logi FALSE
#>   ..$ quietly                   : logi FALSE
#>  $ can_regen: logi TRUE
#>  $ Normalise:List of 7
#>   ..$ dset        : chr "Raw"
#>   ..$ global_specs: NULL
#>   ..$ indiv_specs : NULL
#>   ..$ directions  : NULL
#>   ..$ out2        : chr "coin"
#>   ..$ write_to    : NULL
#>   ..$ write2log   : logi TRUE
```

Now we additionally have a “Normalise” entry, with all the function
arguments that we specified, plus defaults.

Now, the reason that building functions write to the log, is that it
allows coins to be *regenerated*, which means automatically re-running
the building functions that were used to create the coin and its data
sets. This is done with a function called
[`Regen()`](https://bluefoxr.github.io/COINr/reference/Regen.md):

``` r
# regenerate the coin
coin <- Regen(coin, quietly = FALSE)
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Normalised
```

When [`Regen()`](https://bluefoxr.github.io/COINr/reference/Regen.md) is
called, it runs the buildings *in the order that they are found in the
log*. This is an important point because if you iteratively re-run
building functions, you might end up with an order that is not what you
expect. You can check the log if you have any doubts (anyway you would
probably encounter an error if the order is incorrect). Also, each
building function can only be run once in a regeneration.

So why regenerate coins - aren’t the results exactly the same? Yes,
unless you modify something first. And this brings us to the copying and
modifying points. Let us take an example: first, we’ll build the full
example coin, then we’ll make a copy of our existing coin:

``` r
# build full example coin
coin <- build_example_coin(quietly = TRUE)

# copy coin
coin2 <- coin
```

At this point, the coins are identical. What if we want to test an
alternative methodology, for example a different normalisation method?
This can be done by editing the Log of the coin, then regenerating.
Here, we will change the normalisation method to percentile ranks, and
regenerate. To make this change it is necessary to target the right
argument. Let’s first see what is already in the Log for
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md):

``` r
str(coin2$Log$Normalise)
#> List of 7
#>  $ dset        : chr "Treated"
#>  $ global_specs:List of 2
#>   ..$ f_n     : chr "n_minmax"
#>   ..$ f_n_para:List of 1
#>   .. ..$ : num [1:2] 0 100
#>  $ indiv_specs : NULL
#>  $ directions  : NULL
#>  $ out2        : chr "coin"
#>  $ write_to    : NULL
#>  $ write2log   : logi TRUE
```

At the moment, the normalisation is min-max onto the interval of 0 to
100. We will change this to the new function
[`n_prank()`](https://bluefoxr.github.io/COINr/reference/n_prank.md):

``` r
# change to prank function (percentile ranks)
# we don't need to specify any additional parameters (f_n_para) here
coin2$Log$Normalise$global_specs <- list(f_n = "n_prank")

# regenerate
coin2 <- Regen(coin2)
```

And that’s it. In summary, we copied the coin, edited its log to a
different normalisation methodology, and then regenerated the results.
Now what remains is to compare the results, and this is dealt with in
the next section.

Before that, let’s consider what kind of things we can change in a coin.
Anything in the Log can be changed, but of course it is up to you to
change it to something valid. As long as you carefully follow the
function help pages, this shouldn’t be any more difficult than using the
functions directly. You can also change anything else about the coin,
including the input data, by targeting the log of
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
Changing anything outside of the Log will not generally have an effect
because the coin will be recreated by
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
during regeneration and this will be overwritten. The exception is if
you use the `from` argument of
[`Regen()`](https://bluefoxr.github.io/COINr/reference/Regen.md): in
this case the regeneration will only begin from the function name that
you pass to it. This partial regeneration can also be useful to speed up
computation time.

## Adding/removing indicators

One adjustment that may be of interest is to add and remove indicators.
This needs to be done with care because removing an indicator requires
that it is removed from both `iData` and `iMeta` when building the coin
with
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
It is not possible to remove indicators after the coin is assembled,
without completely regenerating the coin.

One way to add or remove indicators is to edit the `iData` and `iMeta`
data frames by hand and then rebuild the coin. Another way is to
regenerate the coin, but use the `exclude` argument of
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).

A short cut function,
[`change_ind()`](https://bluefoxr.github.io/COINr/reference/change_ind.md)
can be also used to quickly add or remove indicators from the framework,
and regenerate the coin, all in one command.

``` r
# copy base coin
coin_remove <- coin

# remove two indicators and regenerate the coin
coin_remove <- change_ind(coin, drop = c("LPI", "Forest"), regen = TRUE)
#> coin has been regenerated using new specs.

coin_remove
#> --------------
#> A coin with...
#> --------------
#> Input:
#>   Units: 51 (AUS, AUT, BEL, ...)
#>   Indicators: 47 (Goods, Services, FDI, ...)
#>   Denominators: 4 (Area, Energy, GDP, ...)
#>   Groups: 4 (GDP_group, GDPpc_group, Pop_group, ...)
#> 
#> Structure:
#>   Level 1 Indicator: 47 indicators (FDI, ForPort, Goods, ...) 
#>   Level 2 Pillar: 8 groups (ConEcFin, Instit, P2P, ...) 
#>   Level 3 Sub-index: 2 groups (Conn, Sust) 
#>   Level 4 Index: 1 groups (Index) 
#> 
#> Data sets:
#>   Raw (51 units)
#>   Denominated (51 units)
#>   Imputed (51 units)
#>   Screened (51 units)
#>   Treated (51 units)
#>   Normalised (51 units)
#>   Aggregated (51 units)
```

The `drop` argument is used to specify which indicators to remove. The
`add` argument adds indicators, although any indicators specified by
`add` must be available in the original `iData` and `iMeta` that were
passed to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
This means that `add` can only be used if you have previously excluded
some of the indicators.

In general, if you want to test the effect of different indicators, you
should include all candidate indicators in `iData` and `iMeta` and use
`exclude` from
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
and/or
[`change_ind()`](https://bluefoxr.github.io/COINr/reference/change_ind.md)
to select subsets. The advantage of doing it this way is that different
subsets can be tested as part of a sensitivity analysis, for example.

In fact
[`change_ind()`](https://bluefoxr.github.io/COINr/reference/change_ind.md)
simply edits the `exclude` argument of
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md),
but is a quick way of doing this. Moreover it is safer, because it
performs a few checks on the indicator codes to add or remove.

It is also possible to effectively remove indicators by setting weights
to zero. This is similar to the above approach but not necessarily
identical: weights only come into play at the aggregation step, which is
usually the last operation. If you perform unit screening, or
imputation, the presence of zero-weighted indicators could still
influence the results, depending on the settings.

The effects of removing indicators and aggregates can also be tested
using the
[`remove_elements()`](https://bluefoxr.github.io/COINr/reference/remove_elements.md)
function, which removes all indicators or aggregates in a specified
level and calculates the impact.

## Comparison

Comparing coins is helped by two dedicated functions,
[`compare_coins()`](https://bluefoxr.github.io/COINr/reference/compare_coins.md)
and
[`compare_coins_multi()`](https://bluefoxr.github.io/COINr/reference/compare_coins_multi.md).
The former is for comparing two coins only, whereas the latter allows to
compare more than two coins. Let’s start by comparing the two coins we
have: the default example coin, and the same coin but with a percentile
rank normalisation method:

``` r
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

This shows that for the overall index, the maximum rank change is 10
places for Portugal. We can compare ranks or scores, for any indicator
or aggregate in the index. This also works if the number of units
changes. At the moment, the coin has an imputation step which fills in
all `NA`s. We could alternatively filter out any units with less than
90% data availability and remove the imputation step.

``` r
# copy original coin
coin90 <- coin

# remove imputation entry completely (function will not be run)
coin90$Log$Impute <- NULL

# set data availability threshold to 90%
coin90$Log$Screen$dat_thresh <- 0.9

# we also need to tell Screen() to use the denominated dset now
coin90$Log$Screen$dset <- "Denominated"

# regenerate
coin90 <- Regen(coin90)

# summarise coin
coin90
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
#>   Denominated (51 units)
#>   Screened (46 units)
#>   Treated (46 units)
#>   Normalised (46 units)
#>   Aggregated (46 units)
```

We can see that we are down to 46 units after the screening step. Now
let’s compare with the original coin:

``` r
# compare index, sort by absolute rank difference
compare_coins(coin, coin90, dset = "Aggregated", iCode = "Index",
              sort_by = "Abs.diff", decreasing = TRUE)
#>    uCode coin.1 coin.2 Diff Abs.diff
#> 40   PAK     50     44    6        6
#> 13   ESP     19     24   -5        5
#> 22   IND     45     40    5        5
#> 45   RUS     51     46    5        5
#> 50   THA     42     37    5        5
#> 8    CHN     49     45    4        4
#> 26   KAZ     47     43    4        4
#> 16   FRA     21     18    3        3
#> 33   MLT     10     13   -3        3
#> 46   SGP     14     11    3        3
#> 21   IDN     43     41    2        2
#> 32   LVA     23     21    2        2
#> 35   MNG     44     42    2        2
#> 47   SVK     24     22    2        2
#> 5    BGR     30     31   -1        1
#> 12   DNK      3      2    1        1
#> 14   EST     22     23   -1        1
#> 15   FIN     13     14   -1        1
#> 18   GRC     32     33   -1        1
#> 19   HRV     18     19   -1        1
#> 28   KOR     31     30    1        1
#> 36   MYS     39     38    1        1
#> 37   NLD      2      3   -1        1
#> 39   NZL     33     32    1        1
#> 41   PHL     38     39   -1        1
#> 42   POL     26     27   -1        1
#> 43   PRT     27     26    1        1
#> 48   SVN     11     10    1        1
#> 1    AUS     35     35    0        0
#> 2    AUT      7      7    0        0
#> 3    BEL      5      5    0        0
#> 7    CHE      1      1    0        0
#> 9    CYP     29     29    0        0
#> 10   CZE     17     17    0        0
#> 11   DEU      9      9    0        0
#> 17   GBR     15     15    0        0
#> 20   HUN     20     20    0        0
#> 23   IRL     12     12    0        0
#> 24   ITA     28     28    0        0
#> 25   JPN     34     34    0        0
#> 30   LTU     16     16    0        0
#> 31   LUX      8      8    0        0
#> 38   NOR      4      4    0        0
#> 44   ROU     25     25    0        0
#> 49   SWE      6      6    0        0
#> 51   VNM     36     36    0        0
#> 4    BGD     46     NA   NA       NA
#> 6    BRN     40     NA   NA       NA
#> 27   KHM     37     NA   NA       NA
#> 29   LAO     48     NA   NA       NA
#> 34   MMR     41     NA   NA       NA
```

The removed units are marked as `NA` in the second coin.

Finally, to demonstrate comparing multiple coins, we can call the
[`compare_coins_multi()`](https://bluefoxr.github.io/COINr/reference/compare_coins_multi.md)
function:

``` r
compare_coins_multi(list(Nominal = coin, Prank = coin2, NoLPIFor = coin_remove,
                         Screen90 = coin90), dset = "Aggregated", iCode = "Index")
#>    uCode Nominal Prank NoLPIFor Screen90
#> 7    CHE       1     1        1        1
#> 37   NLD       2     4        4        3
#> 12   DNK       3     2        2        2
#> 38   NOR       4     3        3        4
#> 3    BEL       5     7        6        5
#> 49   SWE       6     5        5        6
#> 2    AUT       7     6        7        7
#> 31   LUX       8    10        8        8
#> 11   DEU       9     8       11        9
#> 33   MLT      10    19        9       13
#> 48   SVN      11     9       10       10
#> 23   IRL      12    14       12       12
#> 15   FIN      13    13       13       14
#> 46   SGP      14    15       14       11
#> 17   GBR      15    12       16       15
#> 30   LTU      16    11       15       16
#> 10   CZE      17    18       19       17
#> 19   HRV      18    23       21       19
#> 13   ESP      19    24       20       24
#> 20   HUN      20    22       22       20
#> 16   FRA      21    21       25       18
#> 14   EST      22    16       23       23
#> 32   LVA      23    20       17       21
#> 47   SVK      24    26       24       22
#> 44   ROU      25    25       26       25
#> 42   POL      26    28       27       27
#> 43   PRT      27    17       18       26
#> 24   ITA      28    27       28       28
#> 9    CYP      29    30       29       29
#> 5    BGR      30    29       30       31
#> 28   KOR      31    33       31       30
#> 18   GRC      32    32       32       33
#> 39   NZL      33    34       33       32
#> 25   JPN      34    31       34       34
#> 1    AUS      35    35       37       35
#> 51   VNM      36    37       36       36
#> 27   KHM      37    36       35       NA
#> 41   PHL      38    40       40       39
#> 36   MYS      39    38       38       38
#> 6    BRN      40    41       45       NA
#> 34   MMR      41    42       39       NA
#> 50   THA      42    43       42       37
#> 21   IDN      43    49       41       41
#> 35   MNG      44    48       47       42
#> 22   IND      45    46       46       40
#> 4    BGD      46    44       44       NA
#> 26   KAZ      47    45       49       43
#> 29   LAO      48    39       43       NA
#> 8    CHN      49    51       50       45
#> 40   PAK      50    47       48       44
#> 45   RUS      51    50       51       46
```

This simply shows the ranks of each of the three coins side by side. We
can also choose to compare scores, and to display rank changes or
absolute rank changes. Obviously a requirement is that the coins must
all have some common units, and must all have `iCode` and `dset`
available within.
