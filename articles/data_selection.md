# Data selection

This vignette describes how to retrieve data from a coin. The main
functions to do this are
[`get_dset()`](https://bluefoxr.github.io/COINr/reference/get_dset.md)
and the more flexible
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).

These functions are important to understand, because many COINr
functions use them to retrieve data for plotting, analysis and other
functions. Both functions are *generics*, which means that they have
methods for coins and purses.

## Data sets

Every time a “building” operation is applied to a coin, such as
[`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md),
[`Screen()`](https://bluefoxr.github.io/COINr/reference/Screen.md),
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
and so on, a new data set is created. Data sets live in the `.$Data`
sub-list of the coin. We can retrieve a data set at any time using the
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
function:

``` r
library(COINr)

# build full example coin
coin <- build_example_coin(quietly = TRUE)

# retrieve normalised data set
dset_norm <- get_dset(coin, dset = "Normalised")

# view first few rows and cols
head(dset_norm[1:5], 5)
#>   uCode      LPI    Flights     Ship      Bord
#> 1   AUS 79.96112 12.3223217 66.14497   0.00000
#> 2   AUT 94.07137 27.8763185  0.00000  42.01269
#> 3   BEL 94.56023 23.3967426 97.14314 100.00000
#> 4   BGD 27.63906  0.1243185 45.80661  10.85013
#> 5   BGR 34.29965 10.8828790 37.40495  16.34359
```

By default, a data set in the coin consists of indicator columns plus
the “uCode” column, which is the unique identifier of each row. You can
also ask to attach unit metadata columns, such as unit names, groups,
and anything else that was input when building the coin, using the
`also_get` argument:

``` r
# retrieve normalised data set
dset_norm2 <- get_dset(coin, dset = "Normalised", also_get = c("uName", "GDP_group"))

# view first few rows and cols
head(dset_norm2[1:5], 5)
#>   uCode      uName GDP_group      LPI    Flights
#> 1   AUS  Australia        XL 79.96112 12.3223217
#> 2   AUT    Austria         L 94.07137 27.8763185
#> 3   BEL    Belgium         L 94.56023 23.3967426
#> 4   BGD Bangladesh         M 27.63906  0.1243185
#> 5   BGR   Bulgaria         S 34.29965 10.8828790
```

## Data subsets

While
[`get_dset()`](https://bluefoxr.github.io/COINr/reference/get_dset.md)
is a quick way to retrieve an entire data set and metadata, the
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
function is a generalisation: it can also be used to obtain a whole data
set, but also subsets of data, based on e.g. indicator selection and
grouping (columns), as well as unit selection and grouping (rows).

### Indicators/columns

A simple example is to extract one or more named indicators from a
target data set:

``` r
x <- get_data(coin, dset = "Raw", iCodes = c("Flights", "LPI"))

# see first few rows
head(x, 5)
#>    uCode  Flights      LPI
#> 31   AUS 36.05498 3.793385
#> 1    AUT 29.01725 4.097985
#> 2    BEL 31.88546 4.108538
#> 32   BGD  4.27955 2.663902
#> 3    BGR  9.23588 2.807685
```

By default,
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
returns the requested indicators, plus the `uCode` identifier column. We
can also set `also_get = "none"` to return only the indicator columns.

The `iCode` argument can also accept groups of indicators, based on the
structure of the index. In our example, indicators are aggregated into
“pillars” (level 2) within groups. We can name an aggregation group and
extract the underlying indicators:

``` r
x <- get_data(coin, dset = "Raw", iCodes = "Political", Level = 1)
head(x, 5)
#>    uCode Embs IGOs   UNVote
#> 31   AUS   82  196 38.46245
#> 1    AUT   88  227 42.63920
#> 2    BEL   84  248 43.00308
#> 32   BGD   52  145 38.60601
#> 3    BGR   67  209 42.95986
```

Here we have requested all the indicators in level 1 (the indicator
level), that belong to the group called “Political” (one of the
pillars). Specifying the level becomes more relevant when we look at the
aggregated data set, which also includes the pillar, sub-index and index
scores. Here, for example, we can ask for all the pillar scores (level
2) which belong to the sustainability sub-index (level 3):

``` r
x <- get_data(coin, dset = "Aggregated", iCodes = "Sust", Level = 2)

head(x, 5)
#>   uCode  Environ   Social SusEcFin
#> 1   AUS 31.92211 71.88108 55.69987
#> 2   AUT 69.47511 72.76415 62.88150
#> 3   BEL 53.00859 86.16783 50.09020
#> 4   BGD 81.66988 27.51138 64.58884
#> 5   BGR 55.69922 53.30489 61.68677
```

If this isn’t clear, look at the structure of the example index using
e.g. `plot_framework(coin)`. If we wanted to select all the indicators
within the “Sust” sub-index we would set `Level = 1`. If we wanted to
select the sub-index scores themselves we would set `Level = 3`, and so
on.

The idea of selecting indicators and aggregates based on the structure
of the index is useful in many places in COINr, for example examining
correlations within aggregation groups using
[`plot_corr()`](https://bluefoxr.github.io/COINr/reference/plot_corr.md).

### Units/rows

Units (rows) of the data set can also be selected (also in combination
with selecting indicators). Starting with a simple example, let’s select
specified units for a specific indicator:

``` r
get_data(coin, dset = "Raw", iCodes = "Goods", uCodes = c("AUT", "VNM"))
#>    uCode    Goods
#> 1    AUT 278.4264
#> 51   VNM 269.0766
```

Rows can also be sub-setted using groups, i.e. unit groupings that are
defined using variables input with `iMeta$Type = "Group"` when building
the coin. Recall that for our example coin we have several groups (a
reminder that you can see some details about the coin using its print
method):

``` r
coin
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
#>   Imputed (51 units)
#>   Screened (51 units)
#>   Treated (51 units)
#>   Normalised (51 units)
#>   Aggregated (51 units)
```

The first way to subset by unit group is to name a grouping variable,
and a group within that variable to select. For example, say we want to
know the values of the “Goods” indicator for all the countries in the
“XL” GDP group:

``` r
get_data(coin, dset = "Raw", iCodes = "Goods", use_group = list(GDP_group = "XL"))
#>    uCode GDP_group     Goods
#> 1    AUS        XL  288.4893
#> 8    CHN        XL 1713.6190
#> 11   DEU        XL 1919.1940
#> 13   ESP        XL  447.1229
#> 16   FRA        XL  849.3303
#> 17   GBR        XL  778.9052
#> 21   IDN        XL  222.4186
#> 22   IND        XL  288.9806
#> 24   ITA        XL  658.1981
#> 25   JPN        XL  732.2078
#> 28   KOR        XL  568.9920
#> 45   RUS        XL  343.8504
```

Since we have subsetted by group, this also returns the group column
which was used.

Another way of sub-setting is to combine `uCodes` and `use_group`. When
these two arguments are both specified, the result is to return the full
group(s) to which the specified `uCodes` belong. This can be used to put
a unit in context with its peers within a group. For example, we might
want to see the values of the “Flights” indicator for a specific unit,
as well as all other units within the same population group:

``` r
get_data(coin, dset = "Raw", iCodes = "Flights", uCodes = "MLT", use_group = "Pop_group")
#>    uCode Pop_group  Flights
#> 6    BRN         S  2.01900
#> 9    CYP         S  8.75467
#> 14   EST         S  3.12946
#> 19   HRV         S  9.24529
#> 23   IRL         S 34.17721
#> 30   LTU         S  5.37919
#> 31   LUX         S  4.84458
#> 32   LVA         S  6.77976
#> 33   MLT         S  6.75251
#> 35   MNG         S  0.98951
#> 38   NOR         S 25.64994
#> 39   NZL         S 13.37242
#> 48   SVN         S  1.51736
```

Here, we have to specify `use_group` simply as a string rather than a
list. Since MLT is in the “S” population group, it returns all units
within that group.

Overall, the idea of
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
is to flexibly return subsets of indicator data, based on the structure
of the index and unit groups.

## Manual selection

As a final point, it’s worth pointing out that a coin is simply a list
of R objects such as data frames, other lists, vectors and so on. It has
a particular format which allows things to be easily accessed by COINr
functions. But other than that, its an ordinary R object. This means
that even without the helper functions mentioned, you can get at the
data simply by exploring the coin yourself.

The data sets live in the `.$Data` sub-list of the coin:

``` r
names(coin$Data)
#> [1] "Raw"         "Denominated" "Imputed"     "Screened"    "Treated"    
#> [6] "Normalised"  "Aggregated"
```

And we can access any of these directly:

``` r
data_raw <- coin$Data$Raw

head(data_raw[1:5], 5)
#>    uCode      LPI  Flights      Ship Bord
#> 31   AUS 3.793385 36.05498 14.004198    0
#> 1    AUT 4.097985 29.01725  0.000000   35
#> 2    BEL 4.108538 31.88546 20.567121   48
#> 32   BGD 2.663902  4.27955  9.698165   16
#> 3    BGR 2.807685  9.23588  7.919366   18
```

The metadata lives in the `.$Meta` sub-list. For example, the unit
metadata, which includes groups, names etc:

``` r
str(coin$Meta$Unit)
#> 'data.frame':    51 obs. of  11 variables:
#>  $ uCode        : chr  "AUS" "AUT" "BEL" "BGD" ...
#>  $ uName        : chr  "Australia" "Austria" "Belgium" "Bangladesh" ...
#>  $ GDP_group    : chr  "XL" "L" "L" "M" ...
#>  $ GDPpc_group  : chr  "XL" "XL" "L" "S" ...
#>  $ Pop_group    : chr  "L" "M" "L" "XL" ...
#>  $ EurAsia_group: chr  "Asia" "Europe" "Europe" "Asia" ...
#>  $ Time         : num  2018 2018 2018 2018 2018 ...
#>  $ Area         : num  7741220 83871 30528 148460 110879 ...
#>  $ Energy       : num  81.3 27 41.83 27.92 9.96 ...
#>  $ GDP          : num  1304.5 390.8 468 220.8 53.2 ...
#>  $ Population   : num  24451 8735 11429 164670 7085 ...
```

The point is that if COINr tools don’t get you where you want to go,
knowing your way around the coin allows you to access the data exactly
how you want.
