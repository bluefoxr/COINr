# Building coins

``` r
library(COINr)
```

This vignette gives a guide to building “coins”, which are the object
class representing a composite indicator used throughout COINr, and
“purses”, which are time-indexed collections of coins.

## What is a coin?

COINr functions are designed to work in particular on an S3 object class
called a “coin”. To introduce this, consider what constitutes a
composite indicator:

- The indicator data
- Indicator metadata, including weights and directions
- A structure which maps indicators into groups for aggregation,
  typically over multiple levels
- Methodological specifications, including
  - Data treatment
  - Normalisation method and parameters
  - Aggregation method and parameters
- Processed data sets at each stage of the construction
- Resulting aggregated scores and ranks

Meanwhile, in the process of building a composite indicator, a series of
analysis data is generated, including information on data availability,
statistics on individual indicators, correlations and information about
data treatment.

If a composite indicator is built from scratch, it is easy to generate
an environment with dozens of variables and parameters. In case an
alternative version of the composite indicator is built, multiple sets
of variables may need to be generated. With this in mind, it makes sense
to structure all the ingredients of composite indicator, from input
data, to methodology and results, into a single object, which is called
a “coin” in COINr.

How to construct a coin, and some details of its contents, will be
explained in more detail in the following sections. Although coins are
the main object class used in COINr, a number of COINr functions also
have methods for data frames and vectors. This is explained in other
vignettes.

## Building coins

To build a coin you need to use the
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
function. The main two input arguments of this function are two data
frames: `iData` (the indicator data), and `iMeta` (the indicator
metadata). This builds a coin class object containing the raw data,
which can then be developed and expanded by COINr functions by
e.g. normalising, treating data, imputing, aggregating and so on.

Before proceeding, we have to define a couple of things. The “things”
that are being benchmarked/compared by the indicators and composite
indicator are more generally referred to as *units* (quite often, units
correspond to countries). Units are compared using *indicators*, which
are measured variables that are relevant to the overall concept of the
composite indicator.

### Indicator data

The first data frame, `iData` specifies the value of each indicator, for
each unit. It can also contain further attributes and metadata of units,
for example groups, names, and denominating variables (variables which
are used to adjust for size effects of indicators).

To see an example of what `iData` looks like, we can look at the built
in
[ASEM](https://composite-indicators.jrc.ec.europa.eu/asem-sustainable-connectivity/)
data set. This data set is from a composite indicator covering 51
countries with 49 indicators, and is used for examples throughout COINr:

``` r
head(ASEM_iData[1:20], 5)
#>      uName uCode GDP_group GDPpc_group Pop_group EurAsia_group Time   Area
#> 1  Austria   AUT         L          XL         M        Europe 2018  83871
#> 2  Belgium   BEL         L           L         L        Europe 2018  30528
#> 3 Bulgaria   BGR         S           S         M        Europe 2018 110879
#> 4  Croatia   HRV         S           M         S        Europe 2018  56594
#> 5   Cyprus   CYP         S           L         S        Europe 2018   9251
#>   Energy       GDP Population      LPI  Flights      Ship Bord       Elec
#> 1  27.00 390.79999   8735.453 4.097985 29.01725  0.000000   35 35.3697298
#> 2  41.83 467.95527  11429.336 4.108538 31.88546 20.567121   48 26.5330467
#> 3   9.96  53.23964   7084.571 2.807685  9.23588  7.919366   18 11.2775842
#> 4   7.01  51.23100   4189.353 3.160829  9.24529 12.440452   41 19.5283620
#> 5   1.43  20.04623   1179.551 2.999061  8.75467 11.689495    0  0.4393643
#>      Gas ConSpeed Cov4G     Goods
#> 1  0.273     14.1 98.00 278.42640
#> 2 36.100     16.3 99.89 597.87230
#> 3  0.312     15.5 56.73  42.82515
#> 4  0.422      8.6 98.00  28.36795
#> 5  0.029      6.9 60.00   8.76681
```

Here only a few rows and columns are shown to illustrate. The ASEM data
covers covering 51 Asian and European countries, at the national level,
and uses 49 indicators. Notice that each row is an observation (here, a
country), and each column is a variable (mostly indicators, but also
other things).

Columns can be named whatever you want, although a few names are
reserved:

- `uName` \[optional\] gives the name of each unit. Here, units are
  countries, so these are the names of each country.
- `uCode` \[**required**\] is a unique code assigned to each unit
  (country). This is the main “reference” inside COINr for units. If the
  units are countries, ISO Alpha-3 codes should ideally be used, because
  these are recognised by COINr for generating maps.
- `Time` \[optional\] gives the reference time of the data. This is used
  if panel data is passed to
  [`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
  See [Purses and panel data](#purses-and-panel-data).

This means that at a minimum, you need to supply a data frame with a
`uCode` column, and some indicator columns.

Aside from the reserved names above, columns can be assigned to
different uses using the corresponding `iMeta` data frame - this is
clarified in the next section.

Some important rules and tips to keep in mind are:

- Columns don’t have to be in any particular order; they are identified
  by names rather than positions.
- Indicator columns are required to be numeric, i.e. they cannot be
  character vectors.
- There is no restriction on the number of indicators and units.
- Indicator codes and unit codes must have unique names.
- As with everything in R, all codes are case-sensitive.
- Don’t start any column names with a number!

The `iData` data frame will be checked when it is passed to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
You can also perform this check yourself in advance by calling
[`check_iData()`](https://bluefoxr.github.io/COINr/reference/check_iData.md):

``` r
check_iData(ASEM_iData)
#> iData checked and OK.
```

If there are issues with your `iData` data frame this should produce
informative error messages which can help to correct the problem.

### Indicator metadata

The `iMeta` data frame specifies everything about each column in
`iData`, including whether it is an indicator, a group, or something
else; its name, its units, and where it appears in the *structure* of
the index. `iMeta` also requires entries for any aggregates which will
be created by aggregating indicators. Let’s look at the built-in
example.

``` r
head(ASEM_iMeta, 5)
#>   Level   iCode                                    iName Direction Weight
#> 1     1     LPI              Logistics Performance Index         1      1
#> 2     1 Flights International flights passenger capacity         1      1
#> 3     1    Ship        Liner Shipping Connectivity Index         1      1
#> 4     1    Bord                         Border crossings         1      1
#> 5     1    Elec                     Trade in electricity         1      1
#>                  Unit     Target Denominator   Parent      Type
#> 1           Score 1-5   4.118031        <NA> Physical Indicator
#> 2      Thousand seats 200.332655  Population Physical Indicator
#> 3               Score  20.113377        <NA> Physical Indicator
#> 4 Number of crossings 115.900000        Area Physical Indicator
#> 5                 TWh 104.670585      Energy Physical Indicator
```

Required columns for `iMeta` are:

- `Level`: The level in aggregation, where 1 is indicator level, 2 is
  the level resulting from aggregating indicators, 3 is the result of
  aggregating level 2, and so on. Set to `NA` for entries that are not
  included in the index (groups, denominators, etc).
- `iCode`: Indicator code, alphanumeric. Must not start with a number.
  These entries generally correspond to the column names of `iData`.
- `Parent`: Group (`iCode`) to which indicator/aggregate belongs in
  level immediately above. Each entry here should also be found in
  `iCode`. Set to `NA` only for the highest (Index) level (no parent),
  or for entries that are not included in the index (groups,
  denominators, etc).
- `Direction`: Numeric, either -1 or 1
- `Weight`: Numeric weight, will be re-scaled to sum to 1 within
  aggregation group. Set to `NA` for entries that are not included in
  the index (groups, denominators, etc).
- `Type`: The type, corresponding to `iCode`. Can be either `Indicator`,
  `Aggregate`, `Group`, `Denominator`, or `Other`.

Optional columns that are recognised in certain functions are:

- `iName`: Name of the indicator: a longer name which is used in some
  plotting functions.
- `Denominator`: specifies which denominator variable should be used to
  denominate the indicator, if
  [`Denominate()`](https://bluefoxr.github.io/COINr/reference/Denominate.md)
  is called. See the
  [Denomination](https://bluefoxr.github.io/COINr/articles/denomination.md)
  vignette.
- `Unit`: the unit of the indicator, e.g. USD, thousands, score, etc.
  Used in some plots if available.
- `Target`: a target for the indicator. Used if normalisation type is
  distance-to-target.

`iMeta` can also include other columns if needed for specific uses, as
long as they don’t use the names listed above.

The `iMeta` data frame essentially gives details about each of the
columns found in `iData`, as well as details about additional data
columns eventually created by aggregating indicators. This means that
the entries in `iMeta` must include *all* columns in `iData`, *except*
the three “special” column names: `uCode`, `uName`, and `Time`. In other
words, all column names of `iData` should appear in `iMeta$iCode`,
except the three special cases mentioned.

The `Type` column specifies the type of the entry: `Indicator` should be
used for indicators at level 1. `Aggregate` for aggregates created by
aggregating indicators or other aggregates. Otherwise set to `Group` if
the variable is not used for building the index but instead is for
defining groups of units. Set to `Denominator` if the variable is to be
used for scaling (denominating) other indicators. Finally, set to
`Other` if the variable should be ignored but passed through. Any other
entries here will cause an error.

Apart from the indicator entries shown above, we can see aggregate
entries:

``` r
ASEM_iMeta[ASEM_iMeta$Type == "Aggregate", ]
#>    Level     iCode                        iName Direction Weight  Unit Target
#> 50     2  Physical                     Physical         1      1 Score     NA
#> 51     2  ConEcFin Economic and Financial (Con)         1      1 Score     NA
#> 52     2 Political                    Political         1      1 Score     NA
#> 53     2    Instit                Institutional         1      1 Score     NA
#> 54     2       P2P             People to People         1      1 Score     NA
#> 55     2   Environ                Environmental         1      1 Score     NA
#> 56     2    Social                       Social         1      1 Score     NA
#> 57     2  SusEcFin Economic and Financial (Sus)         1      1 Score     NA
#> 58     3      Conn                 Connectivity         1      1 Score     NA
#> 59     3      Sust               Sustainability         1      1 Score     NA
#> 60     4     Index     Sustainable Connectivity         1      1 Score     NA
#>    Denominator Parent      Type
#> 50        <NA>   Conn Aggregate
#> 51        <NA>   Conn Aggregate
#> 52        <NA>   Conn Aggregate
#> 53        <NA>   Conn Aggregate
#> 54        <NA>   Conn Aggregate
#> 55        <NA>   Sust Aggregate
#> 56        <NA>   Sust Aggregate
#> 57        <NA>   Sust Aggregate
#> 58        <NA>  Index Aggregate
#> 59        <NA>  Index Aggregate
#> 60        <NA>   <NA> Aggregate
```

These are the aggregates that will be created by aggregating indicators.
These values will only be created when we call the
[`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md)
function (see relevant vignette). We also have groups:

``` r
ASEM_iMeta[ASEM_iMeta$Type == "Group", ]
#>    Level         iCode                iName Direction Weight Unit Target
#> 61    NA     GDP_group            GDP group        NA     NA <NA>     NA
#> 62    NA   GDPpc_group GDP per capita group        NA     NA <NA>     NA
#> 63    NA     Pop_group     Population group        NA     NA <NA>     NA
#> 64    NA EurAsia_group       Europe or Asia        NA     NA <NA>     NA
#>    Denominator Parent  Type
#> 61        <NA>   <NA> Group
#> 62        <NA>   <NA> Group
#> 63        <NA>   <NA> Group
#> 64        <NA>   <NA> Group
```

Notice that the `iCode` entries here correspond to column names of
`iData`. There are also denominators:

``` r
ASEM_iMeta[ASEM_iMeta$Type == "Denominator", ]
#>    Level      iCode              iName Direction Weight               Unit
#> 65    NA       Area          Land area        NA     NA Thousand square km
#> 66    NA     Energy Energy consumption        NA     NA               Unit
#> 67    NA        GDP                GDP        NA     NA             USD Bn
#> 68    NA Population         Population        NA     NA          Thousands
#>    Target Denominator Parent        Type
#> 65     NA        <NA>   <NA> Denominator
#> 66     NA        <NA>   <NA> Denominator
#> 67     NA        <NA>   <NA> Denominator
#> 68     NA        <NA>   <NA> Denominator
```

Denominators are used to divide or “scale” other indicators. They are
ideally included in `iData` because this ensures that they match the
units and possibly the time points.

The `Direction` column specifies the directionality of each indicator.
Indicators with `1` behave such that higher values of these indicators
will imply higher index values, while indicators with `-1` are
associated in the opposite direction. For example, in a quality of life
index, a “Median Income” indicator would have direction `1`, while
“Crime rate” would have direction `-1`. Note that directions are applied
in COINr during the normalisation step only, and *not* during
aggregation. This means that any directions assigned to aggregates above
level 1 have no effect (i.e. aggregates are all considered to contribute
positively).

The `Parent` column requires a few extra words. This is used to define
the structure of the index. Simply put, it specifies the aggregation
group to which the indicator or aggregate belongs to, in the level
immediately above. For indicators in level 1, this should refer to
`iCode`s in level 2, and for aggregates in level 2, it should refer to
`iCode`s in level 3. Every entry in `Parent` must refer to an entry that
can be found in the `iCode` column, or else be `NA` for the highest
aggregation level or for groups, denominators and other `iData` columns
that are not included in the index.

The `iMeta` data frame is more complex that `iData` and it may be easy
to make errors. Use the
[`check_iMeta()`](https://bluefoxr.github.io/COINr/reference/check_iMeta.md)
function (which is anyway called by
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md))
to check the validity of your `iMeta`. Informative error messages are
included where possible to help correct any errors.

``` r
check_iMeta(ASEM_iMeta)
#> iMeta checked and OK.
```

When
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
is run, additional cross-checks are run between `iData` and `iMeta`.

### Building with `new_coin()`

With the `iData` and `iMeta` data frames prepared, you can build a coin
using the
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
function. This has some other arguments and options that we will see in
a minute, but by default it looks like this:

``` r
# build a new coin using example data
coin <- new_coin(iData = ASEM_iData,
                 iMeta = ASEM_iMeta,
                 level_names = c("Indicator", "Pillar", "Sub-index", "Index"))
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
```

The
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
function checks and cross-checks both input data frames, and outputs a
coin-class object. It also tells us that it has written a data set to
`.$Data$Raw` - this is the sub-list that contains the various data sets
that will be created each time we run a coin-building function.

We can see a summary of the coin by calling the coin print method - this
is done simply by calling the name of the coin at the command line, or
equivalently `print(coin)`:

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
```

This tells us some details about the coin - the number of units,
indicators, denominators and groups; the structure of the index (notice
that the `level_names` argument is used to describe each level), and the
data sets present in the coin. Currently this only consists of the “Raw”
data set, which is the data set that is created by default when we run
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md),
and simply consists of the indicator data plus the `uCode` column.
Indeed, we can retrieve any data set from within a coin at any time
using the
[`get_dset()`](https://bluefoxr.github.io/COINr/reference/get_dset.md)
function:

``` r
# first few cols and rows of Raw data set
data_raw <- get_dset(coin, "Raw")
head(data_raw[1:5], 5)
#>    uCode      LPI  Flights      Ship Bord
#> 31   AUS 3.793385 36.05498 14.004198    0
#> 1    AUT 4.097985 29.01725  0.000000   35
#> 2    BEL 4.108538 31.88546 20.567121   48
#> 32   BGD 2.663902  4.27955  9.698165   16
#> 3    BGR 2.807685  9.23588  7.919366   18
```

By default, calling
[`get_dset()`](https://bluefoxr.github.io/COINr/reference/get_dset.md)
returns only the unit code plus the indicator/aggregate columns. We can
also attach other columns such as groups and names by using the
`also_get` argument. This can be used to attach any of the `iData`
“metadata” columns that were originally passed when calling
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md),
such as groups, etc.

``` r
get_dset(coin, "Raw", also_get = c("uName", "Pop_group"))[1:5] |>
  head(5)
#>   uCode      uName Pop_group      LPI  Flights
#> 1   AUS  Australia         L 3.793385 36.05498
#> 2   AUT    Austria         M 4.097985 29.01725
#> 3   BEL    Belgium         L 4.108538 31.88546
#> 4   BGD Bangladesh        XL 2.663902  4.27955
#> 5   BGR   Bulgaria         M 2.807685  9.23588
```

Apart from the `level_names` argument,
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
also gives the possibility to only pass forward a subset of the
indicators in `iMeta`. This is done using the `exclude` argument, which
is useful when testing alternative sets of indicators - see vignette on
adjustments and comparisons.

``` r
# exclude two indicators
coin <- new_coin(iData = ASEM_iData,
                 iMeta = ASEM_iMeta,
                 level_names = c("Indicator", "Pillar", "Sub-index", "Index"),
                 exclude = c("LPI", "Flights"))
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw

coin
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
```

Here,
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
has removed the indicator columns from `iData` and the corresponding
entries in `iMeta`. However, the full original `iData` and `iMeta`
tables are still stored in the coin.

The
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
function includes a thorough series of checks on its input arguments
which may cause some initial errors while the format is corrected. The
objective is that if you can successfully assemble a coin, this should
work smoothly for all COINr functions.

## Example coin

COINr includes a built in example coin which is constructed using a
function
[`build_example_coin()`](https://bluefoxr.github.io/COINr/reference/build_example_coin.md).
This can be useful for learning how the package works, testing and is
used in COINr documentation extensively because many functions require a
coin as an input. Here we build the example coin (which is again from
the ASEM data set built into COINr) and inspect its contents:

``` r
ASEM <- build_example_coin(quietly = TRUE)

ASEM
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

This shows that the example is a fully populated coin with various data
sets, each resulting from running COINr functions, up to the aggregation
step.

## Purses and panel data

A coin offers a very wide methodological flexibility, but some things
are kept fixed throughout. One is that the set of indicators does not
change once the coin has been created. The other thing is that each coin
represents a single point in time.

If you have panel data, i.e. multiple observations for each
unit-indicator pair, indexed by time, then
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
allows you to create multiple coins in one go. Coins are collected into
a single object called a “*purse*”, and many COINr functions work on
purses directly.

Here we simply explore how to create a purse. The procedure is almost
the same as creating a coin: you need the `iData` and `iMeta` data
frames, and you call
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
The difference is that `iData` must now have a `Time` column, which must
be a numeric column which records which time point each observation is
from. To see an example, we can look at the built-in (artificial) panel
data set `ASEM_iData_p`.

``` r
# sample of 2018 observations
ASEM_iData_p[ASEM_iData_p$Time == 2018, 1:15] |>
  head(5)
#>      uName uCode GDP_group GDPpc_group Pop_group EurAsia_group Time   Area
#> 1  Austria   AUT         L          XL         M        Europe 2018  83871
#> 2  Belgium   BEL         L           L         L        Europe 2018  30528
#> 3 Bulgaria   BGR         S           S         M        Europe 2018 110879
#> 4  Croatia   HRV         S           M         S        Europe 2018  56594
#> 5   Cyprus   CYP         S           L         S        Europe 2018   9251
#>   Energy       GDP Population      LPI  Flights      Ship Bord
#> 1  27.00 390.79999   8735.453 4.097985 29.01725  0.000000   35
#> 2  41.83 467.95527  11429.336 4.108538 31.88546 20.567121   48
#> 3   9.96  53.23964   7084.571 2.807685  9.23588  7.919366   18
#> 4   7.01  51.23100   4189.353 3.160829  9.24529 12.440452   41
#> 5   1.43  20.04623   1179.551 2.999061  8.75467 11.689495    0

# sample of 2019 observations
ASEM_iData_p[ASEM_iData_p$Time == 2019, 1:15] |>
  head(5)
#>       uName uCode GDP_group GDPpc_group Pop_group EurAsia_group Time   Area
#> 52  Austria   AUT         L          XL         M        Europe 2019  83871
#> 53  Belgium   BEL         L           L         L        Europe 2019  30528
#> 54 Bulgaria   BGR         S           S         M        Europe 2019 110879
#> 55  Croatia   HRV         S           M         S        Europe 2019  56594
#> 56   Cyprus   CYP         S           L         S        Europe 2019   9251
#>    Energy       GDP Population      LPI  Flights       Ship      Bord
#> 52  27.00 390.79999   8735.453 4.153182 37.53763  0.6054851 39.752508
#> 53  41.83 467.95527  11429.336 4.149371 41.53901 21.2045607 52.123937
#> 54   9.96  53.23964   7084.571 2.868647 15.82871  7.9467542 23.203648
#> 55   7.01  51.23100   4189.353 3.230168 16.06586 13.0958316 46.566308
#> 56   1.43  20.04623   1179.551 3.098577 10.92502 12.3571194  3.993825
```

This data set has five years of data, spanning 2018-2022 (the data are
artificially generated - at some point I will replace this with a real
example). This means that each row now corresponds to a set of indicator
values for a unit, for a given time point.

To build a purse from this data, we input it into
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)

``` r
# build purse from panel data
purse <- new_coin(iData = ASEM_iData_p,
                  iMeta = ASEM_iMeta,
                  split_to = "all",
                  quietly = TRUE)
```

Notice here that the `iMeta` argument is the same as when we assembled a
single coin - this is because a purse is supposed to consist of coins
with the same indicators and structure, i.e. the aim is to calculate a
composite indicator over several points in time, and generally to apply
the same methodology to all coins in the purse. It is however possible
to have different units between coins in the same purse - this might
occur because of data availability differences at different time points.

The `split_to` argument should be set to `"all"` to create a coin from
each time point found in the data. Alternatively, you can only include a
subset of time points by specifying them as a vector.

A quick way to check the contents of the purse is to call its print
method:

``` r
purse
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
```

This tells us how many coins there are, the number of indicators and
units, and gives some structural information from one of the coins.

A purse is an S3 class object like a coin. In fact, it is simply a data
frame with a `Time` column and a `coin` column, where entries in the
`coin` column are coin objects (in a so-called “list column”). This is
convenient to work with, but if you try to view it in R Studio, for
example, it can be a little messy.

As with coins, the purse class also has a function in COINr which
produces an example purse:

``` r
ASEM_purse <- build_example_purse(quietly = TRUE)

ASEM_purse
#> -----------------------------
#> A purse with... 5 coins 
#> -----------------------------
#> 
#>  Time n_Units n_Inds n_dsets
#>  2018      51     49       5
#>  2019      51     49       5
#>  2020      51     49       5
#>  2021      51     49       5
#>  2022      51     49       5
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
#>   Level 1 Indicator: 49 indicators (FDI, ForPort, Goods, ...) 
#>   Level 2 Pillar: 8 groups (ConEcFin, Instit, P2P, ...) 
#>   Level 3 Sub-index: 2 groups (Conn, Sust) 
#>   Level 4 Index: 1 groups (Index) 
#> 
#> Data sets:
#>   Raw (51 units)
#>   Screened (46 units)
#>   Treated (46 units)
#>   Normalised (46 units)
#>   Aggregated (46 units)
```

The purse class can be used directly with COINr functions - this allows
to impute/normalise/treat/aggregate all coins with a single command, for
example.

## Summary

COINr is mostly designed to work with coins and purses. However, many
key functions also have methods for data frames or vectors. This means
that COINr can either be used as an “ecosystem” of functions built
around coins and purses, or else can just be used as a toolbox for doing
your own work with data frames and other objects.
