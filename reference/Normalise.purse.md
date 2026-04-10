# Create normalised data sets in a purse of coins

This creates normalised data sets for each coin in the purse. In most
respects, this works in a similar way to normalising on a coin, for
which reason please see
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
for most documentation. There is however a special case in terms of
operating on a purse of coins. This is because, when dealing with time
series data, it is often desirable to normalise over the whole panel
data set at once rather than independently for each time point. This
makes the resulting index and aggregates comparable over time. Here, the
`global` argument controls whether to normalise each coin independently
or to normalise across all data at once. In other respects, this
function behaves the same as
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md).

## Usage

``` r
# S3 method for class 'purse'
Normalise(
  x,
  dset,
  global_specs = NULL,
  indiv_specs = NULL,
  directions = NULL,
  global = TRUE,
  write_to = NULL,
  ...
)
```

## Arguments

- x:

  A purse object

- dset:

  The data set to normalise in each coin

- global_specs:

  Default specifications

- indiv_specs:

  Individual specifications

- directions:

  An optional data frame containing the following columns:

  - `iCode` The indicator code, corresponding to the column names of the
    data set

  - `Direction` numeric vector with entries either `-1` or `1` If
    `directions` is not specified, the directions will be taken from the
    `iMeta` table in the coin, if available.

- global:

  Logical: if `TRUE`, normalisation is performed "globally" across all
  coins, by using e.g. the max and min of each indicator in any coin.
  This effectively makes normalised scores comparable between coins
  because they are all scaled using the same parameters. Otherwise if
  `FALSE`, coins are normalised individually.

- write_to:

  Optional character string for naming the data set in each coin. Data
  will be written to `.$Data[[write_to]]`. Default is
  `write_to == "Normalised"`.

- ...:

  arguments passed to or from other methods.

## Value

An updated purse with new normalised data sets added at
`.$Data$Normalised` in each coin

## Details

The same specifications are passed to each coin in the purse. This means
that each coin is normalised using the same set of specifications and
directions. If you need control over individual coins, you will have to
normalise coins individually.

## Examples

``` r
# build example purse
purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)

# normalise raw data set
purse <- Normalise(purse, dset = "Raw", global = TRUE)
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Normalised
```
