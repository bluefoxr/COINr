# Cronbach's alpha

Calculates Cronbach's alpha, a measure of statistical reliability.
Cronbach's alpha is a simple measure of "consistency" of a data set,
where a high value implies higher reliability/consistency. The selection
of indicators via
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
allows to calculate the measure on any group of indicators or
aggregates.

## Usage

``` r
get_cronbach(coin, dset, iCodes, Level, ..., use = "pairwise.complete.obs")
```

## Arguments

- coin:

  A coin or a data frame containing only numerical columns of data.

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- iCodes:

  Indicator codes to retrieve. If `NULL` (default), returns all iCodes
  found in the selected data set. See
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).

- Level:

  The level in the hierarchy to extract data from. See
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).

- ...:

  Further arguments passed to
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md),
  other than those explicitly specified here.

- use:

  Argument to pass to [stats::cor](https://rdrr.io/r/stats/cor.html) to
  calculate the covariance matrix. Default `"pairwise.complete.obs"`.

## Value

Cronbach alpha as a numerical value.

## Details

This function simply returns Cronbach's alpha. If you want a lot more
details on reliability, the 'psych' package has a much more detailed
analysis.

This function replaces the now-defunct `getCronbach()` from COINr \<
v1.0.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# Cronbach's alpha for the "P2P" group
get_cronbach(coin, dset = "Raw", iCodes = "P2P", Level = 1)
#> [1] 0.05126659
```
