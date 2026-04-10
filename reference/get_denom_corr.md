# Correlations between indicators and denominators

Get a data frame containing any correlations between indicators and
denominators that exceed a given threshold. This can be useful when
*whether* to denominate an indicator and *by what* may not be obvious.
If an indicator is strongly correlated with a denominator, this may
suggest to denominate it by that denominator.

## Usage

``` r
get_denom_corr(
  coin,
  dset,
  cor_thresh = 0.6,
  cortype = "pearson",
  nround = 2,
  use_directions = FALSE
)
```

## Arguments

- coin:

  A coin class object.

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- cor_thresh:

  A correlation threshold: the absolute value of any correlations
  between indicator-denominator pairs above this threshold will be
  flagged.

- cortype:

  The type of correlation: to be passed to the `method` argument of
  [`stats::cor`](https://rdrr.io/r/stats/cor.html).

- nround:

  Optional number of decimal places to round correlation values to.
  Default 2, set `NULL` to disable.

- use_directions:

  Logical: if `TRUE` the extracted data is adjusted using directions
  found inside the coin (i.e. the "Direction" column input in `iMeta`.
  See comments on this argument in
  [`get_corr()`](https://bluefoxr.github.io/COINr/reference/get_corr.md).

## Value

A data frame of pairwise correlations that exceed the threshold.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# get correlations >0.7 of any indicator with denominators
get_denom_corr(coin, dset = "Raw", cor_thresh = 0.7)
#>          Ind  Denom Corr
#> 70  CultGood Energy 0.71
#> 119 CultGood    GDP 0.85
#> 52       FDI Energy 0.74
#> 101      FDI    GDP 0.85
#> 99     Goods    GDP 0.80
#> 102   PRemit    GDP 0.71
#> 116 Research    GDP 0.78
#> 100 Services    GDP 0.77
#> 66     StMob Energy 0.73
#> 115    StMob    GDP 0.84
```
