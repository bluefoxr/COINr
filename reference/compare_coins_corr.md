# Compare two coins by correlation

Given two coins, this function returns the correlation between the two
coins, for target datset `dset` and target indicator code(s) `iCodes`.
Correlation is calculated as the Pearson correlation coefficient, but if
`compare_by = "Ranks"` then this is the correlation coefficient of the
ranks, which amounts to the Spearman rank correlation. Set
`compare_by = "Scores"` to return the Pearson correlation between
scores.

## Usage

``` r
compare_coins_corr(coin1, coin2, dset, iCodes, compare_by = "ranks")
```

## Arguments

- coin1:

  A coin

- coin2:

  A coin, with possibly alternative methodology. This should share at
  least two units in common with `coin1`.

- dset:

  Target data set, must be present in both `coin1` and `coin2`

- iCodes:

  Character vector of indicator codes to correlate between the two
  coins.

- compare_by:

  Either `"Ranks"` or `"Scores"`.

## Value

A list containing a correlation table and a list of comparison data
frames.

## Examples

``` r
# build example
coin <- build_example_coin()
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Imputed
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Treated
#> Written data set to .$Data$Normalised
#> Written data set to .$Data$Aggregated

# copy coin
coin2 <- coin

# change to prank function (percentile ranks)
# we don't need to specify any additional parameters (f_n_para) here
coin2$Log$Normalise$global_specs <- list(f_n = "n_prank")

# regenerate
coin2 <- Regen(coin2)

# iCodes to compare: all at level 3 and 4
iCodes <- coin$Meta$Ind$iCode[which(coin$Meta$Ind$Level > 2)]

# compare index, sort by absolute rank difference
l_comp <- compare_coins_corr(coin, coin2, dset = "Aggregated", iCodes = iCodes)

# see df
l_comp$df_corr
#>       iCode Correlation
#> Conn   Conn   0.9780995
#> Sust   Sust   0.9299709
#> Index Index   0.9759276
```
