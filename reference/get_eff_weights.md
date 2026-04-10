# Get effective weights

Calculates the "effective weight" of each indicator and aggregate at the
index level. The effective weight is calculated as the final weight of
each component in the index, and this is due to not just to its own
weight, but also to the weights of each aggregation that it is involved
in, plus the number of indicators/aggregates in each group. The
effective weight is one way of understanding the final contribution of
each indicator to the index. See also
[`vignette("weights")`](https://bluefoxr.github.io/COINr/articles/weights.md).

## Usage

``` r
get_eff_weights(coin, out2 = "df")
```

## Arguments

- coin:

  A coin class object

- out2:

  Either `"coin"` or `"df"`

## Value

Either an iMeta data frame with effective weights as an added column, or
an updated coin with effective weights added to `.$Meta$Ind`.

## Details

This function replaces the now-defunct `effectiveWeight()` from COINr \<
v1.0.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# get effective weights as data frame
w_eff <- get_eff_weights(coin, out2 = "df")

head(w_eff)
#>       iCode Level Weight  EffWeight
#> 9     Goods     1      1 0.02000000
#> 10 Services     1      1 0.02000000
#> 11      FDI     1      1 0.02000000
#> 12   PRemit     1      1 0.02000000
#> 13  ForPort     1      1 0.02000000
#> 31    Renew     1      1 0.03333333
```
