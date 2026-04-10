# Find highly-correlated indicators within groups

This returns a data frame of any highly correlated indicators within the
same aggregation group. The level of the aggregation grouping can be
controlled by the `grouplev` argument.

## Usage

``` r
get_corr_flags(
  coin,
  dset,
  cor_thresh = 0.9,
  thresh_type = "high",
  cortype = "pearson",
  grouplev = NULL,
  roundto = 3,
  use_directions = FALSE
)
```

## Arguments

- coin:

  A coin class object

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- cor_thresh:

  A threshold to flag high correlation. Default 0.9.

- thresh_type:

  Either `"high"`, which will only flag correlations *above*
  `cor_thresh`, or `"low"`, which will only flag correlations *below*
  `cor_thresh`.

- cortype:

  The type of correlation, either `"pearson"` (default), `"spearman"` or
  `"kendall"`. See [stats::cor](https://rdrr.io/r/stats/cor.html).

- grouplev:

  The level to group indicators in. E.g. if `grouplev = 2` it will look
  for high correlations between indicators that belong to the same group
  in Level 2.

- roundto:

  Number of decimal places to round correlations to. Default 3. Set
  `NULL` to disable rounding.

- use_directions:

  Logical: if `TRUE` the extracted data is adjusted using directions
  found inside the coin (i.e. the "Direction" column input in `iMeta`.
  See comments on this argument in
  [`get_corr()`](https://bluefoxr.github.io/COINr/reference/get_corr.md).

## Value

A data frame with one entry for every indicator pair that is highly
correlated within the same group, at the specified level. Pairs are only
reported once, i.e. only uses the upper triangle of the correlation
matrix.

## Details

This function is motivated by the idea that having very
highly-correlated indicators within the same group may amount to double
counting, or possibly redundancy in the framework.

This function replaces the now-defunct `hicorrSP()` from COINr \< v1.0.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "Normalise", quietly = TRUE)

# get correlations between indicator over 0.75 within level 2 groups
get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75,
               thresh_type = "high", grouplev = 2)
#>      Group Ind1      Ind2  Corr
#> 113 Social  CPI FreePress 0.761
#> 116 Social  CPI      NGOs 0.768
```
