# Get correlations

Helper function for getting correlations between indicators and
aggregates. This retrieves subsets of correlation matrices between
different aggregation levels, in different formats. By default, it will
return a long-form data frame, unless `make_long = FALSE`. By default,
any correlations with a p-value less than 0.05 are replaced with `NA`.
See `pval` argument to adjust this.

## Usage

``` r
get_corr(
  coin,
  dset,
  iCodes = NULL,
  Levels = NULL,
  ...,
  cortype = "pearson",
  pval = 0.05,
  withparent = FALSE,
  grouplev = NULL,
  make_long = TRUE,
  use_directions = FALSE
)
```

## Arguments

- coin:

  A coin class coin object

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- iCodes:

  An optional list of character vectors where the first entry specifies
  the indicator/aggregate codes to correlate against the second entry
  (also a specification of indicator/aggregate codes). If this is
  specified as a character vector it will coerced to the first entry of
  a list, i.e. `list(iCodes)`.

- Levels:

  The aggregation levels to take the two groups of indicators from. See
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
  for details. Defaults to indicator level.

- ...:

  Further arguments to be passed to
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
  (`uCodes` and `use_group`).

- cortype:

  The type of correlation to calculate, either `"pearson"`,
  `"spearman"`, or `"kendall"`.

- pval:

  The significance level for including correlations. Correlations with
  \\p \> pval\\ will be returned as `NA`. Default 0.05. Set to 0 to
  disable this.

- withparent:

  If `TRUE`, and `aglev[1] != aglev[2]`, will only return correlations
  of each row with its parent. Alternatively, if
  `withparent = "family"`, will return correlations with parents,
  grandparents etc, up to the highest level. In both cases the data set
  must be aggregated for this to work.

- grouplev:

  The aggregation level to group correlations by if
  `aglev[1] == aglev[2]`. Requires that `make_long = TRUE`.

- make_long:

  Logical: if `TRUE`, returns correlations in long format (default),
  else if `FALSE` returns in wide format. Note that if wide format is
  requested, features specified by `grouplev` and `withparent` are not
  supported.

- use_directions:

  Logical: if `TRUE` the extracted data is adjusted using directions
  found inside the coin (i.e. the "Direction" column input in `iMeta`:
  any indicators with negative direction will have their values
  multiplied by -1 which will reverse the direction of correlation).
  This should only be set to `TRUE` if the data set has not yet been
  normalised. For example, this can be useful to set to `TRUE` to
  analyse correlations in the raw data, but would make no sense to
  analyse correlations in the normalised data because that already has
  the direction adjusted! So you would reverse direction twice. In other
  words, use this at your discretion.

## Value

A data frame of pairwise correlation values in wide or long format (see
`make_long`). Correlations with \\p \> pval\\ will be returned as `NA`.

## Details

This function allows you to obtain correlations between any subset of
indicators or aggregates, from any data set present in a coin. Indicator
selection is performed using
[`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md).
Two different indicator sets can be correlated against each other by
specifying `iCodes` and `Levels` as vectors.

The correlation type can be specified by the `cortype` argument, which
is passed to [`stats::cor()`](https://rdrr.io/r/stats/cor.html).

The `withparent` argument will optionally only return correlations which
correspond to the structure of the index. For example, if
`Levels = c(1,2)` (i.e. we wish to correlate indicators from Level 1
with aggregates from Level 2), and we set `withparent = TRUE`, only the
correlations between each indicator and its parent group will be
returned (not correlations between indicators and other aggregates to
which it does not belong). This can be useful to check whether
correlations of an indicator/aggregate with any of its parent groups
exceeds or falls below thresholds.

Similarly, the `grouplev` argument can be used to restrict correlations
to within groups corresponding to the index structure. Setting e.g.
`grouplev = 2` will only return correlations within the groups defined
at Level 2.

The `grouplev` and `withparent` options are disabled if
`make_long = FALSE`.

Note that this function can only call correlations within the same data
set (i.e. only one data set in `.$Data`).

This function replaces the now-defunct `getCorr()` from COINr \< v1.0.

## See also

- [`plot_corr()`](https://bluefoxr.github.io/COINr/reference/plot_corr.md)
  Plot correlation matrices of indicator subsets

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# get correlations
cmat <- get_corr(coin, dset = "Raw", iCodes = list("Environ"),
                 Levels = 1, make_long = FALSE)
```
