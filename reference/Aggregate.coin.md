# Aggregate indicators in a coin

Aggregates a named data set specified by `dset` using aggregation
function(s) `f_ag`, weights `w`, and optional function parameters
`f_ag_para`. Note that COINr has a number of aggregation functions built
in, all of which are of the form `a_*()`, e.g.
[`a_amean()`](https://bluefoxr.github.io/COINr/reference/a_amean.md),
[`a_gmean()`](https://bluefoxr.github.io/COINr/reference/a_gmean.md) and
friends.

## Usage

``` r
# S3 method for class 'coin'
Aggregate(
  x,
  dset,
  f_ag = NULL,
  w = NULL,
  f_ag_para = NULL,
  dat_thresh = NULL,
  by_df = FALSE,
  out2 = "coin",
  write_to = NULL,
  ...
)
```

## Arguments

- x:

  A coin class object.

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- f_ag:

  The name of an aggregation function, a string. This can either be a
  single string naming a function to use for all aggregation levels, or
  else a character vector of function names of length `n-1`, where `n`
  is the number of levels in the index structure. In this latter case, a
  different aggregation function may be used for each level in the
  index: the first in the vector will be used to aggregate from Level 1
  to Level 2, the second from Level 2 to Level 3, and so on.

- w:

  An optional data frame of weights. If `f_ag` does not require accept
  weights, set to `"none"`. Alternatively, can be the name of a weight
  set found in `.$Meta$Weights`. This can also be specified as a list
  specifying the aggregation weights for each level, in the same way as
  the previous parameters.

- f_ag_para:

  Optional parameters to pass to `f_ag`, other than `x` and `w`. As with
  `f_ag`, this can specified to have different parameters for each
  aggregation level by specifying as a nested list of length `n-1`. See
  details.

- dat_thresh:

  An optional data availability threshold, specified as a number between
  0 and 1. If a row within an aggregation group has data availability
  lower than this threshold, the aggregated value for that row will be
  `NA`. Data availability, for a row `x_row` is defined as
  `sum(!is.na(x_row))/length(x_row)`, i.e. the fraction of non-`NA`
  values. Can also be specified as a vector of length `n-1`, where `n`
  is the number of levels in the index structure, to specify different
  data availability thresholds by level.

- by_df:

  Controls whether to send a numeric vector to `f_ag` (if `FALSE`,
  default) or a data frame (if `TRUE`) - see details. Can also be
  specified as a logical vector of length `n-1`, where `n` is the number
  of levels in the index structure.

- out2:

  Either `"coin"` (default) to return updated coin or `"df"` to output
  the aggregated data set.

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Aggregated"`.

- ...:

  arguments passed to or from other methods.

## Value

An updated coin with aggregated data set added at `.$Data[[write_to]]`
if `out2 = "coin"`, else if `out2 = "df"` outputs the aggregated data
set as a data frame.

## Details

When `by_df = FALSE`, aggregation is performed row-wise using the
function `f_ag`, such that for each row `x_row`, the output is
`f_ag(x_row, f_ag_para)`, and for the whole data frame, it outputs a
numeric vector. Otherwise if `by_df = TRUE`, the entire data frame of
each indicator group is passed to `f_ag`.

The function `f_ag` must be supplied as a string, e.g. `"a_amean"`, and
it must take as a minimum an input `x` which is either a numeric vector
(if `by_df = FALSE`), or a data frame (if `by_df = TRUE`). In the former
case `f_ag` should return a single numeric value (i.e. the result of
aggregating `x`), or in the latter case a numeric vector (the result of
aggregating the whole data frame in one go).

Weights are passed to the function `f_ag` as an argument named `w`. This
means that the function should have arguments that look like
`f_ag(x, w, ...)`, where `...` are possibly other input arguments to the
function. If the aggregation function doesn't use weights, you can set
`w = "none"`, and no weights will be passed to it.

`f_ag` can optionally have other parameters, apart from `x` and `w`,
specified as a list in `f_ag_para`.

The aggregation specifications can be set to be different for each level
of aggregation: the arguments `f_ag`, `f_ag_para`, `dat_thresh`, `w` and
`by_df` can all be optionally specified as vectors or lists of length
n-1, where n is the number of levels in the index. In this case, the
first value in each vector/list will be used for the first round of
aggregation, i.e. from indicators to the aggregates at level 2. The next
will be used to aggregate from level 2 to level 3, and so on.

When different functions are used for different levels, it is important
to get the list syntax correct. For example, in a case with three
aggregations using different functions, say we want to use
[`a_amean()`](https://bluefoxr.github.io/COINr/reference/a_amean.md) for
the first two levels, then a custom function `f_cust()` for the last.
`f_cust()` has some additional parameters `a` and `b`. In this case, we
would specify e.g. `f_ag_para = list(NULL, NULL, list(a = 2, b = 3))` -
this is becauase
[`a_amean()`](https://bluefoxr.github.io/COINr/reference/a_amean.md)
requires no additional parameters, so we pass `NULL`.

Note that COINr has a number of aggregation functions built in, all of
which are of the form `a_*()`, e.g.
[`a_amean()`](https://bluefoxr.github.io/COINr/reference/a_amean.md),
[`a_gmean()`](https://bluefoxr.github.io/COINr/reference/a_gmean.md) and
friends. To see a list browse COINr functions alphabetically or type
`a_` in the R Studio console and press the tab key (after loading
COINr), or see the [online
documentation](https://bluefoxr.github.io/COINr/articles/aggregate.html#coinr-aggregation-functions).

Optionally, a data availability threshold can be assigned below which
the aggregated value will return `NA` (see `dat_thresh` argument). If
`by_df = TRUE`, this will however be ignored because aggregation is not
done on individual rows. Note that more complex constraints could be
built into `f_ag` if needed.

## Examples

``` r
# build example up to normalised data set
coin <- build_example_coin(up_to = "Normalise")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Imputed
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Treated
#> Written data set to .$Data$Normalised

# aggregate normalised data set
coin <- Aggregate(coin, dset = "Normalised")
#> Written data set to .$Data$Aggregated
```
