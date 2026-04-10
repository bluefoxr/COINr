# Aggregate indicators

Aggregates indicators following the structure specified in `iMeta`, for
each coin inside the purse. See
[`Aggregate.coin()`](https://bluefoxr.github.io/COINr/reference/Aggregate.coin.md),
which is applied to each coin, for more information

## Usage

``` r
# S3 method for class 'purse'
Aggregate(
  x,
  dset,
  f_ag = NULL,
  w = NULL,
  f_ag_para = NULL,
  dat_thresh = NULL,
  write_to = NULL,
  by_df = FALSE,
  ...
)
```

## Arguments

- x:

  A purse-class object

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

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Aggregated"`.

- by_df:

  Controls whether to send a numeric vector to `f_ag` (if `FALSE`,
  default) or a data frame (if `TRUE`) - see details. Can also be
  specified as a logical vector of length `n-1`, where `n` is the number
  of levels in the index structure.

- ...:

  arguments passed to or from other methods.

## Value

An updated purse with new treated data sets added at
`.$Data[[write_to]]` in each coin.

## Examples

``` r
# build example purse up to normalised data set
purse <- build_example_purse(up_to = "Normalise", quietly = TRUE)

# aggregate using defaults
purse <- Aggregate(purse, dset = "Normalised")
#> Written data set to .$Data$Aggregated
#> Written data set to .$Data$Aggregated
#> Written data set to .$Data$Aggregated
#> Written data set to .$Data$Aggregated
#> Written data set to .$Data$Aggregated
```
