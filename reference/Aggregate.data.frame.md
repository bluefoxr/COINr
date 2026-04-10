# Aggregate data frame

Aggregates a data frame into a single column using a specified function.
Note that COINr has a number of aggregation functions built in, all of
which are of the form `a_*()`, e.g.
[`a_amean()`](https://bluefoxr.github.io/COINr/reference/a_amean.md),
[`a_gmean()`](https://bluefoxr.github.io/COINr/reference/a_gmean.md) and
friends.

## Usage

``` r
# S3 method for class 'data.frame'
Aggregate(
  x,
  f_ag = NULL,
  f_ag_para = NULL,
  dat_thresh = NULL,
  by_df = FALSE,
  ...
)
```

## Arguments

- x:

  Data frame to be aggregated

- f_ag:

  The name of an aggregation function, as a string.

- f_ag_para:

  Any additional parameters to pass to `f_ag`, as a named list.

- dat_thresh:

  An optional data availability threshold, specified as a number between
  0 and 1. If a row of `x` has data availability lower than this
  threshold, the aggregated value for that row will be `NA`. Data
  availability, for a row `x_row` is defined as
  `sum(!is.na(x_row))/length(x_row)`, i.e. the fraction of non-`NA`
  values.

- by_df:

  Controls whether to send a numeric vector to `f_ag` (if `FALSE`,
  default) or a data frame (if `TRUE`) - see details.

- ...:

  arguments passed to or from other methods.

## Value

A numeric vector

## Details

Aggregation is performed row-wise using the function `f_ag`, such that
for each row `x_row`, the output is `f_ag(x_row, f_ag_para)`, and for
the whole data frame, it outputs a numeric vector. The data frame `x`
must only contain numeric columns.

The function `f_ag` must be supplied as a string, e.g. `"a_amean"`, and
it must take as a minimum an input `x` which is either a numeric vector
(if `by_df = FALSE`), or a data frame (if `by_df = TRUE`). In the former
case `f_ag` should return a single numeric value (i.e. the result of
aggregating `x`), or in the latter case a numeric vector (the result of
aggregating the whole data frame in one go).

`f_ag` can optionally have other parameters, e.g. weights, specified as
a list in `f_ag_para`.

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
# get some indicator data - take a few columns from built in data set
X <- ASEM_iData[12:15]

# normalise to avoid zeros - min max between 1 and 100
X <- Normalise(X,
               global_specs = list(f_n = "n_minmax",
                                    f_n_para = list(l_u = c(1,100))))

# aggregate using harmonic mean, with some weights
y <- Aggregate(X, f_ag = "a_hmean", f_ag_para = list(w = c(1, 1, 2, 1)))
```
