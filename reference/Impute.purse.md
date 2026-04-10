# Impute data sets in a purse

This function imputes the target data set `dset` in each coin using the
imputation function `f_i`, and optionally by specifying parameters via
`f_i_para`. This is performed in the same way as the coin method
[`Impute.coin()`](https://bluefoxr.github.io/COINr/reference/Impute.coin.md),
i.e. each time point is imputed separately, *unless*
`f_i = "impute_panel"`. See details for more information.

## Usage

``` r
# S3 method for class 'purse'
Impute(
  x,
  dset,
  f_i = NULL,
  f_i_para = NULL,
  impute_by = "column",
  group_level = NULL,
  use_group = NULL,
  normalise_first = NULL,
  write_to = NULL,
  warn_on_NAs = TRUE,
  ...
)
```

## Arguments

- x:

  A purse object

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- f_i:

  An imputation function. For the "purse" class, if
  `f_i = "impute_panel` this is a special case: see details.

- f_i_para:

  Further arguments to pass to `f_i`, other than `x`. See details.

- impute_by:

  Specifies how to impute: if `"column"`, passes each column (indicator)
  separately as a numerical vector to `f_i`; if `"row"`, passes each
  *row* separately; and if `"df"` passes the entire data set (data
  frame) to `f_i`. The function called by `f_i` should be compatible
  with the type of data passed to it.

- group_level:

  A level of the framework to use for grouping indicators. This is only
  relevant if `impute_by = "row"` or `"df"`. In that case, indicators
  will be split into their groups at the level specified by
  `group_level`, and imputation will be performed across rows of the
  group, rather than the whole data set. This can make more sense
  because indicators within a group are likely to be more similar.

- use_group:

  Optional grouping variable name to pass to imputation function if this
  supports group imputation.

- normalise_first:

  Logical: if `TRUE`, each column is normalised using a min-max
  operation before imputation. By default this is `FALSE` unless
  `impute_by = "row"`. See details.

- write_to:

  Optional character string for naming the resulting data set in each
  coin. Data will be written to `.$Data[[write_to]]`. Default is
  `write_to == "Imputed"`.

- warn_on_NAs:

  Logical: if `TRUE` will issue a warning if there are any `NA`s
  detected in the data frame after imputation has been applied. Set
  `FALSE` to suppress these warnings.

- ...:

  arguments passed to or from other methods.

## Value

An updated purse with imputed data sets added to each coin.

## Details

If `f_i = "impute_panel"` this is treated as a special case, and the
data sets inside the purse are imputed using the
[`impute_panel()`](https://bluefoxr.github.io/COINr/reference/impute_panel.md)
function, which allows imputation of time series, using past/future
values as information for imputation.

In this case, coins are not imputed individually, but treated as a
single data set. To do this, set `f_i = "impute_panel"` and pass further
parameters to
[`impute_panel()`](https://bluefoxr.github.io/COINr/reference/impute_panel.md)
using the `f_i_para` argument. Note that as this is a special case, the
supported parameters of
[`impute_panel()`](https://bluefoxr.github.io/COINr/reference/impute_panel.md)
to specify through
[`Impute()`](https://bluefoxr.github.io/COINr/reference/Impute.md) are
`"imp_type"` and `"max_time"` (see
[`Impute()`](https://bluefoxr.github.io/COINr/reference/Impute.md) for
details on these). No further arguments need to be passed to
[`impute_panel()`](https://bluefoxr.github.io/COINr/reference/impute_panel.md).
See
[`vignette("imputation")`](https://bluefoxr.github.io/COINr/articles/imputation.md)
for more details.

## Examples

``` r
# see vignette("imputation")
```
