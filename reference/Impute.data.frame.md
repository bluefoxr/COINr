# Impute a data frame

Impute a data frame using any function, either column-wise, row-wise or
by the whole data frame in one shot.

## Usage

``` r
# S3 method for class 'data.frame'
Impute(
  x,
  f_i = NULL,
  f_i_para = NULL,
  impute_by = "column",
  normalise_first = NULL,
  directions = NULL,
  warn_on_NAs = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame with only numeric columns.

- f_i:

  A function to use for imputation. By default, imputation is performed
  by simply substituting the mean of non-`NA` values for each column at
  a time.

- f_i_para:

  Any additional parameters to pass to `f_i`, apart from `x`

- impute_by:

  Specifies how to impute: if `"column"`, passes each column separately
  as a numerical vector to `f_i`; if `"row"`, passes each *row*
  separately; and if `"df"` passes the entire data frame to `f_i`. The
  function called by `f_i` should be compatible with the type of data
  passed to it.

- normalise_first:

  Logical: if `TRUE`, each column is normalised using a min-max
  operation before imputation. By default this is `FALSE` unless
  `impute_by = "row"`. See details.

- directions:

  A vector of directions: either -1 or 1 to indicate the direction of
  each column of `x` - this is only used if `normalise_first = TRUE`.
  See details.

- warn_on_NAs:

  Logical: if `TRUE` will issue a warning if there are any `NA`s
  detected in the data frame after imputation has been applied. Set
  `FALSE` to suppress these warnings.

- ...:

  arguments passed to or from other methods.

## Value

An imputed data frame

## Details

This function only accepts data frames with all numeric columns. It
imputes any `NA`s in the data frame by invoking the function `f_i` and
any optional arguments `f_i_para` on each column at a time (if
`impute_by = "column"`), or on each row at a time (if
`impute_by = "row"`), or by passing the entire data frame to `f_i` if
`impute_by = "df"`.

Clearly, the function `f_i` needs to be able to accept with the data
class passed to it - if `impute_by` is `"row"` or `"column"` this will
be a numeric vector, or if `"df"` it will be a data frame. Moreover,
this function should return a vector or data frame identical to the
vector/data frame passed to it except for `NA` values, which can be
replaced. The function `f_i` is not required to replace *all* `NA`
values.

COINr has several built-in imputation functions of the form `i_*()` for
vectors which can be called by
[`Impute()`](https://bluefoxr.github.io/COINr/reference/Impute.md). See
the [online
documentation](https://bluefoxr.github.io/COINr/articles/imputation.html#data-frames)
for more details.

When imputing row-wise, prior normalisation of the data is recommended.
This is because imputation will use e.g. the mean of the unit values
over all indicators (columns). If the indicators are on very different
scales, the result will likely make no sense. If the indicators are
normalised first, more sensible results can be obtained. There are two
options to pre-normalise: first is by setting `normalise_first = TRUE` -
this is anyway the default if `impute_by = "row"`. In this case, you
also need to supply a vector of directions. The data will then be
normalised using a min-max approach before imputation, followed by the
inverse operation to return the data to the original scales.

Another approach which gives more control is to simply run
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
first, and work with the normalised data from that point onwards. In
that case it is better to set `normalise_first = FALSE`, since by
default if `impute_by = "row"` it will be set to `TRUE`.

Checks are made on the format of the data returned by imputation
functions, to ensure the type and that non-`NA` values have not been
inadvertently altered. This latter check is allowed a degree of
tolerance for numerical precision, controlled by the `sfigs` argument.
This is because if the data frame is normalised, and/or depending on the
imputation function, there may be a very small differences. By default
`sfigs = 9`, meaning that the non-`NA` values pre and post-imputation
are compared to 9 significant figures.

## Examples

``` r
# a df of random numbers
X <- as.data.frame(matrix(runif(50), 10, 5))

# introduce NAs (2 in 3 of 5 cols)
X[sample(1:10, 2), 1] <- NA
X[sample(1:10, 2), 3] <- NA
X[sample(1:10, 2), 5] <- NA

# impute using column mean
Impute(X, f_i = "i_mean")
#>            V1         V2         V3         V4         V5
#> 1  0.32128915 0.28989230 0.03123033 0.68016292 0.36475476
#> 2  0.17494063 0.67838043 0.43161838 0.49884561 0.31524175
#> 3  0.03424133 0.73531960 0.30083081 0.64167935 0.17467589
#> 4  0.32038573 0.19595673 0.63646561 0.66028435 0.53157354
#> 5  0.40232824 0.98053967 0.43161838 0.09602416 0.49363702
#> 6  0.19566983 0.74152153 0.43217126 0.76560016 0.77930863
#> 7  0.40353812 0.05144628 0.70643384 0.76967480 0.20417834
#> 8  0.06366146 0.53021246 0.94857658 0.99071231 0.36475476
#> 9  0.32128915 0.69582388 0.18033877 0.97052090 0.06521611
#> 10 0.97554784 0.68855600 0.21689988 0.38918276 0.35420680

# impute using row median (no normalisation)
Impute(X, f_i = "i_median", impute_by = "row",
       normalise_first = FALSE)
#>            V1         V2         V3         V4         V5
#> 1  0.28989230 0.28989230 0.03123033 0.68016292 0.28989230
#> 2  0.17494063 0.67838043 0.40704368 0.49884561 0.31524175
#> 3  0.03424133 0.73531960 0.30083081 0.64167935 0.17467589
#> 4  0.32038573 0.19595673 0.63646561 0.66028435 0.53157354
#> 5  0.40232824 0.98053967 0.44798263 0.09602416 0.49363702
#> 6  0.19566983 0.74152153 0.43217126 0.76560016 0.77930863
#> 7  0.40353812 0.05144628 0.70643384 0.76967480 0.20417834
#> 8  0.06366146 0.53021246 0.94857658 0.99071231 0.73939452
#> 9  0.43808132 0.69582388 0.18033877 0.97052090 0.06521611
#> 10 0.97554784 0.68855600 0.21689988 0.38918276 0.35420680

```
