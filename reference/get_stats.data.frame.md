# Statistics of columns

Takes a data frame and returns a table of statistics with entries for
each column.

## Usage

``` r
# S3 method for class 'data.frame'
get_stats(
  x,
  t_skew = 2,
  t_kurt = 3.5,
  t_avail = 0.65,
  t_zero = 0.5,
  t_unq = 0.5,
  nsignif = 3,
  ...
)
```

## Arguments

- x:

  A data frame with only numeric columns.

- t_skew:

  Absolute skewness threshold. See details.

- t_kurt:

  Kurtosis threshold. See details.

- t_avail:

  Data availability threshold. See details.

- t_zero:

  A threshold between 0 and 1 for flagging indicators with high
  proportion of zeroes. See details.

- t_unq:

  A threshold between 0 and 1 for flagging indicators with low
  proportion of unique values. See details.

- nsignif:

  Number of significant figures to round the output table to.

- ...:

  arguments passed to or from other methods.

## Value

A data frame of statistics for each column

## Details

The statistics (columns in the output table) are as follows (entries
correspond to each column):

- `Min`: the minimum

- `Max`: the maximum

- `Mean`: the (arirthmetic) mean

- `Median`: the median

- `Std`: the standard deviation

- `Skew`: the skew

- `Kurt`: the kurtosis

- `N.Avail`: the number of non-`NA` values

- `N.NonZero`: the number of non-zero values

- `N.Unique`: the number of unique values

- `Frc.Avail`: the fraction of non-`NA` values

- `Frc.NonZero`: the fraction of non-zero values

- `Frc.Unique`: the fraction of unique values

- `Flag.Avail`: a data availability flag - columns with
  `Frc.Avail < t_avail` will be flagged as `"LOW"`, else `"ok"`.

- `Flag.NonZero`: a flag for columns with a high proportion of zeros.
  Any columns with `Frc.NonZero < t_zero` are flagged as `"LOW"`,
  otherwise `"ok"`.

- `Flag.Unique`: a unique value flag - any columns with
  `Frc.Unique < t_unq` are flagged as `"LOW"`, otherwise `"ok"`.

- `Flag.SkewKurt`: a skew and kurtosis flag which is an indication of
  possible outliers. Any columns with `abs(Skew) > t_skew` AND
  `Kurt > t_kurt` are flagged as `"OUT"`, otherwise `"ok"`.

The aim of this table, among other things, is to check the basic
statistics of each column/indicator, and identify any possible issues
for each indicator. For example, low data availability, having a high
proportion of zeros and/or a low proportion of unique values. Further,
the combination of skew and kurtosis (i.e. the `Flag.SkewKurt` column)
is a simple test for possible outliers, which may require treatment
using [`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md).

See also
[`vignette("analysis")`](https://bluefoxr.github.io/COINr/articles/analysis.md).

## Examples

``` r
# stats of mtcars
get_stats(mtcars)
#>    iCode   Min    Max    Mean Median     Std   Skew   Kurt N.Avail N.NonZero
#> 1    mpg 10.40  33.90  20.100  19.20   6.030  0.672 -0.022      32        32
#> 2    cyl  4.00   8.00   6.190   6.00   1.790 -0.192 -1.760      32        32
#> 3   disp 71.10 472.00 231.000 196.00 124.000  0.420 -1.070      32        32
#> 4     hp 52.00 335.00 147.000 123.00  68.600  0.799  0.275      32        32
#> 5   drat  2.76   4.93   3.600   3.70   0.535  0.293 -0.450      32        32
#> 6     wt  1.51   5.42   3.220   3.32   0.978  0.466  0.417      32        32
#> 7   qsec 14.50  22.90  17.800  17.70   1.790  0.406  0.865      32        32
#> 8     vs  0.00   1.00   0.438   0.00   0.504  0.265 -2.060      32        14
#> 9     am  0.00   1.00   0.406   0.00   0.499  0.401 -1.970      32        13
#> 10  gear  3.00   5.00   3.690   4.00   0.738  0.582 -0.895      32        32
#> 11  carb  1.00   8.00   2.810   2.00   1.620  1.160  2.020      32        32
#>    N.Unique N.Same Frc.Avail Frc.NonZero Frc.Unique Frc.Same Flag.Avail
#> 1        25      2         1       1.000     0.7810   0.0625         ok
#> 2         3     14         1       1.000     0.0938   0.4380         ok
#> 3        27      3         1       1.000     0.8440   0.0938         ok
#> 4        22      3         1       1.000     0.6880   0.0938         ok
#> 5        22      3         1       1.000     0.6880   0.0938         ok
#> 6        29      3         1       1.000     0.9060   0.0938         ok
#> 7        30      2         1       1.000     0.9380   0.0625         ok
#> 8         2     18         1       0.438     0.0625   0.5620         ok
#> 9         2     19         1       0.406     0.0625   0.5940         ok
#> 10        3     15         1       1.000     0.0938   0.4690         ok
#> 11        6     10         1       1.000     0.1880   0.3120         ok
#>    Flag.NonZero Flag.Unique Flag.SkewKurt
#> 1            ok          ok            ok
#> 2            ok         LOW            ok
#> 3            ok          ok            ok
#> 4            ok          ok            ok
#> 5            ok          ok            ok
#> 6            ok          ok            ok
#> 7            ok          ok            ok
#> 8           LOW         LOW            ok
#> 9           LOW         LOW            ok
#> 10           ok         LOW            ok
#> 11           ok         LOW            ok
```
