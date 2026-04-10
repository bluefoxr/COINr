# Quick outlier treatment of a data frame

A simplified version of
[`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md) which
allows direct access to the default parameters. This has less
flexibility, but is an easier interface and probably more convenient if
the objective is to use the default treatment process but with some
minor adjustments.

## Usage

``` r
# S3 method for class 'data.frame'
qTreat(x, winmax = 5, skew_thresh = 2, kurt_thresh = 3.5, f2 = "log_CT", ...)
```

## Arguments

- x:

  A numeric data frame

- winmax:

  Maximum number of points to Winsorise for each column. Default 5.

- skew_thresh:

  Absolute skew threshold - default 2.

- kurt_thresh:

  Kurtosis threshold - default 3.5.

- f2:

  Function to call if Winsorisation does not bring skew and kurtosis
  within limits. Default `"log_CT"`.

- ...:

  arguments passed to or from other methods.

## Value

A list

## Details

This function treats each column in `x` using the following process:

- First, it checks whether skew and kurtosis are within the specified
  limits of `skew_thresh` and `kurt_thresh`

- If the column is not within the limits, it applies the
  [`winsorise()`](https://bluefoxr.github.io/COINr/reference/winsorise.md)
  function, with maximum number of winsorised points specified by
  `winmax`.

- If winsorisation does not bring the column within the skew/kurtosis
  limits, it is instead passed to `f2`, which is a second outlier
  treatment function, default
  [`log_CT()`](https://bluefoxr.github.io/COINr/reference/log_CT.md).

The arguments of
[`qTreat()`](https://bluefoxr.github.io/COINr/reference/qTreat.md) are
passed to
[`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md).

See [`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md)
documentation for more details, and
[`vignette("treat")`](https://bluefoxr.github.io/COINr/articles/treat.md).

## Examples

``` r
# select three indicators
df1 <- ASEM_iData[c("Flights", "Goods", "Services")]

# treat data frame, changing winmax and skew/kurtosis limits
l_treat <- qTreat(df1, winmax = 1, skew_thresh = 1.5, kurt_thresh = 3)

# Now we check what the results are:
l_treat$Dets_Table
#>      iCode check_SkewKurt0.Pass check_SkewKurt0.Skew check_SkewKurt0.Kurt
#> 1  Flights                FALSE             2.103287             4.508879
#> 2    Goods                FALSE             2.649973             8.266610
#> 3 Services                 TRUE             1.701085             2.375656
#>   winsorise.nwin check_SkewKurt1.Pass check_SkewKurt1.Skew check_SkewKurt1.Kurt
#> 1              1                FALSE             1.900658             3.336065
#> 2              1                FALSE             2.469910             7.087309
#> 3             NA                   NA                   NA                   NA
#>   check_SkewKurt2.Pass check_SkewKurt2.Skew check_SkewKurt2.Kurt
#> 1                 TRUE          -0.09502644           -0.8305217
#> 2                 TRUE           0.03104001           -0.8888965
#> 3                   NA                   NA                   NA
```
