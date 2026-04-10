# Minmax a vector

Scales a vector using min-max method.

## Usage

``` r
n_minmax(x, l_u = c(0, 100))
```

## Arguments

- x:

  A numeric vector

- l_u:

  A vector `c(l, u)`, where `l` is the lower bound and `u` is the upper
  bound. `x` will be scaled exactly onto this interval.

## Value

Normalised vector

## Details

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns `minmax_lower`, and `minmax_upper` to
the `iMeta` table, which specify the lower and upper bounds to scale
each indicator to. Then set `f_n_para = "use_iMeta"` within the
`global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
x <- runif(20)
n_minmax(x)
#>  [1]  70.502348  81.616101  50.801442  10.689393 100.000000  55.262608
#>  [7]  83.838049   0.000000  31.886069  42.319863  20.549855   8.412745
#> [13]  16.678175  48.906381   4.120192  57.214026  80.678864  59.890016
#> [19]  35.771866  26.865198
```
