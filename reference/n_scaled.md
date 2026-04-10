# Scale a vector

Scales a vector for normalisation using the method applied in the
GII2020 for some indicators. This does
`x_scaled <- (x-l)/(u-l) * scale_factor`. Note this is *not* the minmax
transformation (see
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)).
This is a linear transformation with shift `u` and scaling factor `u-l`.

## Usage

``` r
n_scaled(x, npara = c(0, 100), scale_factor = 100)
```

## Arguments

- x:

  A numeric vector

- npara:

  Parameters as a vector `c(l, u)`. See description.

- scale_factor:

  Optional scaling factor to apply to the result. Default 100.

## Value

Scaled vector

## Details

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns `scaled_lower`, `scaled_upper` and
`scale_factor` to the `iMeta` table, which specify the first and second
elements of `npara`, respectively. Then set `f_n_para = "use_iMeta"`
within the `global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
x <- runif(20)
n_scaled(x, npara = c(1,10))
#>  [1]  -7.822166  -3.966640  -2.253772  -1.934779  -4.014307  -1.806447
#>  [7]  -3.618005  -8.035937  -2.106239  -7.789435  -6.035125  -7.541234
#> [13] -11.029090  -1.003151  -2.566811 -11.043756 -10.322312  -2.220223
#> [19]  -3.922382  -6.243021
```
