# Normalise as distance to target

A measure of the distance of each value of `x` to a specified target
which can be a high or low target depending on `direction`. See details
below.

## Usage

``` r
n_dist2targ(x, targ, direction = 1, cap_max = FALSE)
```

## Arguments

- x:

  A numeric vector

- targ:

  An target value

- direction:

  Either 1 (default) or -1. In the former case, the indicator is assumed
  to be "positive" so that the target is at the higher end of the range.
  In the latter, the indicator is "negative" so that the target is
  typically at the low end of the range.

- cap_max:

  If `TRUE`, any value of `x` that exceeds `targ` will be assigned a
  score of 1, otherwise will have a score greater than 1.

## Value

Numeric vector

## Details

If `direction = 1`, the formula is:

\$\$ \frac{x - x\_{min}}{x\_{targ} - x\_{min}} \$\$

else if `direction = -1`:

\$\$ \frac{x\_{max} - x}{x\_{max} - x\_{targ}} \$\$

Values surpassing `x_targ` in either case can be optionally capped at 1
if `cap_max = TRUE`.

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns `Target`, and `dist2targ_cap_max` to the
`iMeta` table, which correspond to the `targ` and `cap_max` parameters
respectively. Then set `f_n_para = "use_iMeta"` within the
`global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
x <- runif(20)
n_dist2targ(x, 0.8, cap_max = TRUE)
#>  [1] 1.00000000 0.74879057 0.25307822 0.77315357 0.34568274 0.82982648
#>  [7] 0.94903502 0.36775076 0.71309979 0.11968551 1.00000000 0.79871352
#> [13] 0.52660878 0.67532767 0.43462806 0.08749205 1.00000000 0.00000000
#> [19] 0.54956375 0.32225213
```
