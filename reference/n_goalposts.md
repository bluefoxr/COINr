# Normalise using goalpost method

The fraction of the distance of each value of `x` from the lower
"goalpost" to the upper one. Goalposts are specified by
`gposts = c(l, u, a)`, where `l` is the lower bound, `u` is the upper
bound, and `a` is a scaling parameter.

## Usage

``` r
n_goalposts(x, gposts, direction = 1, trunc2posts = TRUE)
```

## Arguments

- x:

  A numeric vector

- gposts:

  A numeric vector `c(l, u, a)`, where `l` is the lower bound, `u` is
  the upper bound, and `a` is a scaling parameter.

- direction:

  Either 1 or -1. Set to -1 to flip goalposts.

- trunc2posts:

  If `TRUE` (default) will truncate any values that fall outside of the
  goalposts.

## Value

Numeric vector

## Details

Specify `direction = -1` to "flip" the goalposts. In this case, the
fraction from the upper to the lower goalpost is measured.

The goalposts equations are:

\$\$ (x - GP\_{low})/(GP\_{high} - GP\_{low}) \$\$

and for a negative directionality indicator:

\$\$ (x - GP\_{high})/(GP\_{low} - GP\_{high}) \$\$

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns:

- `goalpost_lower`: the lower goalpost

- `goalpost_upper`: the upper goalpost

- `goalpost_scale`: the scaling parameter

- `goalpost_trunc2posts`: corresponds to the `trunc2posts` argument

to the `iMeta` table. Then set `f_n_para = "use_iMeta"` within the
`global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
# positive direction
n_goalposts(1, gposts = c(0, 10, 1))
#> [1] 0.1
# negative direction
n_goalposts(1, gposts = c(0, 10, 1), direction = -1)
#> [1] 0.9
```
