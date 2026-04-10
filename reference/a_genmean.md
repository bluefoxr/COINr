# Weighted generalised mean

Weighted generalised mean of a vector. `NA` are skipped by default.

## Usage

``` r
a_genmean(x, w = NULL, p)
```

## Arguments

- x:

  A numeric vector of positive values.

- w:

  A vector of weights, which should have length equal to `length(x)`.
  Weights are relative and will be re-scaled to sum to 1. If `w` is not
  specified, defaults to equal weights.

- p:

  Coefficient - see details.

## Value

Weighted harmonic mean, as a numeric value.

## Details

The generalised mean is as follows:

\$\$ y = \left( \frac{1}{\sum w_i} \sum w_i x_i^p \right)^{1/p} \$\$

where `p` is a coefficient specified in the function argument here. Note
that:

- For negative `p`, all `x` values must be positive

- Setting `p = 0` will result in an error due to the negative exponent.
  This case is equivalent to the geometric mean in the limit, so use
  [`a_gmean()`](https://bluefoxr.github.io/COINr/reference/a_gmean.md)
  instead.

## Examples

``` r
# a vector of values
x <- 1:10
# a vector of weights
w <- runif(10)
# cubic mean
a_genmean(x,w, p = 2)
#> [1] 5.772109
```
