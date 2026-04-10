# Weighted arithmetic mean

The vector of weights `w` is relative since the formula is:

## Usage

``` r
a_amean(x, w)
```

## Arguments

- x:

  A numeric vector.

- w:

  A vector of numeric weights of the same length as `x`.

## Value

The weighted mean as a scalar value

## Details

\$\$ y = \frac{1}{\sum w_i} \sum w_i x_i \$\$

If `x` contains `NA`s, these `x` values and the corresponding `w` values
are removed before applying the formula above.

## Examples

``` r
x <- c(1:10)
w <- c(10:1)
a_amean(x,w)
#> [1] 4
```
