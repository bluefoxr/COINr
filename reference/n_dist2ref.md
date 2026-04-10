# Normalise as distance to reference value

A measure of the distance to a specific value found in `x`, specified by
`iref`. The formula is:

## Usage

``` r
n_dist2ref(x, iref, cap_max = FALSE)
```

## Arguments

- x:

  A numeric vector

- iref:

  An integer which indexes `x` to specify the reference value. The
  reference value will be `x[iref]`.

- cap_max:

  If `TRUE`, any value of `x` that exceeds `x[iref]` will be assigned a
  score of 1, otherwise will have a score greater than 1.

## Value

Numeric vector

## Details

\$\$ 1 - (x\_{ref} - x)/(x\_{ref} - x\_{min}) \$\$

Values exceeding `x_ref` can be optionally capped at 1 if
`cap_max = TRUE`.

## Examples

``` r
x <- runif(20)
n_dist2ref(x, 5)
#>  [1] 0.27841584 2.42241487 3.81885237 3.56913552 1.00000000 2.17113635
#>  [7] 1.10393114 0.04832999 0.52985615 1.37422701 2.66312582 1.90842788
#> [13] 0.38014453 5.24113261 0.04198017 3.31997734 0.00000000 0.70332643
#> [19] 2.76743487 2.85026090
```
