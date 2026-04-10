# Normalise as distance to maximum value

A measure of the distance to the maximum value, where the maximum value
is the highest-scoring value. The formula used is:

## Usage

``` r
n_dist2max(x)
```

## Arguments

- x:

  A numeric vector

## Value

Numeric vector

## Details

\$\$ 1 - (x\_{max} - x)/(x\_{max} - x\_{min}) \$\$

This means that the closer a value is to the maximum, the higher its
score will be. Scores will be in the range of 0 to 1.

## Examples

``` r
x <- runif(20)
n_dist2max(x)
#>  [1] 0.00000000 0.29614326 0.38625088 0.86992765 0.16770452 0.49225948
#>  [7] 0.37314663 0.18037182 1.00000000 0.01529551 0.97864163 0.77294056
#> [13] 0.49070972 0.38631307 0.09333637 0.34905571 0.78214635 0.63651794
#> [19] 0.79081408 0.80977681
```
