# Normalise as fraction of max value

The ratio of each value of `x` to `max(x)`.

## Usage

``` r
n_fracmax(x)
```

## Arguments

- x:

  A numeric vector

## Value

Numeric vector

## Details

\$\$ x / x\_{max} \$\$

## Examples

``` r
x <- runif(20)
n_fracmax(x)
#>  [1] 0.09984128 0.49843601 0.78661835 0.30029104 0.56632329 0.80909147
#>  [7] 0.71026144 0.89335584 0.74599727 0.07794426 1.00000000 0.44003248
#> [13] 0.95707559 0.19692764 0.48659092 0.27753959 0.68561449 0.26421969
#> [19] 0.82413666 0.97875220
```
