# P-values for correlations in a data frame or matrix

This is a stripped down version of the "cor.mtest()" function from the
"corrplot" package. It uses the
[`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) function to
calculate pairwise p-values. Unlike the corrplot version, this only
calculates p-values, and not confidence intervals. Credit to corrplot
for this code, I only replicate it here to avoid depending on their
package for a single function.

## Usage

``` r
get_pvals(X, ...)
```

## Arguments

- X:

  A numeric matrix or data frame

- ...:

  Additional arguments passed to function
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html), e.g.
  `conf.level = 0.95`.

## Value

Matrix of p-values

## Examples

``` r
# a matrix of random numbers, 3 cols
x <- matrix(runif(30), 10, 3)

# get correlations between cols
cor(x)
#>            [,1]       [,2]       [,3]
#> [1,]  1.0000000 -0.4518163 -0.5778409
#> [2,] -0.4518163  1.0000000  0.4333702
#> [3,] -0.5778409  0.4333702  1.0000000

# get p values of correlations between cols
get_pvals(x)
#>            [,1]      [,2]       [,3]
#> [1,] 0.00000000 0.1899003 0.08019865
#> [2,] 0.18990031 0.0000000 0.21087999
#> [3,] 0.08019865 0.2108800 0.00000000
```
