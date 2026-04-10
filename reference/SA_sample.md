# Generate sample for sensitivity analysis

Generates an input sample for a Monte Carlo estimation of global
sensitivity indices. Used in the
[`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
function. The total sample size will be \\N(d+2)\\.

## Usage

``` r
SA_sample(N, d)
```

## Arguments

- N:

  The number of sample points per dimension.

- d:

  The dimensionality of the sample

## Value

A matrix with \\N(d+2)\\ rows and `d` columns.

## Details

This function generates a Monte Carlo sample as described e.g. in the
[Global Sensitivity Analysis: The Primer
book](https://onlinelibrary.wiley.com/doi/book/10.1002/9780470725184).

## See also

- [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
  Perform global sensitivity or uncertainty analysis on a COIN.

- [`SA_estimate()`](https://bluefoxr.github.io/COINr/reference/SA_estimate.md)
  Estimate sensitivity indices from system output, as a result of input
  design from SA_sample().

## Examples

``` r
# sensitivity analysis sample for 3 dimensions with 100 points per dimension
X <- SA_sample(100, 3)
```
