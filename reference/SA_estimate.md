# Estimate sensitivity indices

Post process a sample to obtain sensitivity indices. This function takes
a univariate output which is generated as a result of running a Monte
Carlo sample from
[`SA_sample()`](https://bluefoxr.github.io/COINr/reference/SA_sample.md)
through a system. Then it estimates sensitivity indices using this
sample.

## Usage

``` r
SA_estimate(yy, N, d, Nboot = NULL)
```

## Arguments

- yy:

  A vector of model output values, as a result of a \\N(d+2)\\ Monte
  Carlo design.

- N:

  The number of sample points per dimension.

- d:

  The dimensionality of the sample

- Nboot:

  Number of bootstrap draws for estimates of confidence intervals on
  sensitivity indices. If this is not specified, bootstrapping is not
  applied.

## Value

A list with the output variance, plus a data frame of first order and
total order sensitivity indices for each variable, as well as
bootstrapped confidence intervals if `!is.null(Nboot)`.

## Details

This function is built to be used inside
[`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md).

## See also

- [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
  Perform global sensitivity or uncertainty analysis on a COIN

- [`SA_sample()`](https://bluefoxr.github.io/COINr/reference/SA_sample.md)
  Input design for estimating sensitivity indices

## Examples

``` r
# This is a generic example rather than applied to a COIN (for reasons of speed)

# A simple test function
testfunc <- function(x){
x[1] + 2*x[2] + 3*x[3]
}

# First, generate a sample
X <- SA_sample(500, 3)

# Run sample through test function to get corresponding output for each row
y <- apply(X, 1, testfunc)

# Estimate sensitivity indices using sample
SAinds <- SA_estimate(y, N = 500, d = 3, Nboot = 1000)
SAinds$SensInd
#>   Variable         Si        STi       Si_q5    Si_q95     STi_q5    STi_q95
#> 1       V1 0.01720215 0.08130432 -0.07435191 0.1085802 0.07441833 0.08806782
#> 2       V2 0.11565972 0.29849936 -0.04906497 0.2705599 0.27274578 0.32764396
#> 3       V3 0.88827761 0.67981024  0.65305371 1.1462452 0.62228774 0.73518590
# Notice that total order indices have narrower confidence intervals than first order.
```
