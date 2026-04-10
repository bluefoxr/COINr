# Copeland scores

Aggregates a data frame of indicator values into a single column using
the Copeland method. This function calls
[`outrankMatrix()`](https://bluefoxr.github.io/COINr/reference/outrankMatrix.md).

## Usage

``` r
a_copeland(X, w = NULL)
```

## Arguments

- X:

  A numeric data frame or matrix of indicator data, with observations as
  rows and indicators as columns. No other columns should be present
  (e.g. label columns).

- w:

  A numeric vector of weights, which should have length equal to
  `ncol(X)`. Weights are relative and will be re-scaled to sum to 1. If
  `w` is not specified, defaults to equal weights.

## Value

Numeric vector of Copeland scores.

## Details

The outranking matrix is transformed as follows:

- values \> 0.5 are replaced by 1

- values \< 0.5 are replaced by -1

- values == 0.5 are replaced by 0

- the diagonal of the matrix is all zeros

The Copeland scores are calculated as the row sums of this transformed
matrix.

This function replaces the now-defunct `copeland()` from COINr \< v1.0.

## Examples

``` r
# some example data
ind_data <- COINr::ASEM_iData[12:16]

# aggregate with vector of weights
outlist <- outrankMatrix(ind_data)
#> No weights specified for outranking matrix, using equal weights.
```
