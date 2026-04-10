# Outranking matrix

Constructs an outranking matrix based on a data frame of indicator data
and corresponding weights.

## Usage

``` r
outrankMatrix(X, w = NULL)
```

## Arguments

- X:

  A data frame or matrix of indicator data, with observations as rows
  and indicators as columns. No other columns should be present (e.g.
  label columns).

- w:

  A vector of weights, which should have length equal to `ncol(X)`.
  Weights are relative and will be re-scaled to sum to 1. If `w` is not
  specified, defaults to equal weights.

## Value

A list with:

- `.$OutRankMatrix` the outranking matrix with `nrow(X)` rows and
  columns (matrix class).

- `.$nDominant` the number of dominance/robust pairs

- `.$fracDominant` the percentage of dominance/robust pairs

## Examples

``` r
# get a sample of a few indicators
ind_data <- COINr::ASEM_iData[12:16]
# calculate outranking matrix
outlist <- outrankMatrix(ind_data)
#> No weights specified for outranking matrix, using equal weights.
# see fraction of dominant pairs (robustness)
outlist$fracDominant
#> [1] 0.2533333
```
