# Get subsets of indicator data

A helper function to retrieve a named data set from coin or purse
objects. See individual method documentation:

## Usage

``` r
get_data(x, ...)
```

## Arguments

- x:

  A coin or purse

- ...:

  Arguments passed to methods

## Value

Data frame of indicator data, indexed also by time if input is a purse.

## Details

- [`get_data.coin()`](https://bluefoxr.github.io/COINr/reference/get_data.coin.md)

- [`get_data.purse()`](https://bluefoxr.github.io/COINr/reference/get_data.purse.md)

This function replaces the now-defunct `getIn()` from COINr \< v1.0.

## Examples

``` r
# see individual method documentation
```
