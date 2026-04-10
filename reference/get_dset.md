# Gets a named data set and performs checks

A helper function to retrieve a named data set from coin or purse
objects. See individual documentation on:

## Usage

``` r
get_dset(x, dset, ...)
```

## Arguments

- x:

  A coin or purse

- dset:

  A character string corresponding to a named data set within `.$Data`.
  E.g. `"Raw"`.

- ...:

  arguments passed to or from other methods.

## Value

Data frame of indicator data, indexed also by time if input is a purse.

## Details

- [`get_dset.coin()`](https://bluefoxr.github.io/COINr/reference/get_dset.coin.md)

- [`get_dset.purse()`](https://bluefoxr.github.io/COINr/reference/get_dset.purse.md)

## Examples

``` r
# see examples for methods
```
