# Regenerate a coin or purse

Methods for regenerating coins and purses. Regeneration is re-running
all the functions used to build the coin/purse, using the order and
parameters found in the `.$Log` list of the coin.

## Usage

``` r
Regen(x, from = NULL, quietly = TRUE)
```

## Arguments

- x:

  A coin or purse object to be regenerated

- from:

  Optional: a construction function name. If specified, regeneration
  begins from this function, rather than re-running all functions.

- quietly:

  If `TRUE` (default), messages are suppressed during building.

## Value

A regenerated object

## Details

Please see individual method documentation:

- [`Regen.coin()`](https://bluefoxr.github.io/COINr/reference/Regen.coin.md)

- [`Regen.purse()`](https://bluefoxr.github.io/COINr/reference/Regen.purse.md)

See also
[`vignette("adjustments")`](https://bluefoxr.github.io/COINr/articles/adjustments.md).

This function replaces the now-defunct `regen()` from COINr \< v1.0.

## Examples

``` r
# see individual method examples
```
