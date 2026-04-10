# Regenerate a purse

Regenerates the `.$Data` entries in all coins by rerunning the
construction functions according to the specifications in `.$Log`, for
each coin in the purse. This effectively regenerates the results.

## Usage

``` r
# S3 method for class 'purse'
Regen(x, from = NULL, quietly = TRUE, ...)
```

## Arguments

- x:

  A purse class object

- from:

  Optional: a construction function name. If specified, regeneration
  begins from this function, rather than re-running all functions.

- quietly:

  If `TRUE` (default), messages are suppressed during building.

- ...:

  arguments passed to or from other methods.

## Value

Updated purse object with regenerated results.

## Details

The `from` argument allows partial regeneration, starting from a
specified function. This can be helpful to speed up regeneration in some
cases. However, keep in mind that if you change a `.$Log` argument from
a function that is run before the point that you choose to start running
from, it will not affect the results.

Note that for the moment, regeneration of purses is only partially
supported. This is because usually, in the normalisation step, it is
necessary to normalise across the full panel data set (see the `global`
argument in
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)).
At the moment, purse regeneration is performed by regenerating each coin
individually, but this does not allow for global normalisation which has
to be done at the purse level. This may be fixed in future releases.

See also documentation for
[`Regen.coin()`](https://bluefoxr.github.io/COINr/reference/Regen.coin.md)
and
[`vignette("adjustments")`](https://bluefoxr.github.io/COINr/articles/adjustments.md).

## Examples

``` r
# see examples from Regen.coin() and vignette("adjustments")
```
