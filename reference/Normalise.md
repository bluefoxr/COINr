# Normalise data

This is a generic function for normalising variables and indicators,
i.e. bringing them onto a common scale. Please see individual method
documentation depending on your data class:

## Usage

``` r
Normalise(x, ...)
```

## Arguments

- x:

  Object to be normalised

- ...:

  Further arguments to be passed to methods.

## Details

- [`Normalise.numeric()`](https://bluefoxr.github.io/COINr/reference/Normalise.numeric.md)

- [`Normalise.data.frame()`](https://bluefoxr.github.io/COINr/reference/Normalise.data.frame.md)

- [`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)

- [`Normalise.purse()`](https://bluefoxr.github.io/COINr/reference/Normalise.purse.md)

See also
[`vignette("normalise")`](https://bluefoxr.github.io/COINr/articles/normalise.md)
for more details.

This function replaces the now-defunct `normalise()` from COINr \< v1.0.

## Examples

``` r
# See individual method documentation.
```
