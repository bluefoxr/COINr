# Treat outliers

Generic function for treating outliers using a two-step process. See
individual method documentation:

## Usage

``` r
Treat(x, ...)
```

## Arguments

- x:

  Object to be treated

- ...:

  arguments passed to or from other methods.

## Value

Treated object plus details.

## Details

- [`Treat.numeric()`](https://bluefoxr.github.io/COINr/reference/Treat.numeric.md)

- [`Treat.data.frame()`](https://bluefoxr.github.io/COINr/reference/Treat.data.frame.md)

- [`Treat.coin()`](https://bluefoxr.github.io/COINr/reference/Treat.coin.md)

- [`Treat.purse()`](https://bluefoxr.github.io/COINr/reference/Treat.purse.md)

See also
[`vignette("treat")`](https://bluefoxr.github.io/COINr/articles/treat.md).

This function replaces the now-defunct `treat()` from COINr \< v1.0.
