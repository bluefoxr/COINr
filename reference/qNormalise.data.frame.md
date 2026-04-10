# Quick normalisation of a data frame

This is a wrapper function for
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md),
which offers a simpler syntax but less flexibility. It normalises a data
frame using a specified function `f_n` which is used to normalise each
column, with additional function arguments passed by `f_n_para`. By
default, `f_n = "n_minmax"` and `f_n_para` is set so that the columns of
`x` are normalised using the min-max method, between 0 and 100.

## Usage

``` r
# S3 method for class 'data.frame'
qNormalise(x, f_n = "n_minmax", f_n_para = NULL, directions = NULL, ...)
```

## Arguments

- x:

  A numeric data frame

- f_n:

  Name of a normalisation function (as a string) to apply to each column
  of `x`. Default `"n_minmax"`.

- f_n_para:

  Any further arguments to pass to `f_n`, as a named list. If
  `f_n = "n_minmax"`, this defaults to `list(l_u = c(0,100))` (scale
  between 0 and 100).

- directions:

  An optional data frame containing the following columns:

  - `iCode` The indicator code, corresponding to the column names of the
    data frame

  - `Direction` numeric vector with entries either `-1` or `1` If
    `directions` is not specified, the directions will all be assigned
    as `1`. Non-numeric columns do not need to have directions assigned.

- ...:

  arguments passed to or from other methods.

## Value

A normalised data frame

## Details

Essentially, this function is similar to
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
but brings parameters into the function arguments rather than being
wrapped in a list. It also does not allow individual normalisation.

See
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
documentation for more details, and
[`vignette("normalise")`](https://bluefoxr.github.io/COINr/articles/normalise.md).

## Examples

``` r
# some made up data
X <- data.frame(uCode = letters[1:10],
                a = runif(10),
                b = runif(10)*100)
# normalise (defaults to min-max)
qNormalise(X)
#>    uCode          a          b
#> 1      a  72.674546   0.000000
#> 2      b  54.294419 100.000000
#> 3      c  24.795524  63.509916
#> 4      d  26.664329  96.224015
#> 5      e   5.394857  36.498074
#> 6      f  60.895229  78.206968
#> 7      g 100.000000   3.645011
#> 8      h   0.000000   9.413429
#> 9      i  89.699042  57.508121
#> 10     j  91.257956  59.682932
```
