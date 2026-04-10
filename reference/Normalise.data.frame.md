# Normalise a data frame

Normalises a data frame using specifications specified in
`global_specs`. Columns can also optionally be normalised with
individual specifications using the `indiv_specs` argument. If variables
should have their directions reversed, this can be specified using the
`directions` argument. Non-numeric columns are ignored automatically by
this function. By default, this function normalises each indicator using
the "min-max" method, scaling indicators to lie between 0 and 100. This
calls the
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)
function. COINr has a number of built-in normalisation functions of the
form `n_*()`. See [online
documentation](https://bluefoxr.github.io/COINr/articles/normalise.html#built-in-normalisation-functions)
for details.

## Usage

``` r
# S3 method for class 'data.frame'
Normalise(x, global_specs = NULL, indiv_specs = NULL, directions = NULL, ...)
```

## Arguments

- x:

  A data frame

- global_specs:

  Specifications to apply to all columns, apart from those specified by
  `indiv_specs`. See details.

- indiv_specs:

  Specifications applied to specific columns, overriding those specified
  in `global_specs`. See details.

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

### Global specification

The `global_specs` argument is a list which specifies the normalisation
function and any function parameters that should be used to normalise
the columns of `x`. Unless `indiv_specs` is specified, this will be
applied to all numeric columns of `x`. The list should have two entries:

- `.$f_n`: the name of the function to use to normalise each column

- `.$f_n_para`: any further parameters to pass to `f_n`, apart from the
  numeric vector (each column of `x`)

In this list, `f_n` should be a character string which is the name of a
normalisation function. For example, `f_n = "n_minmax"` calls the
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)
function. `f_n_para` is a list of any further arguments to `f_n`. This
means that any function can be passed to
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md),
as long as its first argument is `x`, a numeric vector, and it returns a
numeric vector of the same length. See
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)
for an example.

`f_n_para` is *required* to be a named list. So e.g. if we define a
function `f1(x, arg1, arg2)` then we should specify `f_n = "f1"`, and
`f_n_para = list(arg1 = val1, arg2 = val2)`, where `val1` and `val2` are
the values assigned to the arguments `arg1` and `arg2` respectively.

The default list for `global_specs` is:
`list(f_n = "n_minmax", f_n_para = list(l_u = c(0,100)))`.

Note, all COINr normalisation functions (passed to `f_n`) are of the
form `n_*()`. Type `n_` in the R Studio console and press the Tab key to
see a list.

### Individual column specification

Optionally, columns of `x` can be normalised with different
normalisation functions and parameters using the `indiv_specs` argument.
This must be specified as a named list e.g.
`list(i1 = specs1, i2 = specs2)` where `i1` and `i2` are column names of
`x` to apply individual normalisation to, and `specs1` and `specs2` are
respectively lists of the same format as `global_specs` (see above). In
other words, `indiv_specs` is a big list wrapping together
`global_specs`-style lists. Any numeric columns of `x` not named in
`indiv_specs` ( i.e. those not in `names(indiv_specs)`) are normalised
using the specifications from `global_specs`. So `indiv_specs` lists the
exceptions to `global_specs`.

See also
[`vignette("normalise")`](https://bluefoxr.github.io/COINr/articles/normalise.md)
for more details.

## Examples

``` r
iris_norm <- Normalise(iris)
head(iris_norm)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1    22.222222    62.50000     6.779661    4.166667  setosa
#> 2    16.666667    41.66667     6.779661    4.166667  setosa
#> 3    11.111111    50.00000     5.084746    4.166667  setosa
#> 4     8.333333    45.83333     8.474576    4.166667  setosa
#> 5    19.444444    66.66667     6.779661    4.166667  setosa
#> 6    30.555556    79.16667    11.864407   12.500000  setosa
```
