# Normalise a numeric vector

Normalise a numeric vector using a specified function `f_n`, with
possible reversal of direction using `direction`.

## Usage

``` r
# S3 method for class 'numeric'
Normalise(x, f_n = NULL, f_n_para = NULL, direction = 1, ...)
```

## Arguments

- x:

  Object to be normalised

- f_n:

  The normalisation method, specified as string which refers to a
  function of the form `f_n(x, npara)`. See details. Defaults to
  `"n_minmax"` which is the min-max function.

- f_n_para:

  Supporting list of arguments for `f_n`. This is required to be a list.

- direction:

  If `direction = -1` the highest values of `x` will correspond to the
  lowest values of the normalised `x`. Else if `direction = 1` the
  direction of `x` in unaltered.

- ...:

  arguments passed to or from other methods.

## Value

A normalised numeric vector

## Details

Normalisation is specified using the `f_n` and `f_n_para` arguments. In
these, `f_n` should be a character string which is the name of a
normalisation function. For example, `f_n = "n_minmax"` calls the
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)
function. `f_n_para` is a list of any further arguments to `f_n`. This
means that any function can be passed to
[`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md),
as long as its first argument is `x`, a numeric vector, and it returns a
numeric vector of the same length. See
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)
for an example.

COINr has a number of built-in normalisation functions of the form
`n_*()`. See [online
documentation](https://bluefoxr.github.io/COINr/articles/normalise.html#built-in-normalisation-functions)
for details.

`f_n_para` is *required* to be a named list. So e.g. if we define a
function `f1(x, arg1, arg2)` then we should specify `f_n = "f1"`, and
`f_n_para = list(arg1 = val1, arg2 = val2)`, where `val1` and `val2` are
the values assigned to the arguments `arg1` and `arg2` respectively.

See also
[`vignette("normalise")`](https://bluefoxr.github.io/COINr/articles/normalise.md)
for more details.

## Examples

``` r
# example vector
x <- runif(10)

# normalise using distance to reference (5th data point)
x_norm <- Normalise(x, f_n = "n_dist2ref", f_n_para = list(iref = 5))

# view side by side
data.frame(x, x_norm)
#>             x     x_norm
#> 1  0.54248041 1.48647371
#> 2  0.54460339 1.49251443
#> 3  0.27859715 0.73562357
#> 4  0.44670247 1.21394835
#> 5  0.37151118 1.00000000
#> 6  0.02806097 0.02275103
#> 7  0.46598719 1.26882086
#> 8  0.39003139 1.05269716
#> 9  0.02006522 0.00000000
#> 10 0.37697093 1.01553509
```
