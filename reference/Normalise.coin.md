# Create a normalised data set

Creates a normalised data set using specifications specified in
`global_specs`. Columns of `dset` can also optionally be normalised with
individual specifications using the `indiv_specs` argument. If
indicators should have their directions reversed, this can be specified
using the `directions` argument. Non-numeric columns are ignored
automatically by this function. By default, this function normalises
each indicator using the "min-max" method, scaling indicators to lie
between 0 and 100. This calls the
[`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)
function. COINr has a number of built-in normalisation functions of the
form `n_*()`. See [online
documentation](https://bluefoxr.github.io/COINr/articles/normalise.html#built-in-normalisation-functions)
for details.

## Usage

``` r
# S3 method for class 'coin'
Normalise(
  x,
  dset,
  global_specs = NULL,
  indiv_specs = NULL,
  directions = NULL,
  out2 = "coin",
  write_to = NULL,
  write2log = TRUE,
  ...
)
```

## Arguments

- x:

  A coin

- dset:

  A named data set found in `.$Data`

- global_specs:

  Specifications to apply to all columns, apart from those specified by
  `indiv_specs`. See details.

- indiv_specs:

  Specifications applied to specific columns, overriding those specified
  in `global_specs`. See details.

- directions:

  An optional data frame containing the following columns:

  - `iCode` The indicator code, corresponding to the column names of the
    data set

  - `Direction` numeric vector with entries either `-1` or `1` If
    `directions` is not specified, the directions will be taken from the
    `iMeta` table in the coin, if available.

- out2:

  Either `"coin"` to return normalised data set back to the coin, or
  `df` to simply return a data frame.

- write_to:

  Optional character string for naming the data set in the coin. Data
  will be written to `.$Data[[write_to]]`. Default is
  `write_to == "Normalised"`.

- write2log:

  Logical: if `FALSE`, the arguments of this function are not written to
  the coin log, so this function will not be invoked when regenerating.
  Recommend to keep `TRUE` unless you have a good reason to do
  otherwise.

- ...:

  arguments passed to or from other methods.

## Value

An updated coin

## Details

### Global specification

The `global_specs` argument is a list which specifies the normalisation
function and any function parameters that should be used to normalise
the indicators found in the data set. Unless `indiv_specs` is specified,
this will be applied to all indicators. The list should have two
entries:

- `.$f_n`: the name of the function to use to normalise each indicator

- `.$f_n_para`: any further parameters to pass to `f_n`, apart from the
  numeric vector (each column of the data set)

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
`list(f_n = "n_minmax", f_n_para = list(l_u = c(0,100)))`, i.e. min-max
normalisation between 0 and 100.

Note, all COINr normalisation functions (passed to `f_n`) are of the
form `n_*()`. Type `n_` in the R Studio console and press the Tab key to
see a list.

### Individual parameter specification with iMeta

For some normalisation methods we may use the same function for all
indicators but use different parameters - for example, using distance to
target normalisation or goalpost normalisation. COINr now supports
specifying these parameters in the `iMeta` table. To enable this, set
`f_n_para = "use_iMeta"` within the `global_specs` list.

For this to work you will also need to add the correct-named columns in
the `iMeta` table. To see which column names to add, check the function
documentation of the normalisation function you wish to use (e.g.
[`n_goalposts()`](https://bluefoxr.github.io/COINr/reference/n_goalposts.md)).
See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).
These columns should be added before construction of the coin.

### Individual column specification

To give full individual control, indicators can be normalised with
different normalisation functions and parameters using the `indiv_specs`
argument. This must be specified as a named list e.g.
`list(i1 = specs1, i2 = specs2)` where `i1` and `i2` are `iCode`s to
apply individual normalisation to, and `specs1` and `specs2` are
respectively lists of the same format as `global_specs` (see above). In
other words, `indiv_specs` is a big list wrapping together
`global_specs`-style lists. Any `iCode`s not named in `indiv_specs` (
i.e. those not in `names(indiv_specs)`) are normalised using the
specifications from `global_specs`. So `indiv_specs` lists the
exceptions to `global_specs`.

See also
[`vignette("normalise")`](https://bluefoxr.github.io/COINr/articles/normalise.md)
for more details.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw

# normalise the raw data set
coin <- Normalise(coin, dset = "Raw")
#> Written data set to .$Data$Normalised
```
