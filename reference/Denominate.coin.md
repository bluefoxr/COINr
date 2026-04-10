# Denominate data set in a coin

"Denominates" or "scales" indicators by other variables. Typically this
is done by dividing extensive variables such as GDP by a scaling
variable such as population, to give an intensive variable (GDP per
capita).

## Usage

``` r
# S3 method for class 'coin'
Denominate(
  x,
  dset,
  denoms = NULL,
  denomby = NULL,
  denoms_ID = NULL,
  f_denom = NULL,
  write_to = NULL,
  out2 = "coin",
  ...
)
```

## Arguments

- x:

  A coin class object

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- denoms:

  An optional data frame of denominator data. Columns should be
  denominator data, with column names corresponding to entries in
  `denomby`. This must also include an ID column identified by
  `denoms_ID` to match rows. If `denoms` is not specified, will extract
  any potential denominator columns that were attached to `iData` when
  calling
  [`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).

- denomby:

  Optional data frame which specifies which denominators to use for each
  indicator, and any scaling factors to apply. Should have columns
  `iCode`, `Denominator`, `ScaleFactor`. `iCode` specifies an indicator
  code found in `dset`, `Denominator` specifies a column name from
  `denoms` to use to denominate the corresponding column from `x`.
  `ScaleFactor` allows the possibility to scale denominators if needed,
  and specifies a factor to multiply the resulting values by. For
  example, if GDP is a denominator and is measured in dollars, dividing
  will create very small numbers (order 1e-10 and smaller) which could
  cause problems with numerical precision. If `denomby` is not
  specified, specifications will be taken from the "Denominator" column
  in `iMeta`, if it exists.

- denoms_ID:

  An ID column for matching `denoms` with the data to be denominated.
  This column should contain `uMeta` codes to match with the data set
  extracted from the coin.

- f_denom:

  A function which takes two numeric vector arguments and is used to
  perform the denomination for each column. By default, this is
  division, i.e. `x[[col]]/denoms[[col]]` for given columns, but any
  function can be passed that takes two numeric vectors as inputs and
  returns a single numeric vector. See details.

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Denominated"`.

- out2:

  Either `"coin"` (default) to return updated coin or `"df"` to output
  the aggregated data set.

- ...:

  arguments passed to or from other methods

## Value

An updated coin if `out2 = "coin"`, else a data frame of denominated
data if `out2 = "df"`.

## Details

This function denominates a data set `dset` inside the coin. By default,
denominating variables are taken from the coin, specifically as
variables in `iData` with `Type = "Denominator"` in `iMeta` (input to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)).
Specifications to map denominators to indicators are also taken by
default from `iMeta$Denominator`, if it exists.

These specifications can be overridden using the `denoms` and `denomby`
arguments. The operator for denomination can also be changed using the
`f_denom` argument.

See also documentation for
[`Denominate.data.frame()`](https://bluefoxr.github.io/COINr/reference/Denominate.data.frame.md)
which is called by this method.

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# denominate (here, we only need to say which dset to use, takes
# specs and denominators from within the coin)
coin <- Denominate(coin, dset = "Raw")
#> Written data set to .$Data$Denominated
```
