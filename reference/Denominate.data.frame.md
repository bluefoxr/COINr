# Denominate data sets by other variables

"Denominates" or "scales" variables by other variables. Typically this
is done by dividing extensive variables such as GDP by a scaling
variable such as population, to give an intensive variable (GDP per
capita).

## Usage

``` r
# S3 method for class 'data.frame'
Denominate(
  x,
  denoms,
  denomby,
  x_ID = NULL,
  denoms_ID = NULL,
  f_denom = NULL,
  ...
)
```

## Arguments

- x:

  A data frame of data to be denominated. Columns to be denominated must
  be numeric, but any columns not specified in `denomby` will be
  ignored. `x` must also contain an ID column specified by `x_ID` to
  match rows with `denoms`.

- denoms:

  A data frame of denominator data. Columns should be denominator data,
  with column names corresponding to entries in `denomby`. This must
  also include an ID column identified by `denoms_ID` to match rows.

- denomby:

  A data frame which specifies which denominators to use for each
  indicator, and any scaling factors to apply. Should have columns
  `iCode`, `Denominator`, `ScaleFactor`. `iCode` specifies a column name
  from `x`, `Denominator` specifies a column name from `denoms` to use
  to denominate the corresponding column from `x`. `ScaleFactor` allows
  the possibility to scale denominators if needed, and specifies a
  factor to multiply the resulting values by. For example, if GDP is a
  denominator and is measured in dollars, dividing will create very
  small numbers (order 1e-10 and smaller) which could cause problems
  with numerical precision.

- x_ID:

  A column name of `x` to use to match rows with `denoms`. Default is
  `"uCode"`.

- denoms_ID:

  A column name of `denoms` to use to match rows with `x`. Default is
  `"uCode"`.

- f_denom:

  A function which takes two numeric vector arguments and is used to
  perform the denomination for each column. By default, this is
  division, i.e. `x[[col]]/denoms[[col]]` for given columns, but any
  function can be passed that takes two numeric vectors as inputs and
  returns a single numeric vector. See details.

- ...:

  arguments passed to or from other methods.

## Value

A data frame of the same size as `x`, with any specified columns
denominated according to specifications.

## Details

A data frame `x` is denominated by variables found in another data frame
`denoms`, according to specifications in `denomby`. `denomby` specifies
which columns in `x` are to be denominated, and by which columns in
`denoms`, and any scaling factors to apply to each denomination.

Both `x` and `denomby` must contain an ID column which matches the rows
of `x` to `denomby`. If not specified, this is assumed to be `uCode`,
but can also be specified using the `x_ID` and `denoms_ID` arguments.
All entries in `x[[x_ID]]` must be present in `denoms[[denoms_ID]]`,
although extra rows are allowed in `denoms`. This is because the rows of
`x` are matched to the rows of `denoms` using these ID columns, to
ensure that units (rows) are correctly denominated.

By default, columns of `x` are divided by columns of `denoms`. This can
be generalised by setting `f_denom` to another function which takes two
numeric vector arguments. I.e. setting ``` denoms = ``*``  ``` will
multiply columns of `x` and denoms together.

## See also

- [WorldDenoms](https://bluefoxr.github.io/COINr/reference/WorldDenoms.md)
  A data set of some common national-level denominators.

## Examples

``` r
# Get a sample of indicator data (note must be indicators plus a "UnitCode" column)
iData <- ASEM_iData[c("uCode", "Goods", "Flights", "LPI")]
# Also get some denominator data
denoms <- ASEM_iData[c("uCode", "GDP", "Population")]
# specify how to denominate
denomby <- data.frame(iCode = c("Goods", "Flights"),
Denominator = c("GDP", "Population"),
ScaleFactor = c(1, 1000))
# Denominate one by the other
iData_den <- Denominate(iData, denoms, denomby)
```
