# Denominate a data set within a purse.

This works in almost exactly the same way as
[`Denominate.coin()`](https://bluefoxr.github.io/COINr/reference/Denominate.coin.md).
The only point of care is that the `denoms` argument here cannot take
time-indexed data, but only a single value for each unit. It is
therefore recommended to pass the time-dependent denominator data as
part of `iData` when calling
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
In this way, denominators can vary with time. See
[`vignette("denomination")`](https://bluefoxr.github.io/COINr/articles/denomination.md).

## Usage

``` r
# S3 method for class 'purse'
Denominate(
  x,
  dset,
  denoms = NULL,
  denomby = NULL,
  denoms_ID = NULL,
  f_denom = NULL,
  write_to = NULL,
  ...
)
```

## Arguments

- x:

  A purse class object

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
  This column should contain uMeta codes to match with the data set
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

- ...:

  arguments passed to or from other methods.

## Value

An updated purse

## Examples

``` r
# build example purse
purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)

# denominate using data/specs already included in coin
purse <- Denominate(purse, dset = "Raw")
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Denominated

```
