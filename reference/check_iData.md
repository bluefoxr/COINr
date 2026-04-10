# Check iData

Checks the format of `iData` input to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).
This check must be passed to successfully build a new coin.

## Usage

``` r
check_iData(iData, quietly = FALSE)
```

## Arguments

- iData:

  A data frame of indicator data.

- quietly:

  Set `TRUE` to suppress message if input is valid.

## Value

Message if everything ok, else error messages.

## Details

The restrictions on `iData` are not extensive. It should be a data frame
with only one required column `uCode` which gives the code assigned to
each unit (alphanumeric, not starting with a number). All other columns
are defined by corresponding entries in `iMeta`, with the following
special exceptions:

- `Time` is an optional column which allows panel data to be input,
  consisting of e.g. multiple rows for each `uCode`: one for each `Time`
  value. This can be used to split a set of panel data into multiple
  coins (a so-called "purse") which can be input to COINr functions. See
  [`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
  for more details.

- `uName` is an optional column which specifies a longer name for each
  unit. If this column is not included, unit codes (`uCode`) will be
  used as unit names where required.

No column names should contain blank spaces.

## Examples

``` r
check_iData(ASEM_iData)
#> iData checked and OK.
```
