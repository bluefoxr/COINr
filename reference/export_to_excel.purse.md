# Export a purse to Excel

Exports the contents of the purse to Excel. This is similar to the coin
method
[`export_to_excel.coin()`](https://bluefoxr.github.io/COINr/reference/export_to_excel.coin.md),
but combines data sets from various time points. It also selectively
writes metadata since this may be spread across multiple coins.

## Usage

``` r
# S3 method for class 'purse'
export_to_excel(x, fname = "coin_export.xlsx", include_log = FALSE, ...)
```

## Arguments

- x:

  A purse class object

- fname:

  The file name/path to write to, as a character string

- include_log:

  Logical: if `TRUE`, also writes data frames from the `.$Log` list
  inside the coin.

- ...:

  arguments passed to or from other methods.

## Value

.xlsx file at specified path

## Examples

``` r
#
```
