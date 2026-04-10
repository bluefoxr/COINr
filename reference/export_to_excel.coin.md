# Export a coin to Excel

Exports the contents of the coin to Excel. This writes all data frames
inside the coin to Excel, with each data frame on a separate tab. Tabs
are named according to the position in the coin object. You can write
other data frames by simply attaching them to the coin object somewhere.

## Usage

``` r
# S3 method for class 'coin'
export_to_excel(x, fname = "coin_export.xlsx", include_log = FALSE, ...)
```

## Arguments

- x:

  A coin class object

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
## Here we write a COIN to Excel, but this is done to a temporary directory
## to avoid "polluting" the working directory when running automatic tests.
## In a real case, set fname to a directory of your choice.

# build example coin up to data treatment step
coin <- build_example_coin(up_to = "Treat")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Imputed
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Treated

# write to Excel in temporary directory
export_to_excel(coin, fname = paste0(tempdir(), "\\ASEM_results.xlsx"))

# spreadsheet is at:
print(paste0(tempdir(), "\\ASEM_results.xlsx"))
#> [1] "/tmp/Rtmp3Xy3ML\\ASEM_results.xlsx"

# now delete temporary file to keep things tidy in testing
unlink(paste0(tempdir(),"\\ASEM_results.xlsx"))
```
