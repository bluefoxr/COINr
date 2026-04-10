# Get data availability of units

Returns a list of data frames: the data availability of each unit (row)
in a given data set, as well as percentage of zeros. A second data frame
gives data availability by aggregation (indicator) groups.

## Usage

``` r
# S3 method for class 'coin'
get_data_avail(x, dset, out2 = "coin", ...)
```

## Arguments

- x:

  A coin

- dset:

  String indicating name of data set in `.$Data`.

- out2:

  Either `"coin"` to output an updated coin or `"list"` to output a
  list.

- ...:

  arguments passed to or from other methods.

## Value

An updated coin with data availability tables written in
`.$Analysis[[dset]]`, or a list of data availability tables.

## Details

This function ignores any non-numeric columns, and returns a data
availability table of numeric columns with non-numeric columns appended
at the beginning.

See also vignettes:
[`vignette("analysis")`](https://bluefoxr.github.io/COINr/articles/analysis.md)
and
[`vignette("imputation")`](https://bluefoxr.github.io/COINr/articles/imputation.md).

## Examples

``` r
# build example coin
coin <-  build_example_coin(up_to = "new_coin", quietly = TRUE)

# get data availability of Raw dset
l_dat <- get_data_avail(coin, dset = "Raw", out2 = "list")
head(l_dat$Summary, 5)
#>    uCode N_missing N_zero N_miss_or_zero Dat_Avail  Non_Zero
#> 31   AUS         0      3              3  1.000000 0.9387755
#> 1    AUT         0      2              2  1.000000 0.9591837
#> 2    BEL         0      2              2  1.000000 0.9591837
#> 32   BGD         6      1              7  0.877551 0.9767442
#> 3    BGR         0      0              0  1.000000 1.0000000
```
