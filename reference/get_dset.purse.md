# Gets a named data set and performs checks

A helper function to retrieve a named data set from a purse object.
Retrieves the specified data set from each coin in the purse and joins
them together in a single data frame using
[`rbind()`](https://rdrr.io/r/base/cbind.html), indexed with a `Time`
column.

## Usage

``` r
# S3 method for class 'purse'
get_dset(x, dset, Time = NULL, also_get = NULL, ...)
```

## Arguments

- x:

  A purse class object

- dset:

  A character string corresponding to a named data set within each coin
  `.$Data`. E.g. `"Raw"`.

- Time:

  Optional time index to extract from a subset of the coins present in
  the purse. Should be a vector containing one or more entries in
  `x$Time` or `NULL` to return all (default).

- also_get:

  A character vector specifying any columns to attach to the data set
  that are *not* indicators or aggregates. These will be e.g. `uName`,
  groups, denominators or columns labelled as "Other" in `iMeta`. These
  columns are stored in `.$Meta$Unit` to avoid repetition. Set
  `also_get = "all"` to attach all columns, or set `also_get = "none"`
  to return only numeric columns, i.e. no `uCode` column.

- ...:

  arguments passed to or from other methods.

## Value

Data frame of indicator data.

## Examples

``` r
# build example purse
purse <- build_example_purse(up_to = "new_coin", quietly = TRUE)

# get raw data set
df1 <- get_dset(purse, dset = "Raw")
```
