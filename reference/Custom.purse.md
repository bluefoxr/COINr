# Custom operation

Custom operation on a purse. This is an experimental new feature.

## Usage

``` r
# S3 method for class 'purse'
Custom(
  x,
  dset,
  f_cust,
  f_cust_para = NULL,
  global = FALSE,
  write_to = NULL,
  ...
)
```

## Arguments

- x:

  A purse object

- dset:

  The data set to apply the operation to.

- f_cust:

  Function to apply to the data set. See details.

- f_cust_para:

  Optional additional parameters to pass to the function defined by
  `f_cust`.

- global:

  Logical: if `TRUE`, the entire data set, over all time points, is
  passed to the function `f_cust`. This is useful if the custom
  operation should be different for different time points, for example.
  Otherwise if `FALSE`, passes the data set within each coin one at a
  time to `f_cust`.

- write_to:

  Name of data set to write to

- ...:

  Arguments to pass to/from other methods.

## Value

An updated purse.

## Details

In this function, the data set named `dset` is extracted from the coin
using `get_dset(purse, dset)`. It is passed to the function `f_cust`,
which is required to return an equivalent but modified data frame, which
is then written as a new data set with name `write_to`. This is intended
to allow arbitrary operations on coin data sets while staying within the
COINr framework, which means that if
[`Regen()`](https://bluefoxr.github.io/COINr/reference/Regen.md) is
used, these operations will be re-run, allowing them to be included in
things like sensitivity analysis.

The format of `f_cust` is important. It must be a function whose first
argument is called `x`: this will be the argument that the data is
passed to. The data will be in the same format as extracted via
`get_dset(purse, dset)`, which means it will have `uCode` and `Time`
columns. `f_cust` can have other arguments which are passed to it via
`f_cust_para`. The function should return a data frame similar to the
data that was passed to it, it must contain have the same column names
(meaning you can't remove indicators), but otherwise is flexible - this
means some caution is necessary to ensure that subsequent operations
don't fail. Be careful, for example, to ensure that there are no
duplicates in `uCode`, and that indicator columns are numeric.

The function assigned to `f_cust` is passed to
[`base::do.call()`](https://rdrr.io/r/base/do.call.html), therefore it
can be passed either as a string naming the function, or as the function
itself. Depending on the context, the latter option may be preferable
because this stores the function within the coin, which makes it
portable. Otherwise, if the function is simply named as a string, you
must make sure it is available to access in the environment.

## Examples

``` r
# build example purse
purse <- build_example_purse(up_to = "new_coin")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw

# custom function - set points before 2020 to NA for BEL in FDI due to a
# break in the series
f_cust <- function(x){x[(x$uCode == "BEL") & (x$Time < 2020), "FDI"] <- NA;
                      return(x)}


```
