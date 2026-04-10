# Get subsets of indicator data

A flexible function for retrieving data from a coin, from a specified
data set. Subsets of data can be returned based on selection of columns,
using the `iCodes` and `Level` arguments, and by filtering rowwise using
the `uCodes` and `use_group` arguments. The `also_get` argument also
allows unit metadata columns to be attached, such as names, groups, and
denominators.

## Usage

``` r
# S3 method for class 'coin'
get_data(
  x,
  dset,
  iCodes = NULL,
  Level = NULL,
  uCodes = NULL,
  use_group = NULL,
  also_get = NULL,
  ...
)
```

## Arguments

- x:

  A coin class object

- dset:

  The name of the data set to apply the function to, which should be
  accessible in `.$Data`.

- iCodes:

  Optional indicator codes to retrieve. If `NULL` (default), returns all
  iCodes found in the selected data set. Can also refer to indicator
  groups. See details.

- Level:

  Optionally, the level in the hierarchy to extract data from. See
  details.

- uCodes:

  Optional unit codes to filter rows of the resulting data set. Can also
  be used in conjunction with groups. See details.

- use_group:

  Optional group to filter rows of the data set. Specified as
  `list(Group_Var = Group)`, where `Group_Var` is a Group\_ column that
  must be present in the selected data set, and `Group` is a specified
  group inside that grouping variable. This filters the selected data to
  only include rows from the specified group. Can also be used in
  conjunction with `uCodes` – see details.

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

A data frame of indicator data according to specifications.

## Details

The `iCodes` argument can be used to directly select named indicators,
i.e. setting `iCodes = c("a", "b")` will select indicators "a" and "b",
attaching any extra columns specified by `also_get`. However, using this
in conjunction with the `Level` argument returns named groups of
indicators. For example, setting `iCodes = "Group1"` (for e.g. an
aggregation group in Level 2) and `Level = 1` will return all indicators
in Level 1, belonging to "Group1".

Rows can also be subsetted. The `uCodes` argument can be used to select
specified units in the same way as `iCodes`. Additionally, the
`use_group` argument filters to specified groups. If `uCodes` is
specified, and `use_group` refers to a named group column, then it will
return all units in the groups that the `uCodes` belong to. This is
useful for putting a unit into context with its peers based on some
grouping variable.

Note that if you want to retrieve a whole data set (with no column/row
subsetting), use the
[`get_dset()`](https://bluefoxr.github.io/COINr/reference/get_dset.md)
function which should be slightly faster.

## Examples

``` r
# build full example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# get all indicators in "Political group
x <- get_data(coin, dset = "Raw", iCodes = "Political", Level = 1)
head(x, 5)
#>    uCode Embs IGOs   UNVote
#> 31   AUS   82  196 38.46245
#> 1    AUT   88  227 42.63920
#> 2    BEL   84  248 43.00308
#> 32   BGD   52  145 38.60601
#> 3    BGR   67  209 42.95986

# see vignette("data_selection") for more examples
```
