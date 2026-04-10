# Compare two data frames

A custom function for comparing two data frames of indicator data, to
see whether they match up, at a specified number of significant figures.
Specifically, this is intended to compare two data frames, without
regard to row or column ordering. Rows are matched by the required
`matchcol` argument. Hence, it is different from e.g.
[`all.equal()`](https://rdrr.io/r/base/all.equal.html) which requires
rows to be ordered. In COINr, typically `matchcol` is the `uCode`
column, for example.

## Usage

``` r
compare_df(df1, df2, matchcol, sigfigs = 5)
```

## Arguments

- df1:

  A data frame

- df2:

  Another data frame

- matchcol:

  A common column name that is used to match row order. E.g. this might
  be `uCode`.

- sigfigs:

  The number of significant figures to use for matching numerical
  columns

## Value

A list with comparison results. List contains:

- `.$Same`: overall summary: if `TRUE` the data frames are the same
  according to the rules specified, otherwise `FALSE`.

- `.$Details`: details of each column as a data frame. Each row
  summarises a column of the data frame, saying whether the column is
  the same as its equivalent, and the number of differences, if any. In
  case the two data frames have differing numbers of columns and rows,
  or have differing column names or entries in `matchcol`, `.$Details`
  will simply contain a message to this effect.

- `.$Differences`: a list with one entry for every column which contains
  different entries. Differences are summarised as a data frame with one
  row for each difference, reporting the value from `df1` and its
  equivalent from `df2`.

## Details

This function compares numerical and non-numerical columns to see if
they match. Rows and columns can be in any order. The function performs
the following checks:

- Checks that the two data frames are the same size

- Checks that column names are the same, and that the matching column
  has the same entries

- Checks column by column that the elements are the same, after sorting
  according to the matching column

It then summarises for each column whether there are any differences,
and also what the differences are, if any.

This is intended to cross-check results. For example, if you run
something in COINr and want to check indicator results against external
calculations.

This function replaces the now-defunct `compareDF()` from COINr \< v1.0.

## Examples

``` r
# take a sample of indicator data (including the uCode column)
data1 <- ASEM_iData[c(2,12:15)]
# copy the data
data2 <- data1
# make a change: replace one value in data2 by NA
data2[1,2] <- NA
# compare data frames
compare_df(data1, data2, matchcol = "uCode")
#> $Same
#> [1] FALSE
#> 
#> $Details
#>    Column TheSame                          Comment NDifferent
#> 1   uCode    TRUE      Non-numerical and identical          0
#> 2     LPI   FALSE Numerical and different at 5 sf.          1
#> 3 Flights    TRUE Numerical and identical to 5 sf.          0
#> 4    Ship    TRUE Numerical and identical to 5 sf.          0
#> 5    Bord    TRUE Numerical and identical to 5 sf.          0
#> 
#> $Differences
#> $Differences$LPI
#>   uCode      df1 df2
#> 1   AUT 4.097985  NA
#> 
#> 
```
