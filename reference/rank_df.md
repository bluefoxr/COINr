# Convert a data frame to ranks

Replaces all numerical columns of a data frame with their ranks. Uses
sport ranking, i.e. ties share the highest rank place. Ignores
non-numerical columns. See [`rank()`](https://rdrr.io/r/base/rank.html).
Optionally, returns in-group ranks using a specified grouping column.

## Usage

``` r
rank_df(df, use_group = NULL)
```

## Arguments

- df:

  A data frame

- use_group:

  An optional column of df (specified as a string) to use as a grouping
  variable. If specified, returns ranks inside each group present in
  this column.

## Value

A data frame equal to the data frame that was input, but with any
numerical columns replaced with ranks.

## Details

This function replaces the now-defunct `rankDF()` from COINr \< v1.0.

## Examples

``` r
# some random data, with a column of characters
df <- data.frame(RName = c("A", "B", "C"),
Score1 = runif(3), Score2 = runif(3))
# convert to ranks
rank_df(df)
#>   RName Score1 Score2
#> 1     A      1      3
#> 2     B      3      2
#> 3     C      2      1
# grouped ranking - use some example data
df1 <- ASEM_iData[c("uCode", "GDP_group", "Goods", "LPI")]
rank_df(df1, use_group = "GDP_group")
#>    uCode GDP_group Goods LPI
#> 1    AUT         L     7   5
#> 2    BEL         L     2   4
#> 3    BGR         S     2  10
#> 4    HRV         S     5   6
#> 5    CYP         S    11   8
#> 6    CZE         M     1   2
#> 7    DNK         L    10   7
#> 8    EST         S     7   3
#> 9    FIN         M     7   1
#> 10   FRA        XL     3   4
#> 11   DEU        XL     1   1
#> 12   GRC         M     8   7
#> 13   HUN         M     3   3
#> 14   IRL         L    12   8
#> 15   ITA        XL     6   6
#> 16   LVA         S     6   4
#> 17   LTU         S     3   2
#> 18   LUX         S     4   1
#> 19   MLT         S     9   7
#> 20   NLD         L     1   2
#> 21   NOR         L    11   9
#> 22   POL         L     5  11
#> 23   PRT         M     6   4
#> 24   ROU         M     5   8
#> 25   SVK         M     4   6
#> 26   SVN         S     1   5
#> 27   ESP        XL     8   7
#> 28   SWE         L     9   1
#> 29   CHE         L     4   6
#> 30   GBR        XL     4   2
#> 31   AUS        XL    11   5
#> 32   BGD         M     9  12
#> 33   BRN         S    13   9
#> 34   KHM         S     8  11
#> 35   CHN        XL     2   9
#> 36   IND        XL    10  10
#> 37   IDN        XL    12  11
#> 38   JPN        XL     5   3
#> 39   KAZ         M    10  11
#> 40   KOR        XL     7   8
#> 41   LAO         S    10  13
#> 42   MYS         L     8  10
#> 43   MNG         S    12  12
#> 44   MMR         M    13  13
#> 45   NZL         M    11   5
#> 46   PAK         M    12  10
#> 47   PHL         L    13  13
#> 48   RUS        XL     9  12
#> 49   SGP         L     3   3
#> 50   THA         L     6  12
#> 51   VNM         M     2   9
```
