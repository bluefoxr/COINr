# Screen units based on data availability

Screens units (rows) based on a data availability threshold and presence
of zeros. Units can be optionally "forced" to be included or excluded,
making exceptions for the data availability threshold.

## Usage

``` r
# S3 method for class 'data.frame'
Screen(
  x,
  id_col = NULL,
  unit_screen,
  dat_thresh = NULL,
  nonzero_thresh = NULL,
  Force = NULL,
  ...
)
```

## Arguments

- x:

  A data frame

- id_col:

  Name of column of the data frame to be used as the identifier, e.g.
  normally this would be `uCode` for indicator data sets used in coins.
  This must be specified if `Force` is specified.

- unit_screen:

  Specifies whether and how to screen units based on data availability
  or zero values.

  - If set to `"byNA"`, screens units with data availability below
    `dat_thresh`

  - If set to `"byzeros"`, screens units with non-zero values below
    `nonzero_thresh`

  - If set to `"byNAandzeros"`, screens units based on either of the
    previous two criteria being true.

- dat_thresh:

  A data availability threshold (`>= 1` and `<= 0`) used for flagging
  low data and screening units if `unit_screen != "none"`. Default 0.66.

- nonzero_thresh:

  As `dat_thresh` but for non-zero values. Defaults to 0.05, i.e. it
  will flag any units with less than 5% non-zero values (equivalently
  more than 95% zero values).

- Force:

  A data frame with any additional units to force inclusion or
  exclusion. Required columns `uCode` (unit code(s)) and `Include`
  (logical: `TRUE` to include and `FALSE` to exclude). Specifications
  here override exclusion/inclusion based on data rules.

- ...:

  arguments passed to or from other methods.

## Value

Missing data stats and screened data as a list.

## Details

The two main criteria of interest are `NA` values, and zeros. The
summary table gives percentages of `NA` values for each unit, across
indicators, and percentage zero values (*as a percentage of non-`NA`
values*). Each unit is flagged as having low data or too many zeros
based on thresholds.

See also
[`vignette("screening")`](https://bluefoxr.github.io/COINr/articles/screening.md).

## Examples

``` r
# example data
iData <- ASEM_iData[40:51, c("uCode", "Research", "Pat", "CultServ", "CultGood")]

# screen to 75% data availability (by row)
l_scr <- Screen(iData, unit_screen = "byNA", dat_thresh = 0.75)

# summary of screening
head(l_scr$DataSummary)
#>    uCode N_missing N_zero N_miss_or_zero Dat_Avail Non_Zero LowData LowNonZero
#> 40   KOR         1      0              1      0.75        1   FALSE      FALSE
#> 41   LAO         3      0              3      0.25        1    TRUE      FALSE
#> 42   MYS         0      0              0      1.00        1   FALSE      FALSE
#> 43   MNG         0      0              0      1.00        1   FALSE      FALSE
#> 44   MMR         2      0              2      0.50        1    TRUE      FALSE
#> 45   NZL         0      0              0      1.00        1   FALSE      FALSE
#>    LowDatOrZeroFlag Included
#> 40            FALSE     TRUE
#> 41             TRUE    FALSE
#> 42            FALSE     TRUE
#> 43            FALSE     TRUE
#> 44             TRUE    FALSE
#> 45            FALSE     TRUE
```
