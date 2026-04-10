# Screen units based on data availability

Screens units based on a data availability threshold and presence of
zeros. Units can be optionally "forced" to be included or excluded,
making exceptions for the data availability threshold.

## Usage

``` r
# S3 method for class 'coin'
Screen(
  x,
  dset,
  unit_screen,
  dat_thresh = NULL,
  nonzero_thresh = NULL,
  Force = NULL,
  out2 = "coin",
  write_to = NULL,
  ...
)
```

## Arguments

- x:

  A coin

- dset:

  The data set to be checked/screened

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

  A data frame with any additional countries to force inclusion or
  exclusion. Required columns `uCode` (unit code(s)) and `Include`
  (logical: `TRUE` to include and `FALSE` to exclude). Specifications
  here override exclusion/inclusion based on data rules.

- out2:

  Where to output the results. If `"COIN"` (default for COIN input),
  appends to updated COIN, otherwise if `"list"` outputs to data frame.

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Screened"`.

- ...:

  arguments passed to or from other methods.

## Value

An updated coin with data frames showing missing data in `.$Analysis`,
and a new data set `.$Data$Screened`. If `out2 = "list"` wraps missing
data stats and screened data set into a list.

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
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# screen units from raw dset
coin <- Screen(coin, dset = "Raw", unit_screen = "byNA",
               dat_thresh = 0.85, write_to = "Filtered_85pc")
#> Written data set to .$Data$Filtered_85pc

# some details about the coin by calling its print method
coin
#> --------------
#> A coin with...
#> --------------
#> Input:
#>   Units: 51 (AUS, AUT, BEL, ...)
#>   Indicators: 49 (Goods, Services, FDI, ...)
#>   Denominators: 4 (Area, Energy, GDP, ...)
#>   Groups: 4 (GDP_group, GDPpc_group, Pop_group, ...)
#> 
#> Structure:
#>   Level 1 Indicator: 49 indicators (FDI, ForPort, Goods, ...) 
#>   Level 2 Pillar: 8 groups (ConEcFin, Instit, P2P, ...) 
#>   Level 3 Sub-index: 2 groups (Conn, Sust) 
#>   Level 4 Index: 1 groups (Index) 
#> 
#> Data sets:
#>   Raw (51 units)
#>   Filtered_85pc (48 units)
```
