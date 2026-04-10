# Build example purse

Shortcut function to build an example purse. This is currently an
"artificial" example, in that it takes the ASEM data set used in
[`build_example_coin()`](https://bluefoxr.github.io/COINr/reference/build_example_coin.md)
and replicates it for five years, adding artificial noise to simulate
year-on-year variation. This was done simply to demonstrate the
functionality of purses, and will at some point be replaced with a real
example. See also
[`vignette("coins")`](https://bluefoxr.github.io/COINr/articles/coins.md).

## Usage

``` r
build_example_purse(up_to = NULL, quietly = FALSE)
```

## Arguments

- up_to:

  The point up to which to build the index. If `NULL`, builds full
  index. Else specify a `build_*` function (as a string) - the index
  will be built up to and including this function. This option is mainly
  for helping with function examples. Example:
  `up_to = "build_normalise"`.

- quietly:

  If `TRUE`, suppresses all messages.

## Value

purse class object

## Examples

``` r
# build example purse up to unit screening step
purse <- build_example_purse(up_to = "Screen")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Screened
purse
#> -----------------------------
#> A purse with... 5 coins 
#> -----------------------------
#> 
#>  Time n_Units n_Inds n_dsets
#>  2018      51     49       2
#>  2019      51     49       2
#>  2020      51     49       2
#>  2021      51     49       2
#>  2022      51     49       2
#> 
#> -----------------------------------
#> Sample from first coin (2018):
#> -----------------------------------
#> 
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
#>   Screened (46 units)
```
