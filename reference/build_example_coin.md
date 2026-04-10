# Build ASEM example coin

Shortcut function to build the ASEM example coin, using inbuilt example
data. This can be useful for testing and also for building reproducible
examples. To see the underlying commands run `edit(build_example_coin)`.
See also
[`vignette("coins")`](https://bluefoxr.github.io/COINr/articles/coins.md).

## Usage

``` r
build_example_coin(up_to = NULL, quietly = FALSE)
```

## Arguments

- up_to:

  The point up to which to build the index. If `NULL`, builds full
  index. Else specify a building function (as a string) - the index will
  be built up to and including this function. This option is mainly for
  helping with function examples. Example: `up_to = "Normalise"`.

- quietly:

  If `TRUE`, suppresses all messages.

## Value

coin class object

## Details

This function replaces the now-defunct `build_ASEM()` from COINr \<
v1.0.

## Examples

``` r
# build example coin up to data treatment step
coin <- build_example_coin(up_to = "Treat")
#> iData checked and OK.
#> iMeta checked and OK.
#> Written data set to .$Data$Raw
#> Written data set to .$Data$Denominated
#> Written data set to .$Data$Imputed
#> Written data set to .$Data$Screened
#> Written data set to .$Data$Treated
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
#>   Denominated (51 units)
#>   Imputed (51 units)
#>   Screened (51 units)
#>   Treated (51 units)
```
