# Add and remove indicators

A shortcut function to add and remove indicators. This will make the
relevant changes and recalculate the index if asked. Adding and removing
is done relative to the current set of indicators used in calculating
the index results. Any indicators that are added must of course be
present in the original `iData` and `iMeta` that were input to
[`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md).

## Usage

``` r
change_ind(coin, add = NULL, drop = NULL, regen = FALSE)
```

## Arguments

- coin:

  coin object

- add:

  A character vector of indicator codes to add (must be present in the
  original input data)

- drop:

  A character vector of indicator codes to remove (must be present in
  the original input data)

- regen:

  Logical (default): if `TRUE`, automatically regenerates the results
  based on the new specs Otherwise, just updates the `.$Log` parameters.
  This latter might be useful if you want to Make other changes before
  re-running using the
  [`Regen()`](https://bluefoxr.github.io/COINr/reference/Regen.md)
  function.

## Value

An updated coin, with regenerated results if `regen = TRUE`.

## Details

See also
[`vignette("adjustments")`](https://bluefoxr.github.io/COINr/articles/adjustments.md).

This function replaces the now-defunct `indChange()` from COINr \< v1.0.

## Examples

``` r
# build full example coin
coin <- build_example_coin(quietly = TRUE)

# exclude two indicators and regenerate
# remove two indicators and regenerate the coin
coin_remove <- change_ind(coin, drop = c("LPI", "Forest"), regen = TRUE)
#> coin has been regenerated using new specs.

coin_remove
#> --------------
#> A coin with...
#> --------------
#> Input:
#>   Units: 51 (AUS, AUT, BEL, ...)
#>   Indicators: 47 (Goods, Services, FDI, ...)
#>   Denominators: 4 (Area, Energy, GDP, ...)
#>   Groups: 4 (GDP_group, GDPpc_group, Pop_group, ...)
#> 
#> Structure:
#>   Level 1 Indicator: 47 indicators (FDI, ForPort, Goods, ...) 
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
#>   Normalised (51 units)
#>   Aggregated (51 units)
```
