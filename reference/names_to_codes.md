# Generate short codes from long names

Given a character vector of long names (probably with spaces), generates
short codes. Intended for use when importing from the COIN Tool.

## Usage

``` r
names_to_codes(cvec, maxword = 2, maxlet = 4)
```

## Arguments

- cvec:

  A character vector of names

- maxword:

  The maximum number of words to use in building a short name (default
  2)

- maxlet:

  The number of letters to take from each word (default 4)

## Value

A corresponding character vector, but with short codes, and no
duplicates.

## Details

This function replaces the now-defunct `names2Codes()` from COINr \<
v1.0.

## See also

- [`import_coin_tool()`](https://bluefoxr.github.io/COINr/reference/import_COIN_tool.md)
  Import data from the COIN Tool (Excel).

## Examples

``` r
# get names from example data
iNames <- ASEM_iMeta$iName

# convert to codes
names_to_codes(iNames)
#>  [1] "LogiPerf"   "InteFlig"   "LineShip"   "BordCros"   "TradElec"  
#>  [6] "TradGas"    "AverConn"   "PopuCove"   "TradGood"   "TradServ"  
#> [11] "ForeDire"   "PersRemi"   "ForePort"   "EmbaNetw"   "PartInte"  
#> [16] "VotiAlig"   "CostExpo"   "MeanTari"   "TechBarr"   "SignConv"  
#> [21] "RegiTrad"   "VisaVisa"   "InteStud"   "ReseOutp"   "PateWith"  
#> [26] "TradCult"   "TradCult_1" "TourArri"   "MigrStoc"   "CommLang"  
#> [31] "ReneEner"   "PrimEner"   "EmisCapi"   "DomeMate"   "ForeLoss"  
#> [36] "PopuLivi"   "PalmInde"   "TertGrad"   "FreePres"   "ToleMino"  
#> [41] "PresInte"   "CorrPerc"   "FemaLabo"   "WomeNati"   "PublDebt"  
#> [46] "PrivDebt"   "CapiGrow"   "R&ExPerc"   "PropYout"   "Phys"      
#> [51] "EconFina"   "Poli"       "Inst"       "PeopPeop"   "Envi"      
#> [56] "Soci"       "EconFina_1" "Conn"       "Sust"       "SustConn"  
#> [61] "Grou"       "CapiGrou"   "PopuGrou"   "EuroAsia"   "LandArea"  
#> [66] "EnerCons"   "GDP"        "Popu"      
```
