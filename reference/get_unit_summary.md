# Generate unit summary table

Generates a summary table for a single unit. This is mostly useful in
unit reports.

## Usage

``` r
get_unit_summary(coin, usel, Levels, dset = "Aggregated", nround = 2)
```

## Arguments

- coin:

  A coin

- usel:

  A selected unit code

- Levels:

  The aggregation levels to display results from.

- dset:

  The data set within the coin to extract scores and ranks from

- nround:

  Number of decimals to round scores to, default 2. Set to `NULL` to
  disable rounding.

## Value

A summary table as a data frame, containing scores and ranks for
specified indicators/aggregates.

## Details

This returns the scores and ranks for each indicator/aggregate as
specified in `aglevs`. It orders the table so that the highest
aggregation levels are first. This means that if the index level is
included, it will be first.

This function replaces the now-defunct `getUnitSummary()` from COINr \<
v1.0.

## Examples

``` r
# build full example coin
coin <- build_example_coin(quietly = TRUE)

# summary of scores for IND at levels 4, 3 and 2
get_unit_summary(coin, usel = "IND", Levels = c(4,3,2), dset = "Aggregated")
#>         Code                         Name Score Rank
#> 1      Index     Sustainable Connectivity 39.79   45
#> 2       Conn                 Connectivity 28.60   44
#> 3       Sust               Sustainability 50.99   43
#> 4   Physical                     Physical 18.78   48
#> 5   ConEcFin Economic and Financial (Con)  6.38   49
#> 6  Political                    Political 51.90   33
#> 7     Instit                Institutional 59.84   37
#> 8        P2P             People to People  6.07   47
#> 9    Environ                Environmental 75.34    6
#> 10    Social                       Social 22.74   51
#> 11  SusEcFin Economic and Financial (Sus) 54.88   36
```
