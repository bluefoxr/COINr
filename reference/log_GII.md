# Log-transform a vector

Performs a log transform on a numeric vector. This function is currently
not recommended - see comments below.

## Usage

``` r
log_GII(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Set `TRUE` to remove `NA` values, otherwise returns `NA`.

## Value

A log-transformed vector of data.

## Details

Specifically, this performs a "GII log" transform, which is what was
encoded in the GII2020 spreadsheet.

Note that this transformation is currently NOT recommended because it
seems quite volatile and can flip the direction of the indicator. If the
maximum value of the indicator is less than one, this reverses the
direction.

## Examples

``` r
x <- runif(20)
log_GII(x)
#> $x
#>  [1] 0.91046969 0.14909131 0.53485780 0.34260067 0.70459850 0.53606807
#>  [7] 0.93217485 0.94375907 0.80970037 0.56567287 0.69001231 0.41359769
#> [13] 0.40231717 0.01197623 0.23306886 0.21793723 0.80899358 0.54132597
#> [19] 0.22344710 0.33578517
#> 
#> $treated
#>  [1] "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII"
#>  [8] "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII"
#> [15] "log_GII" "log_GII" "log_GII" "log_GII" "log_GII" "log_GII"
#> 
```
