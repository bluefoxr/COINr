# Impute by group mean

Replaces `NA`s in a numeric vector with the grouped arithmetic means of
the non-`NA` values. Groups are defined by the `f` argument.

## Usage

``` r
i_mean_grp(x, f, skip_f_na = TRUE)
```

## Arguments

- x:

  A numeric vector

- f:

  A grouping variable, of the same length of `x`, that specifies the
  group that each value of `x` belongs to. This will be coerced to a
  factor.

- skip_f_na:

  If `TRUE`, will work around any `NA`s in `f` (the corresponding values
  of `x` will be excluded from the imputation and returned unaltered).
  Else if `FALSE`, will cause an error.

## Value

A numeric vector

## Examples

``` r
x <- c(NA, runif(10), NA)
f <- c(rep("a", 6), rep("b", 6))
i_mean_grp(x, f)
#>  [1] 0.4564603 0.1497639 0.1511259 0.6441252 0.6677410 0.6695454 0.2101826
#>  [8] 0.1899729 0.4772996 0.7766469 0.7732800 0.4854764
```
