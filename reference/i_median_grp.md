# Impute by group median

Replaces `NA`s in a numeric vector with the grouped medians of the
non-`NA` values. Groups are defined by the `f` argument.

## Usage

``` r
i_median_grp(x, f, skip_f_na = TRUE)
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
i_median_grp(x, f)
#>  [1] 0.52659494 0.52562715 0.97964470 0.98923273 0.52659494 0.09680426
#>  [7] 0.56856717 0.14015188 0.59003401 0.93166719 0.19964817 0.56856717
```
