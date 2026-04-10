# Z-score a vector

Standardises a vector `x` by scaling it to have a mean and standard
deviation specified by `m_sd`.

## Usage

``` r
n_zscore(x, m_sd = c(0, 1))
```

## Arguments

- x:

  A numeric vector

- m_sd:

  A vector `c(m, sd)`, where `m` is desired mean and `sd` is the target
  standard deviation.

## Value

Numeric vector

## Details

This function also supports parameter specification in `iMeta` for the
[`Normalise.coin()`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
method. To do this, add columns `zscore_mean`, and `zscore_sd` to the
`iMeta` table, which specify the mean and standard deviation to scale
each indicator to, respectively. Then set `f_n_para = "use_iMeta"`
within the `global_specs` list. See also examples in the [normalisation
vignette](https://bluefoxr.github.io/COINr/articles/normalise.html).

## Examples

``` r
x <- runif(20)
n_zscore(x)
#>  [1]  0.64036049 -0.38159445 -0.42493602  0.95744369 -0.07100715  1.56024413
#>  [7]  1.18698061 -0.47583721  0.83766773  0.35357759  1.26002616 -0.16788279
#> [13] -1.92275696  0.78902480 -1.62701141  0.18359300  0.23014442 -1.69837809
#> [19] -0.35421387 -0.87544467
```
