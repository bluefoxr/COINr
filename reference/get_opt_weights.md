# Weight optimisation

This function provides optimised weights to agree with a pre-specified
vector of "target importances".

## Usage

``` r
get_opt_weights(
  coin,
  itarg = NULL,
  dset,
  Level,
  cortype = "pearson",
  optype = "balance",
  toler = NULL,
  maxiter = NULL,
  weights_to = NULL,
  out2 = "list"
)
```

## Arguments

- coin:

  coin object

- itarg:

  a vector of (relative) target importances. For example, `c(1,2,1)`
  would specify that the second indicator should be twice as "important"
  as the other two.

- dset:

  Name of the aggregated data set found in `coin$Data` which results
  from calling
  [`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md).

- Level:

  The aggregation level to apply the weight adjustment to. This can only
  be one level.

- cortype:

  The type of correlation to use - can be either `"pearson"`,
  `"spearman"` or `"kendall"`. See
  [stats::cor](https://rdrr.io/r/stats/cor.html).

- optype:

  The optimisation type. Either `"balance"`, which aims to balance
  correlations according to a vector of "importances" specified by
  `itarg` (default), or `"infomax"` which aims to maximise overall
  correlations.

- toler:

  Tolerance for convergence. Defaults to 0.1 (decrease for more
  accuracy, increase if convergence problems).

- maxiter:

  Maximum number of iterations. Default 500.

- weights_to:

  Name to write the optimised weight set to, if `out2 = "coin"`.

- out2:

  Where to output the results. If `"coin"` (default for coin input),
  appends to updated coin, creating a new list of weights in
  `.$Parameters$Weights`. Otherwise if `"list"` outputs to a list
  (default).

## Value

If `out2 = "coin"` returns an updated coin object with a new set of
weights in `.$Meta$Weights`, plus details of the optimisation in
`.$Analysis`. Else if `out2 = "list"` the same outputs (new weights plus
details of optimisation) are wrapped in a list.

## Details

This is a linear version of the weight optimisation proposed in this
paper:
[doi:10.1016/j.ecolind.2017.03.056](https://doi.org/10.1016/j.ecolind.2017.03.056)
. Weights are optimised to agree with a pre-specified vector of
"importances". The optimised weights are returned back to the coin.

See
[`vignette("weights")`](https://bluefoxr.github.io/COINr/articles/weights.md)
for more details on the usage of this function and an explanation of the
underlying method. Note that this function calculates correlations
without considering statistical significance.

This function replaces the now-defunct `weightOpt()` from COINr \< v1.0.

## Examples

``` r
# build example coin
coin <- build_example_coin(quietly = TRUE)

# check correlations between level 3 and index
get_corr(coin, dset = "Aggregated", Levels = c(3, 4))
#>    Var1 Var2 Correlation
#> 1 Index Conn   0.9397805
#> 2 Index Sust   0.8382873

# optimise weights at level 3
l_opt <- get_opt_weights(coin, itarg = "equal", dset = "Aggregated",
                        Level = 3, weights_to = "OptLev3", out2 = "list")
#> iterating... objective function = -7.11287670895252
#> iterating... objective function = -6.75731482891423
#> iterating... objective function = -7.5563175412706
#> iterating... objective function = -8.21181051402935
#> iterating... objective function = -10.0802172796095
#> iterating... objective function = -13.3043247136273
#> iterating... objective function = -8.7011048855954
#> iterating... objective function = -7.93721550859392
#> iterating... objective function = -9.92111795779074
#> iterating... objective function = -8.57337082557942
#> iterating... objective function = -13.0490317878554
#> iterating... objective function = -10.1205749624737
#> iterating... objective function = -11.4698196057753
#> iterating... objective function = -11.5046209642509
#> iterating... objective function = -12.938292451273
#> Optimisation successful!

# view results
tail(l_opt$WeightsOpt)
#>       iCode Level    Weight
#> 55  Environ     2 1.0000000
#> 56   Social     2 1.0000000
#> 57 SusEcFin     2 1.0000000
#> 58     Conn     3 0.3902439
#> 59     Sust     3 0.6097561
#> 60    Index     4 1.0000000

l_opt$CorrResultsNorm
#>   Desired  Obtained OptWeight
#> 1     0.5 0.5015505 0.3902439
#> 2     0.5 0.4984495 0.6097561
```
