# Perform PCA on a coin

Performs Principle Component Analysis (PCA) on a specified data set and
subset of indicators or aggregation groups. This function has two main
outputs: the output(s) of
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html), and optionally
the weights resulting from the PCA. Therefore it can be used as an
analysis tool and/or a weighting tool. For the weighting aspect, please
see the details below.

## Usage

``` r
get_PCA(
  coin,
  dset = "Raw",
  iCodes = NULL,
  Level = NULL,
  by_groups = TRUE,
  nowarnings = FALSE,
  weights_to = NULL,
  out2 = "list"
)
```

## Arguments

- coin:

  A coin

- dset:

  The name of the data set in `.$Data` to use.

- iCodes:

  An optional character vector of indicator codes to subset the
  indicator data, passed to
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)

- Level:

  The aggregation level to take indicator data from. Integer from 1
  (indicator level) to N (top aggregation level, typically the index).

- by_groups:

  If `TRUE` (default), performs PCA inside each aggregation group inside
  the specified level. If `FALSE`, performs a single PCA over all
  indicators/aggregates in the specified level.

- nowarnings:

  If `FALSE` (default), will give warnings where missing data are found.
  Set to `TRUE` to suppress these warnings.

- weights_to:

  A string to name the resulting set of weights. If this is specified,
  and `out2 = "coin"`, will write a new set of "PCA weights" to the
  `.$Meta$Weights` list. This is experimental - see details. If `NULL`,
  does not write any weights (default).

- out2:

  If the input is a coin object, this controls where to send the output.
  If `"coin"`, it sends the results to the coin object, otherwise if
  `"list"`, outputs to a separate list (default).

## Value

If `out2 = "coin"`, results are appended to the coin object.
Specifically:

- A list is added to `.$Analysis` containing PCA weights (loadings) of
  the first principle component, and the output of
  [stats::prcomp](https://rdrr.io/r/stats/prcomp.html), for each
  aggregation group found in the targeted level.

- If `weights_to` is specified, a new set of PCA weights is added to
  `.$Meta$Weights` If `out2 = "list"` the same outputs are contained in
  a list.

## Details

PCA must be approached with care and an understanding of what is going
on. First, let's consider the PCA excluding the weighting component. PCA
takes a set of data consisting of variables (indicators) and
observations. It then rotates the coordinate system such that in the new
coordinate system, the first axis (called the first principal component
(PC)) aligns with the direction of maximum variance of the data set. The
amount of variance explained by the first PC, and by the next several
PCs, can help to understand whether the data can be explained by simpler
set of variables. PCA is often used for dimensionality reduction in
modelling, for example.

In the context of composite indicators, PCA can be used first as an
analysis tool. We can check for example, within an aggregation group,
can the indicators mostly be explained by one PC? If so, this gives a
little extra justification to aggregating the indicators because the
information lost in aggregation will be less. We can also check this
over the entire set of indicators.

The complications are in a composite indicator, the indicators are
grouped and arranged into a hierarchy. This means that when performing a
PCA, we have to decide which level to perform it at, and which groupings
to use, if any. The `get_PCA()` function, using the `by_groups`
argument, allows to automatically apply PCA by group if this is
required.

The output of `get_PCA()` is a PCA object for each of the groups
specified, which can then be examined using existing tools in R, see
[`vignette("analysis")`](https://bluefoxr.github.io/COINr/articles/analysis.md).

The other output of `get_PCA()` is a set of "PCA weights" if the
`weights_to` argument is specified. Here we also need to say some words
of caution. First, what constitutes "PCA weights" in composite
indicators is not very well-defined. In COINr, a simple option is
adopted. That is, the loadings of the first principal component are
taken as the weights. The logic here is that these loadings should
maximise the explained variance - the implication being that if we use
these as weights in an aggregation, we should maximise the explained
variance and hence the information passed from the indicators to the
aggregate value. This is a nice property in a composite indicator, where
one of the aims is to represent many indicators by single composite. See
[doi:10.1016/j.envsoft.2021.105208](https://doi.org/10.1016/j.envsoft.2021.105208)
for a discussion on this.

But. The weights that result from PCA have a number of downsides. First,
they can often include negative weights which can be hard to justify.
Also PCA may arbitrarily flip the axes (since from a variance point of
view the direction is not important). In the quest for maximum variance,
PCA will also weight the strongest-correlating indicators the highest,
which means that other indicators may be neglected. In short, it often
results in a very unbalanced set of weights. Moreover, PCA can only be
performed on one level at a time.

All these considerations point to the fact: while PCA as an analysis
tool is well-established, please use PCA weights with care and
understanding of what is going on.

This function replaces the now-defunct `getPCA()` from COINr \< v1.0.

## See also

- [stats::prcomp](https://rdrr.io/r/stats/prcomp.html) Principle
  component analysis

## Examples

``` r
# build example coin
coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

# PCA on "Sust" group of indicators
l_pca <- get_PCA(coin, dset = "Raw", iCodes = "Sust",
                 out2 = "list", nowarnings = TRUE)

# Summary of results for one of the sub-groups
summary(l_pca$PCAresults$Social$PCAres)
#> Importance of components:
#>                           PC1    PC2    PC3     PC4     PC5     PC6     PC7
#> Standard deviation     2.2042 1.1256 0.9788 0.78834 0.77153 0.56836 0.42463
#> Proportion of Variance 0.5398 0.1408 0.1065 0.06905 0.06614 0.03589 0.02003
#> Cumulative Proportion  0.5398 0.6806 0.7871 0.85611 0.92225 0.95814 0.97817
#>                            PC8     PC9
#> Standard deviation     0.36068 0.25760
#> Proportion of Variance 0.01445 0.00737
#> Cumulative Proportion  0.99263 1.00000
```
