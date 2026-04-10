# Analysis

This vignette describes the “analysis” features of COINr. By this, we
mean functions that retrieve statistical measures from the coin in
various ways. This excludes things like sensitivity analysis, which
involves tinkering with the construction methodology.

In short, here we discuss obtaining indicator statistics, correlations,
data availability, and some slightly more complex ideas such as
Cronbach’s alpha and principal component analysis.

## Indicator statistics

Indicator statistics can be obtained using the
[`get_stats()`](https://bluefoxr.github.io/COINr/reference/get_stats.md)
function.

``` r
# load COINr
library(COINr)

# build example coin
coin <-  build_example_coin(up_to = "new_coin", quietly = TRUE)

# get table of indicator statistics for raw data set
stat_table <- get_stats(coin, dset = "Raw", out2 = "df")
```

The resulting data frame has 18 columns, which is hard to display
concisely here. Therefore we will look at the columns in groups of five.

``` r
head(stat_table[1:5], 5)
#>     iCode  Min    Max  Mean Median
#> 1     LPI 2.07   4.23  3.41   3.42
#> 2 Flights 0.99 211.00 38.60  25.60
#> 3    Ship 0.00  21.20 12.00  12.70
#> 4    Bord 0.00 122.00 23.60  18.00
#> 5    Elec 0.00 110.00 16.20   6.91
```

Each row is one of the indicators from the targeted data set. Then
columns are statistics, here obvious things like the minimum, maximum,
mean and median.

``` r
head(stat_table[6:10], 5)
#>      Std   Skew   Kurt N.Avail N.NonZero
#> 1  0.538 -0.304 -0.657      51        51
#> 2 46.700  2.100  4.510      51        51
#> 3  6.840 -0.576 -0.681      51        42
#> 4 24.800  2.150  5.790      51        45
#> 5 22.700  2.230  5.790      51        47
```

In the first three columns here we find the standard deviation, skewness
and kurtosis. The remaining two columns are “N.Avail”, which is the
number of non-`NA` data points, and “N.NonZero”, the number of non-zero
points. This latter can be of interest because some indicators may have
a high proportion of zeroes, which can be problematic.

``` r
head(stat_table[11:15], 5)
#>   N.Unique N.Same Frc.Avail Frc.NonZero Frc.Unique
#> 1       51      1         1       1.000      1.000
#> 2       51      1         1       1.000      1.000
#> 3       43      9         1       0.824      0.843
#> 4       30      6         1       0.882      0.588
#> 5       46      4         1       0.922      0.902
```

Here we have “N.Unique”, which is the number of unique data points
(i.e. excluding duplicate values). The following three columns are
similar to previous columns, e.g. “Frc.Avail” is the fraction of data
availability, as opposed to the number of available points (N.Avail).
The final column, “Flag.Avail”, is a logical flag: if the data
availability (“Frc.Avail”) is below the limit specified by the `t_avail`
argument of
[`get_stats()`](https://bluefoxr.github.io/COINr/reference/get_stats.md),
it will be flagged as “LOW”.

``` r
head(stat_table[16:ncol(stat_table)], 5)
#>   Frc.Same Flag.Avail Flag.NonZero Flag.Unique Flag.SkewKurt
#> 1   0.0196         ok           ok          ok            ok
#> 2   0.0196         ok           ok          ok           OUT
#> 3   0.1760         ok           ok          ok            ok
#> 4   0.1180         ok           ok          ok           OUT
#> 5   0.0784         ok           ok          ok           OUT
```

The first two final columns are analogous to “Flag.Avail” and have
thresholds which are controlled by arguments to
[`get_stats()`](https://bluefoxr.github.io/COINr/reference/get_stats.md).
The final column is a basic test of outliers which is commonly used in
composite indicators, for example in the [COIN
Tool](https://knowledge4policy.ec.europa.eu/composite-indicators/coin-tool_en).
This is the same process as used in
[`check_SkewKurt()`](https://bluefoxr.github.io/COINr/reference/check_SkewKurt.md)
which will flag “OUT” if the absolute skewness is greater than a set
threshold (default 2) AND kurtosis is greater than a threshold (default
3.5). In short, indicators that are flagged here could be considered for
outlier treatment.

## Unit data availability

The same kind of analysis can be performed for units, rather than
indicators. Here, the main thing of interest is data availability, which
can be obtained throug the
[`get_data_avail()`](https://bluefoxr.github.io/COINr/reference/get_data_avail.md)
function.

``` r
l_dat <- get_data_avail(coin, dset = "Raw", out2 = "list")

str(l_dat, max.level = 1)
#> List of 2
#>  $ Summary:'data.frame': 51 obs. of  6 variables:
#>  $ ByGroup:'data.frame': 51 obs. of  12 variables:
```

Here we see the output is a list with two data frames. The first is a
summary for each unit:

``` r
head(l_dat$Summary, 5)
#>    uCode N_missing N_zero N_miss_or_zero Dat_Avail  Non_Zero
#> 31   AUS         0      3              3  1.000000 0.9387755
#> 1    AUT         0      2              2  1.000000 0.9591837
#> 2    BEL         0      2              2  1.000000 0.9591837
#> 32   BGD         6      1              7  0.877551 0.9767442
#> 3    BGR         0      0              0  1.000000 1.0000000
```

Each unit has its number of missing points, zero points, missing-or-zero
points, as well as the percentage data availability and percentage
non-zero. The “ByGroup” data frame gives data availability within
aggregation groups:

``` r
head(l_dat$ByGroup[1:5], 5)
#>    uCode ConEcFin    Instit  P2P Physical
#> 31   AUS        1 1.0000000 1.00    1.000
#> 1    AUT        1 1.0000000 1.00    1.000
#> 2    BEL        1 1.0000000 1.00    1.000
#> 32   BGD        1 0.8333333 0.75    0.875
#> 3    BGR        1 1.0000000 1.00    1.000
```

Here we just view the first few columns to save space. The values are
the fraction of indicator availability within each aggregation group.

## Correlations

Correlations can be obtained and viewed directly using the
[`plot_corr()`](https://bluefoxr.github.io/COINr/reference/plot_corr.md)
function which is explained in the
[Visualisation](https://bluefoxr.github.io/COINr/articles/visualisation.md)
vignette. Here, we explore the functions for obtaining correlation
matrices, flags and p-values.

The most general-purpose function for obtaining correlations between
indicators is
[`get_corr()`](https://bluefoxr.github.io/COINr/reference/get_corr.md)
function (which is called by
[`plot_corr()`](https://bluefoxr.github.io/COINr/reference/plot_corr.md)).
This allows almost any set of indicators/aggregates to be correlated
against almost any other set. We won’t go over the full functionality
here because this is covered in
[Visualisation](https://bluefoxr.github.io/COINr/articles/visualisation.md)
vignette. However to demonstrate a couple of examples we begin by
building the full example coin up to the aggregated data set.

``` r
coin <- build_example_coin(quietly = TRUE)
```

Now we can take some examples. First, to get the correlations between
indicators within the Environmental group:

``` r
# get correlations
cmat <- get_corr(coin, dset = "Raw", iCodes = list("Environ"), Levels = 1)
# examine first few rows
head(cmat)
#>       Var1   Var2 Correlation
#> 1      CO2    CO2   1.0000000
#> 2   Forest    CO2          NA
#> 3   MatCon    CO2   0.7222051
#> 4 PrimEner    CO2   0.3268484
#> 5    Renew    CO2  -0.5986338
#> 6      CO2 Forest          NA
```

Here we see that the default output of
[`get_corr()`](https://bluefoxr.github.io/COINr/reference/get_corr.md)
is a long-format correlation table. If you want the wide format, set
`make_long = FALSE`.

``` r
# get correlations
cmat <- get_corr(coin, dset = "Raw", iCodes = list("Environ"), Levels = 1, make_long = FALSE)
# examine first few rows
round_df(head(cmat), 2)
#>     CO2 Forest MatCon PrimEner Renew
#> 1  1.00     NA   0.72     0.33 -0.60
#> 2    NA   1.00     NA       NA  0.35
#> 3  0.72     NA   1.00     0.38 -0.42
#> 4  0.33     NA   0.38     1.00    NA
#> 5 -0.60   0.35  -0.42       NA  1.00
```

This gives the more classical-looking correlation matrix, although the
long format can sometimes be easier to work with for futher processing.
One further option that is worth mentioning is `pval`: by default,
[`get_corr()`](https://bluefoxr.github.io/COINr/reference/get_corr.md)
will return any correlations with a p-value lower than 0.05 as `NA`,
indicating that these correlations are insignificant at this
significance level. You can adjust this threshold by changing `pval`, or
disable it completely by setting `pval = 0`.

On the subject of p-values, COINr includes a
[`get_pvals()`](https://bluefoxr.github.io/COINr/reference/get_pvals.md)
function which can be used to get p-values of correlations between a
supplied matrix or data frame. This cannot be used directly on a coin
and is more of a helper function but may still be useful.

Two further functions are of interest regarding correlations. The first
is
[`get_corr_flags()`](https://bluefoxr.github.io/COINr/reference/get_corr_flags.md).
This is a useful function for finding correlations between indicators
that exceed or fall below a given threshold, within aggregation groups:

``` r
get_corr_flags(coin, dset = "Normalised", cor_thresh = 0.75,
               thresh_type = "high", grouplev = 2)
#>      Group Ind1      Ind2  Corr
#> 250 Social  CPI FreePress 0.761
#> 283 Social  CPI      NGOs 0.768
```

Here we have identified any correlations above 0.75, from the
“Normalised” data set, that are between indicators in the same group in
level 2. Actually 0.75 is quite low for searching for “high
correlations”, but it is used as an example here because the example
data set doesn’t have any very high correlations.

By switching `thresh_type = "low"` we can similarly look for
low/negative correlations:

``` r
get_corr_flags(coin, dset = "Normalised", cor_thresh = -0.5,
               thresh_type = "low", grouplev = 2)
#>       Group      Ind1   Ind2   Corr
#> 162  Instit CostImpEx   TBTs -0.697
#> 1778 Instit      RTAs   TBTs -0.752
#> 2019 Instit      TBTs TIRcon -0.623
```

Our example has some fairly significant negative correlations! All
within the “Institutional” group, and with the Technical Barriers to
Trade indicator.

A final function to mention is
[`get_denom_corr()`](https://bluefoxr.github.io/COINr/reference/get_denom_corr.md).
This is related to the operation of denominating indicators (see
[Denomination](https://bluefoxr.github.io/COINr/articles/denomination.md)
vignette), and identifies any indicators that are correlated (using the
absolute value of correlation) above a given threshold with any of the
supplied denominators. This can help to identify in some cases *whether*
to denominate an indicator and *with what* - i.e. if an indicator is
strongly related with a denominator that means it is dependent on it,
which may be a reason to denominate.

``` r
get_denom_corr(coin, dset = "Raw", cor_thresh = 0.7)
#>          Ind  Denom Corr
#> 70  CultGood Energy 0.71
#> 119 CultGood    GDP 0.85
#> 52       FDI Energy 0.74
#> 101      FDI    GDP 0.85
#> 99     Goods    GDP 0.80
#> 102   PRemit    GDP 0.71
#> 116 Research    GDP 0.78
#> 100 Services    GDP 0.77
#> 66     StMob Energy 0.73
#> 115    StMob    GDP 0.84
```

Using a threshold of 0.7, and examining the raw data set, we see that
several indicators are strongly related to the denominators, a classical
example being export value of goods (Goods) being well correlated with
GDP. Many of these pairs flagged here are indeed used as denominators in
the ASEM example, but also for conceptual reasons.

## Multivariate tools

A first simple tool is to calculate Cronbach’s alpha. This can be done
with any group of indicators, either the full set, or else targeting
specific groups.

``` r
get_cronbach(coin, dset = "Raw", iCodes = "P2P", Level = 1)
#> [1] 0.05126659
```

This simply calculates Cronbach’s alpha (a measure of statistical
consistency) for the “P2P” group (People to People connectivity, this
case).

Another multivariate analysis tool is principal component analysis
(PCA). Although, like correlation, this is built into base R, the
[`get_PCA()`](https://bluefoxr.github.io/COINr/reference/get_PCA.md)
function makes it easier to obtain PCA for groups of indicators,
following the structure of the index.

``` r
l_pca <- get_PCA(coin, dset = "Raw", iCodes = "Sust", out2 = "list")
#> Warning in PCAwts(parents[ii]): 1 missing values found. Removing 1 rows with missing values in order to perform
#> PCA. You can also try imputing data first to avoid this.
#> Warning in PCAwts(parents[ii]): 22 missing values found. Removing 14 rows with missing values in order to perform
#> PCA. You can also try imputing data first to avoid this.
#> Warning in PCAwts(parents[ii]): 8 missing values found. Removing 7 rows with missing values in order to perform
#> PCA. You can also try imputing data first to avoid this.
```

The function can return its results either as a list, or appended to the
coin if `out2 = "coin"`. Here the output is a list and we will explore
its output. First note the warnings above due to missing data, which can
be suppressed using `nowarnings = TRUE`. The output list looks like
this:

``` r
str(l_pca, max.level = 1)
#> List of 2
#>  $ Weights   :'data.frame':  60 obs. of  3 variables:
#>  $ PCAresults:List of 3
```

I.e. we have a data frame of “PCA weights” and some PCA results. We
ignore the weights for the moment and look closer at the PCA results:

``` r
str(l_pca$PCAresults, max.level = 2)
#> List of 3
#>  $ Environ :List of 3
#>   ..$ wts   : num [1:5] -0.581 0.214 -0.543 -0.313 0.473
#>   ..$ PCAres:List of 5
#>   .. ..- attr(*, "class")= chr "prcomp"
#>   ..$ iCodes: chr [1:5] "CO2" "Forest" "MatCon" "PrimEner" ...
#>  $ Social  :List of 3
#>   ..$ wts   : num [1:9] -0.417 -0.324 0.401 -0.191 0.302 ...
#>   ..$ PCAres:List of 5
#>   .. ..- attr(*, "class")= chr "prcomp"
#>   ..$ iCodes: chr [1:9] "CPI" "FemLab" "FreePress" "NGOs" ...
#>  $ SusEcFin:List of 3
#>   ..$ wts   : num [1:5] 0.472 0.451 -0.492 -0.227 -0.53
#>   ..$ PCAres:List of 5
#>   .. ..- attr(*, "class")= chr "prcomp"
#>   ..$ iCodes: chr [1:5] "GDPGrow" "NEET" "PrivDebt" "PubDebt" ...
```

By default,
[`get_PCA()`](https://bluefoxr.github.io/COINr/reference/get_PCA.md)
will run a separate PCA for each aggregation group within the specified
level. In this case, it has run three: one for each of the “Environ”,
“Social” and “SusEcFin” groups. Each of these contains `wts`, a set of
PCA weights for that group, `PCAres`, which is the direct output of
[`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html), and `iCodes`,
which is the corresponding vector of indicator codes for the group.

We can do some basic PCA analysis using base R’s tools using the
“PCAres” objects, e.g.:

``` r
# summarise PCA results for "Social" group
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

See [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html) and
elsewhere for more resources on PCA in R.

Now turning to the weighting,
[`get_PCA()`](https://bluefoxr.github.io/COINr/reference/get_PCA.md)
also outputs a set of “PCA weights”. These are output attached to the
list as shown above, or if `out2 = "coin"`, will be written to the
weights list inside the coin if `weights_to` is also specified. See the
[Weights](https://bluefoxr.github.io/COINr/articles/weights.md) vignette
for some more details on this. Note that PCA weights come with a number
of caveats: please read the documentation for
[`get_PCA()`](https://bluefoxr.github.io/COINr/reference/get_PCA.md) for
a discussion on this.
