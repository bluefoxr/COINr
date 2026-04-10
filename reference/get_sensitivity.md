# Sensitivity and uncertainty analysis of a coin

This function performs global sensitivity and uncertainty analysis of a
coin. You must specify which parameters of the coin to vary, and the
alternatives/distributions for those parameters.

## Usage

``` r
get_sensitivity(
  coin,
  SA_specs,
  N,
  SA_type = "UA",
  dset,
  iCode,
  Nboot = NULL,
  quietly = FALSE,
  check_addresses = TRUE,
  diagnostic_mode = FALSE
)
```

## Arguments

- coin:

  A coin

- SA_specs:

  Specifications of the input uncertainties

- N:

  The number of regenerations

- SA_type:

  The type of analysis to run. `"UA"` runs an uncertainty analysis.
  `"SA"` runs a sensitivity analysis (which anyway includes an
  uncertainty analysis).

- dset:

  The data set to extract the target variable from (passed to
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)).

- iCode:

  The variable within `dset` to use as the target variable (passed to
  [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)).

- Nboot:

  Number of bootstrap samples to take when estimating confidence
  intervals on sensitivity indices.

- quietly:

  Set to `TRUE` to suppress progress messages.

- check_addresses:

  Logical: if `FALSE` skips the check of the validity of the parameter
  addresses. Default `TRUE`, but useful to set to `FALSE` if running
  this e.g. in a Rmd document (because may require user input).

- diagnostic_mode:

  Logical: if `TRUE`, this will additionally attach all coins generated
  as part of the sensitivity analysis to the output list. This is
  intended to be used to check what is going on within the sensitivity
  analysis.

## Value

Sensitivity analysis results as a list, containing:

- `.$Scores` a data frame with a row for each unit, and columns are the
  scores for each replication.

- `.$Ranks` as `.$Scores` but for unit ranks

- `.$RankStats` summary statistics for ranks of each unit

- `.$Para` a list containing parameter values for each run

- `.$Nominal` the nominal scores and ranks of each unit (i.e. from the
  original COIN)

- `.$Sensitivity` (only if `SA_type = "SA"`) sensitivity indices for
  each parameter. Also confidence intervals if `Nboot`

- `.$coins` (only if `diagnostic_mode = TRUE`) a list of all coins
  generated during the sensitivity analysis

- Some information on the time elapsed, average time, and the parameters
  perturbed.

- Depending on the setting of `store_results`, may also contain a list
  of Methods or a list of COINs for each replication.

## Details

COINr implements a flexible variance-based global sensitivity analysis
approach, which allows almost any assumption to be varied, as long as
the distribution of alternative values can be described. Variance-based
"sensitivity indices" are estimated using a Monte Carlo design (running
the composite indicator many times with a particular combination of
input values). This follows the methodology described in
[doi:10.1111/j.1467-985X.2005.00350.x](https://doi.org/10.1111/j.1467-985X.2005.00350.x)
.

To understand how this function works, please see
[`vignette("sensitivity")`](https://bluefoxr.github.io/COINr/articles/sensitivity.md).
Here, we briefly recap the main input arguments.

First, you can select whether to run an uncertainty analysis
`SA_type = "UA"` or sensitivity analysis `SA_type = "SA"`. The number of
replications (regenerations of the coin) is specified by `N`. Keep in
mind that the *total* number of replications is `N` for an uncertainty
analysis but is `N*(d + 2)` for a sensitivity analysis due to the
experimental design used.

To run either types of analysis, you must specify *which* parts of the
coin to vary and *what the distributions/alternatives are* This is done
using `SA_specs`, a structured list. See
[`vignette("sensitivity")`](https://bluefoxr.github.io/COINr/articles/sensitivity.md)
for details and examples.

You also need to specify the target of the sensitivity analysis. This
should be an indicator or aggregate that can be found in one of the data
sets of the coin, and is specified using the `dset` and `iCode`
arguments.

If `SA_type = "SA"`, it is advisable to set `Nboot` to e.g. 100 or more,
which is the number of bootstrap samples to take when estimating
confidence intervals on sensitivity indices. This does *not* perform
extra regenerations of the coin, so setting this to a higher number
shouldn't have much impact on computational time.

If you want to understand what is going on more deeply in the
regenerated coins in the sensitivity analysis, set
`diagnostic_mode = TRUE` in `get_sensitivity()`. This will additionally
output a list containing every coin that was generated as part of the
sensitivity analysis, and allows you to check in detail whether the
coins are generated as you expect. Clearly it is better to run this on a
low sample size as the output can potentially become quite large.

This function replaces the now-defunct `sensitivity()` from COINr \<
v1.0.

## Examples

``` r
# for examples, see `vignette("sensitivity")`
# (this is because package examples are run automatically and this function can
# take a few minutes to run at realistic settings)
```
