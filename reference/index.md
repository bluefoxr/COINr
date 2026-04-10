# Package index

## Build

Modify and create new data sets

- [`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)
  : Create a new coin
- [`Screen()`](https://bluefoxr.github.io/COINr/reference/Screen.md) :
  Screen units based on data availability
- [`Denominate()`](https://bluefoxr.github.io/COINr/reference/Denominate.md)
  : Denominate data
- [`Impute()`](https://bluefoxr.github.io/COINr/reference/Impute.md) :
  Imputation of missing data
- [`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md) :
  Treat outliers
- [`qTreat()`](https://bluefoxr.github.io/COINr/reference/qTreat.md) :
  Quick outlier treatment
- [`Normalise()`](https://bluefoxr.github.io/COINr/reference/Normalise.md)
  : Normalise data
- [`qNormalise()`](https://bluefoxr.github.io/COINr/reference/qNormalise.md)
  : Quick normalisation
- [`Aggregate()`](https://bluefoxr.github.io/COINr/reference/Aggregate.md)
  : Aggregate data
- [`Custom()`](https://bluefoxr.github.io/COINr/reference/Custom.md) :
  Custom operation

## Analyse

Analyse data sets, summarise results and check sensitivity

- [`get_corr()`](https://bluefoxr.github.io/COINr/reference/get_corr.md)
  : Get correlations
- [`get_corr_flags()`](https://bluefoxr.github.io/COINr/reference/get_corr_flags.md)
  : Find highly-correlated indicators within groups
- [`get_cronbach()`](https://bluefoxr.github.io/COINr/reference/get_cronbach.md)
  : Cronbach's alpha
- [`get_data()`](https://bluefoxr.github.io/COINr/reference/get_data.md)
  : Get subsets of indicator data
- [`get_data_avail()`](https://bluefoxr.github.io/COINr/reference/get_data_avail.md)
  : Get data availability of units
- [`get_denom_corr()`](https://bluefoxr.github.io/COINr/reference/get_denom_corr.md)
  : Correlations between indicators and denominators
- [`get_eff_weights()`](https://bluefoxr.github.io/COINr/reference/get_eff_weights.md)
  : Get effective weights
- [`get_opt_weights()`](https://bluefoxr.github.io/COINr/reference/get_opt_weights.md)
  : Weight optimisation
- [`get_noisy_weights()`](https://bluefoxr.github.io/COINr/reference/get_noisy_weights.md)
  : Noisy replications of weights
- [`get_PCA()`](https://bluefoxr.github.io/COINr/reference/get_PCA.md) :
  Perform PCA on a coin
- [`get_pvals()`](https://bluefoxr.github.io/COINr/reference/get_pvals.md)
  : P-values for correlations in a data frame or matrix
- [`get_results()`](https://bluefoxr.github.io/COINr/reference/get_results.md)
  : Results summary tables
- [`get_sensitivity()`](https://bluefoxr.github.io/COINr/reference/get_sensitivity.md)
  : Sensitivity and uncertainty analysis of a coin
- [`get_stats()`](https://bluefoxr.github.io/COINr/reference/get_stats.md)
  : Statistics of columns/indicators
- [`get_str_weak()`](https://bluefoxr.github.io/COINr/reference/get_str_weak.md)
  : Generate strengths and weaknesses for a specified unit
- [`get_trends()`](https://bluefoxr.github.io/COINr/reference/get_trends.md)
  : Get time trends
- [`get_unit_summary()`](https://bluefoxr.github.io/COINr/reference/get_unit_summary.md)
  : Generate unit summary table
- [`remove_elements()`](https://bluefoxr.github.io/COINr/reference/remove_elements.md)
  : Check the effect of removing indicators or aggregates

## Plot

Plot indicators, distributions and others

- [`plot_bar()`](https://bluefoxr.github.io/COINr/reference/plot_bar.md)
  : Bar chart
- [`plot_corr()`](https://bluefoxr.github.io/COINr/reference/plot_corr.md)
  : Static heatmaps of correlation matrices
- [`plot_dist()`](https://bluefoxr.github.io/COINr/reference/plot_dist.md)
  : Static indicator distribution plots
- [`plot_dot()`](https://bluefoxr.github.io/COINr/reference/plot_dot.md)
  : Dot plots of single indicator with highlighting
- [`plot_framework()`](https://bluefoxr.github.io/COINr/reference/plot_framework.md)
  : Framework plots
- [`plot_scatter()`](https://bluefoxr.github.io/COINr/reference/plot_scatter.md)
  : Scatter plot of two variables
- [`plot_sensitivity()`](https://bluefoxr.github.io/COINr/reference/plot_sensitivity.md)
  : Plot sensitivity indices
- [`plot_uncertainty()`](https://bluefoxr.github.io/COINr/reference/plot_uncertainty.md)
  : Plot ranks from an uncertainty/sensitivity analysis

## Adjust and compare

Adjust coins, regenerate them and compare alternative versions

- [`Regen()`](https://bluefoxr.github.io/COINr/reference/Regen.md) :
  Regenerate a coin or purse
- [`change_ind()`](https://bluefoxr.github.io/COINr/reference/change_ind.md)
  : Add and remove indicators
- [`compare_coins()`](https://bluefoxr.github.io/COINr/reference/compare_coins.md)
  : Compare two coins
- [`compare_coins_multi()`](https://bluefoxr.github.io/COINr/reference/compare_coins_multi.md)
  : Compare multiple coins
- [`compare_coins_corr()`](https://bluefoxr.github.io/COINr/reference/compare_coins_corr.md)
  : Compare two coins by correlation

## Helpers

Helper functions to take care of common tasks

- [`import_coin_tool()`](https://bluefoxr.github.io/COINr/reference/import_COIN_tool.md)
  : Import data directly from COIN Tool
- [`COIN_to_coin()`](https://bluefoxr.github.io/COINr/reference/COIN_to_coin.md)
  : Convert a COIN to a coin
- [`build_example_coin()`](https://bluefoxr.github.io/COINr/reference/build_example_coin.md)
  : Build ASEM example coin
- [`build_example_purse()`](https://bluefoxr.github.io/COINr/reference/build_example_purse.md)
  : Build example purse
- [`export_to_excel()`](https://bluefoxr.github.io/COINr/reference/export_to_excel.md)
  : Export a coin or purse to Excel

## Other functions

Other functions including methods and small functions called by other
functions

- [`Aggregate(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Aggregate.coin.md)
  : Aggregate indicators in a coin
- [`Aggregate(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/Aggregate.data.frame.md)
  : Aggregate data frame
- [`Aggregate(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Aggregate.purse.md)
  : Aggregate indicators
- [`approx_df()`](https://bluefoxr.github.io/COINr/reference/approx_df.md)
  : Interpolate time-indexed data frame
- [`a_amean()`](https://bluefoxr.github.io/COINr/reference/a_amean.md) :
  Weighted arithmetic mean
- [`a_copeland()`](https://bluefoxr.github.io/COINr/reference/a_copeland.md)
  : Copeland scores
- [`a_gmean()`](https://bluefoxr.github.io/COINr/reference/a_gmean.md) :
  Weighted geometric mean
- [`a_hmean()`](https://bluefoxr.github.io/COINr/reference/a_hmean.md) :
  Weighted harmonic mean
- [`a_genmean()`](https://bluefoxr.github.io/COINr/reference/a_genmean.md)
  : Weighted generalised mean
- [`boxcox()`](https://bluefoxr.github.io/COINr/reference/boxcox.md) :
  Box Cox transformation
- [`CAGR()`](https://bluefoxr.github.io/COINr/reference/CAGR.md) :
  Compound annual growth rate
- [`check_iData()`](https://bluefoxr.github.io/COINr/reference/check_iData.md)
  : Check iData
- [`check_iMeta()`](https://bluefoxr.github.io/COINr/reference/check_iMeta.md)
  : Check iMeta
- [`check_SkewKurt()`](https://bluefoxr.github.io/COINr/reference/check_SkewKurt.md)
  : Check skew and kurtosis of a vector
- [`compare_df()`](https://bluefoxr.github.io/COINr/reference/compare_df.md)
  : Compare two data frames
- [`Custom(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Custom.coin.md)
  : Custom operation
- [`Custom(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Custom.purse.md)
  : Custom operation
- [`Denominate(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Denominate.coin.md)
  : Denominate data set in a coin
- [`Denominate(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/Denominate.data.frame.md)
  : Denominate data sets by other variables
- [`Denominate(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Denominate.purse.md)
  : Denominate a data set within a purse.
- [`export_to_excel(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/export_to_excel.coin.md)
  : Export a coin to Excel
- [`export_to_excel(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/export_to_excel.purse.md)
  : Export a purse to Excel
- [`get_data(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/get_data.coin.md)
  : Get subsets of indicator data
- [`get_data(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/get_data.purse.md)
  : Get subsets of indicator data
- [`get_data_avail(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/get_data_avail.coin.md)
  : Get data availability of units
- [`get_data_avail(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/get_data_avail.data.frame.md)
  : Get data availability of units
- [`get_dset(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/get_dset.coin.md)
  : Gets a named data set and performs checks
- [`get_dset(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/get_dset.purse.md)
  : Gets a named data set and performs checks
- [`get_dset()`](https://bluefoxr.github.io/COINr/reference/get_dset.md)
  : Gets a named data set and performs checks
- [`get_stats(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/get_stats.coin.md)
  : Statistics of indicators
- [`get_stats(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/get_stats.data.frame.md)
  : Statistics of columns
- [`Impute(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Impute.coin.md)
  : Impute a data set in a coin
- [`Impute(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/Impute.data.frame.md)
  : Impute a data frame
- [`Impute(`*`<numeric>`*`)`](https://bluefoxr.github.io/COINr/reference/Impute.numeric.md)
  : Impute a numeric vector
- [`Impute(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Impute.purse.md)
  : Impute data sets in a purse
- [`impute_panel()`](https://bluefoxr.github.io/COINr/reference/impute_panel.md)
  : Impute panel data
- [`is.coin()`](https://bluefoxr.github.io/COINr/reference/is.coin.md) :
  Check if object is coin class
- [`is.purse()`](https://bluefoxr.github.io/COINr/reference/is.purse.md)
  : Check if object is purse class
- [`i_mean()`](https://bluefoxr.github.io/COINr/reference/i_mean.md) :
  Impute by mean
- [`i_mean_grp()`](https://bluefoxr.github.io/COINr/reference/i_mean_grp.md)
  : Impute by group mean
- [`i_median()`](https://bluefoxr.github.io/COINr/reference/i_median.md)
  : Impute by median
- [`i_median_grp()`](https://bluefoxr.github.io/COINr/reference/i_median_grp.md)
  : Impute by group median
- [`icodes_to_inames()`](https://bluefoxr.github.io/COINr/reference/icodes_to_inames.md)
  : Convert iCodes to iNames
- [`kurt()`](https://bluefoxr.github.io/COINr/reference/kurt.md) :
  Calculate kurtosis
- [`log_CT()`](https://bluefoxr.github.io/COINr/reference/log_CT.md) :
  Log-transform a vector
- [`log_CT_plus()`](https://bluefoxr.github.io/COINr/reference/log_CT_plus.md)
  : Log transform a vector (skew corrected)
- [`log_CT_orig()`](https://bluefoxr.github.io/COINr/reference/log_CT_orig.md)
  : Log-transform a vector
- [`log_GII()`](https://bluefoxr.github.io/COINr/reference/log_GII.md) :
  Log-transform a vector
- [`names_to_codes()`](https://bluefoxr.github.io/COINr/reference/names_to_codes.md)
  : Generate short codes from long names
- [`Normalise(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Normalise.coin.md)
  : Create a normalised data set
- [`Normalise(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/Normalise.data.frame.md)
  : Normalise a data frame
- [`Normalise(`*`<numeric>`*`)`](https://bluefoxr.github.io/COINr/reference/Normalise.numeric.md)
  : Normalise a numeric vector
- [`Normalise(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Normalise.purse.md)
  : Create normalised data sets in a purse of coins
- [`n_borda()`](https://bluefoxr.github.io/COINr/reference/n_borda.md) :
  Normalise using Borda scores
- [`n_dist2max()`](https://bluefoxr.github.io/COINr/reference/n_dist2max.md)
  : Normalise as distance to maximum value
- [`n_dist2ref()`](https://bluefoxr.github.io/COINr/reference/n_dist2ref.md)
  : Normalise as distance to reference value
- [`n_dist2targ()`](https://bluefoxr.github.io/COINr/reference/n_dist2targ.md)
  : Normalise as distance to target
- [`n_fracmax()`](https://bluefoxr.github.io/COINr/reference/n_fracmax.md)
  : Normalise as fraction of max value
- [`n_goalposts()`](https://bluefoxr.github.io/COINr/reference/n_goalposts.md)
  : Normalise using goalpost method
- [`n_minmax()`](https://bluefoxr.github.io/COINr/reference/n_minmax.md)
  : Minmax a vector
- [`n_prank()`](https://bluefoxr.github.io/COINr/reference/n_prank.md) :
  Normalise using percentile ranks
- [`n_rank()`](https://bluefoxr.github.io/COINr/reference/n_rank.md) :
  Normalise using ranks
- [`n_scaled()`](https://bluefoxr.github.io/COINr/reference/n_scaled.md)
  : Scale a vector
- [`n_zscore()`](https://bluefoxr.github.io/COINr/reference/n_zscore.md)
  : Z-score a vector
- [`outrankMatrix()`](https://bluefoxr.github.io/COINr/reference/outrankMatrix.md)
  : Outranking matrix
- [`prc_change()`](https://bluefoxr.github.io/COINr/reference/prc_change.md)
  : Percentage change of time series
- [`print(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/print.COIN.md)
  : Print coin
- [`print(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/print.purse.md)
  : Print purse
- [`qNormalise(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/qNormalise.coin.md)
  : Quick normalisation of a coin
- [`qNormalise(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/qNormalise.data.frame.md)
  : Quick normalisation of a data frame
- [`qNormalise(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/qNormalise.purse.md)
  : Quick normalisation of a purse
- [`qTreat(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/qTreat.coin.md)
  : Quick outlier treatment of a coin
- [`qTreat(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/qTreat.data.frame.md)
  : Quick outlier treatment of a data frame
- [`qTreat(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/qTreat.purse.md)
  : Quick outlier treatment of a purse
- [`rank_df()`](https://bluefoxr.github.io/COINr/reference/rank_df.md) :
  Convert a data frame to ranks
- [`Regen(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Regen.coin.md)
  : Regenerate a coin
- [`Regen(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Regen.purse.md)
  : Regenerate a purse
- [`replace_df()`](https://bluefoxr.github.io/COINr/reference/replace_df.md)
  : Replace multiple values in a data frame
- [`round_df()`](https://bluefoxr.github.io/COINr/reference/round_df.md)
  : Round down a data frame
- [`SA_estimate()`](https://bluefoxr.github.io/COINr/reference/SA_estimate.md)
  : Estimate sensitivity indices
- [`SA_sample()`](https://bluefoxr.github.io/COINr/reference/SA_sample.md)
  : Generate sample for sensitivity analysis
- [`Screen(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Screen.coin.md)
  : Screen units based on data availability
- [`Screen(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/Screen.data.frame.md)
  : Screen units based on data availability
- [`Screen(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Screen.purse.md)
  : Screen units based on data availability
- [`signif_df()`](https://bluefoxr.github.io/COINr/reference/signif_df.md)
  : Round a data frame to specified significant figures
- [`skew()`](https://bluefoxr.github.io/COINr/reference/skew.md) :
  Calculate skewness
- [`Treat(`*`<coin>`*`)`](https://bluefoxr.github.io/COINr/reference/Treat.coin.md)
  : Treat a data set in a coin for outliers
- [`Treat(`*`<data.frame>`*`)`](https://bluefoxr.github.io/COINr/reference/Treat.data.frame.md)
  : Treat a data frame for outliers
- [`Treat(`*`<numeric>`*`)`](https://bluefoxr.github.io/COINr/reference/Treat.numeric.md)
  : Treat a numeric vector for outliers
- [`Treat(`*`<purse>`*`)`](https://bluefoxr.github.io/COINr/reference/Treat.purse.md)
  : Treat a purse of coins for outliers
- [`ucodes_to_unames()`](https://bluefoxr.github.io/COINr/reference/ucodes_to_unames.md)
  : Convert uCodes to uNames
- [`winsorise()`](https://bluefoxr.github.io/COINr/reference/winsorise.md)
  : Winsorise a vector

## Data

Built in data sets used in examples and testing.

- [`ASEM_COIN`](https://bluefoxr.github.io/COINr/reference/ASEM_COIN.md)
  : ASEM COIN (COINr \< v1.0)
- [`ASEM_iData`](https://bluefoxr.github.io/COINr/reference/ASEM_iData.md)
  : ASEM raw indicator data
- [`ASEM_iData_p`](https://bluefoxr.github.io/COINr/reference/ASEM_iData_p.md)
  : ASEM raw panel data
- [`ASEM_iMeta`](https://bluefoxr.github.io/COINr/reference/ASEM_iMeta.md)
  : ASEM indicator metadata
- [`WorldDenoms`](https://bluefoxr.github.io/COINr/reference/WorldDenoms.md)
  : World denomination data
