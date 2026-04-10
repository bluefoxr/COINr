# Statistics of indicators

Given a coin and a specified data set (`dset`), returns a table of
statistics with entries for each column.

## Usage

``` r
# S3 method for class 'coin'
get_stats(
  x,
  dset,
  t_skew = 2,
  t_kurt = 3.5,
  t_avail = 0.65,
  t_zero = 0.5,
  t_unq = 0.5,
  nsignif = 3,
  out2 = "df",
  ...
)
```

## Arguments

- x:

  A coin

- dset:

  A data set present in `.$Data`

- t_skew:

  Absolute skewness threshold. See details.

- t_kurt:

  Kurtosis threshold. See details.

- t_avail:

  Data availability threshold. See details.

- t_zero:

  A threshold between 0 and 1 for flagging indicators with high
  proportion of zeroes. See details.

- t_unq:

  A threshold between 0 and 1 for flagging indicators with low
  proportion of unique values. See details.plot

- nsignif:

  Number of significant figures to round the output table to.

- out2:

  Either `"df"` (default) to output a data frame of indicator
  statistics, or "`coin`" to output an updated coin with the data frame
  attached under `.$Analysis`.

- ...:

  arguments passed to or from other methods.

## Value

Either a data frame or updated coin - see `out2`.

## Details

The statistics (columns in the output table) are as follows (entries
correspond to each column):

- `Min`: the minimum

- `Max`: the maximum

- `Mean`: the (arirthmetic) mean

- `Median`: the median

- `Std`: the standard deviation

- `Skew`: the skew

- `Kurt`: the kurtosis

- `N.Avail`: the number of non-`NA` values

- `N.NonZero`: the number of non-zero values

- `N.Unique`: the number of unique values

- `Frc.Avail`: the fraction of non-`NA` values

- `Frc.NonZero`: the fraction of non-zero values

- `Frc.Unique`: the fraction of unique values

- `Flag.Avail`: a data availability flag - columns with
  `Frc.Avail < t_avail` will be flagged as `"LOW"`, else `"ok"`.

- `Flag.NonZero`: a flag for columns with a high proportion of zeros.
  Any columns with `Frc.NonZero < t_zero` are flagged as `"LOW"`,
  otherwise `"ok"`.

- `Flag.Unique`: a unique value flag - any columns with
  `Frc.Unique < t_unq` are flagged as `"LOW"`, otherwise `"ok"`.

- `Flag.SkewKurt`: a skew and kurtosis flag which is an indication of
  possible outliers. Any columns with `abs(Skew) > t_skew` AND
  `Kurt > t_kurt` are flagged as `"OUT"`, otherwise `"ok"`.

The aim of this table, among other things, is to check the basic
statistics of each column/indicator, and identify any possible issues
for each indicator. For example, low data availability, having a high
proportion of zeros and/or a low proportion of unique values. Further,
the combination of skew and kurtosis (i.e. the `Flag.SkewKurt` column)
is a simple test for possible outliers, which may require treatment
using [`Treat()`](https://bluefoxr.github.io/COINr/reference/Treat.md).

The table can be returned either to the coin or as a standalone data
frame - see `out2`.

See also
[`vignette("analysis")`](https://bluefoxr.github.io/COINr/articles/analysis.md).

## Examples

``` r
# build example coin
coin <-  build_example_coin(up_to = "new_coin", quietly = TRUE)

# get table of indicator statistics for raw data set
get_stats(coin, dset = "Raw", out2 = "df")
#>        iCode       Min      Max     Mean   Median      Std    Skew    Kurt
#> 1        LPI   2.07000     4.23 3.41e+00    3.420 5.38e-01 -0.3040 -0.6570
#> 2    Flights   0.99000   211.00 3.86e+01   25.600 4.67e+01  2.1000  4.5100
#> 3       Ship   0.00000    21.20 1.20e+01   12.700 6.84e+00 -0.5760 -0.6810
#> 4       Bord   0.00000   122.00 2.36e+01   18.000 2.48e+01  2.1500  5.7900
#> 5       Elec   0.00000   110.00 1.62e+01    6.910 2.27e+01  2.2300  5.7900
#> 6        Gas   0.00841    94.80 1.00e+01    1.140 1.75e+01  2.8300 10.3000
#> 7   ConSpeed   5.50000    28.60 1.43e+01   14.600 5.11e+00  0.4620  0.1870
#> 8      Cov4G   0.00000   100.00 7.56e+01   91.000 3.21e+01 -1.3700  0.5420
#> 9      Goods   7.23000  1920.00 2.90e+02  143.000 3.88e+02  2.6500  8.2700
#> 10  Services   1.38000   657.00 1.29e+02   54.100 1.61e+02  1.7000  2.3800
#> 11       FDI   0.13000    75.60 1.25e+01    6.100 1.58e+01  2.1000  4.8900
#> 12    PRemit   0.16600    30.20 6.56e+00    4.690 7.37e+00  1.8100  2.9400
#> 13   ForPort   0.00189 10600.00 1.58e+03  259.000 2.59e+03  2.0200  3.3000
#> 14      Embs  28.00000   100.00 7.09e+01   75.000 2.01e+01 -0.3680 -0.9930
#> 15      IGOs  82.00000   329.00 1.98e+02  197.000 5.86e+01 -0.0635 -0.2490
#> 16    UNVote  35.80000    43.20 4.09e+01   42.500 2.38e+00 -0.6750 -1.2000
#> 17 CostImpEx   0.00000   992.00 1.25e+02   45.000 1.82e+02  2.6900  9.8400
#> 18    Tariff   0.00000    10.50 2.46e+00    1.600 2.38e+00  2.4500  5.4100
#> 19      TBTs   1.00000  1750.00 7.95e+02 1140.000 5.50e+02 -0.3960 -1.5500
#> 20    TIRcon   0.00000     1.00 7.45e-01    1.000 4.40e-01 -1.1600 -0.6850
#> 21      RTAs   1.00000    46.00 2.39e+01   30.000 9.38e+00 -0.6780  0.1330
#> 22      Visa   1.00000    92.00 6.92e+01   79.000 2.22e+01 -1.7600  2.1700
#> 23     StMob   1.77000   445.00 6.41e+01   33.200 8.66e+01  2.7300  8.3100
#> 24  Research 175.00000 96300.00 1.63e+04 7730.000 2.31e+04  2.2200  4.8100
#> 25       Pat   0.30000  2770.00 3.34e+02  113.000 5.27e+02  2.8500 10.1000
#> 26  CultServ   0.00266     9.57 1.45e+00    0.578 2.09e+00  2.4700  6.3900
#> 27  CultGood   0.04600    74.50 1.00e+01    3.300 1.50e+01  2.6100  7.7400
#> 28   Tourist   0.12500    82.60 1.51e+01    9.200 1.84e+01  2.1900  4.9000
#> 29  MigStock   0.08170    10.90 2.48e+00    1.400 2.53e+00  1.5400  2.0600
#> 30      Lang   0.01880    21.50 9.31e+00    9.100 6.35e+00  0.0934 -1.0800
#> 31     Renew   0.01490    64.90 2.27e+01   17.200 1.72e+01  0.8220 -0.0661
#> 32  PrimEner  53.50000   192.00 1.06e+02   96.600 3.54e+01  0.7500 -0.1470
#> 33       CO2   0.29700    22.10 6.71e+00    5.940 4.55e+00  1.1100  1.8300
#> 34    MatCon   2.58000    38.40 1.60e+01   15.000 8.21e+00  0.7590  0.4240
#> 35    Forest   0.32600    31.80 5.95e+00    4.980 5.63e+00  2.7100  9.3500
#> 36   Poverty   0.00000    22.70 2.44e+00    0.400 5.51e+00  2.9200  7.8800
#> 37     Palma   0.88000     2.62 1.34e+00    1.250 3.75e-01  1.3300  2.3000
#> 38  TertGrad   2.01000    37.60 2.19e+01   23.300 8.52e+00 -0.3830 -0.3030
#> 39 FreePress   8.00000    87.00 3.84e+01   28.000 2.44e+01  0.7540 -0.7800
#> 40    TolMin   1.10000     9.80 5.39e+00    5.100 2.15e+00  0.0633 -0.6600
#> 41      NGOs   0.00000  1820.00 2.20e+02   66.000 3.87e+02  2.7900  7.9200
#> 42       CPI  21.00000    89.00 5.74e+01   57.000 1.98e+01  0.0203 -1.2500
#> 43    FemLab   0.29700     1.03 8.18e-01    0.863 1.41e-01 -2.0600  5.0200
#> 44   WomParl   4.86000    43.60 2.44e+01   23.700 9.72e+00  0.0859 -0.7890
#> 45   PubDebt   2.81000   248.00 6.30e+01   52.100 4.37e+01  2.0000  5.9700
#> 46  PrivDebt  19.70000   421.00 1.40e+02  128.000 8.49e+01  0.9570  1.4300
#> 47   GDPGrow  -0.67700     7.58 3.22e+00    3.530 2.04e+00 -0.0516 -0.9300
#> 48     RDExp   0.08470     4.23 1.50e+00    1.290 1.02e+00  0.6100 -0.2760
#> 49      NEET   0.60000    42.10 1.26e+01   10.900 7.74e+00  1.6000  3.6300
#>    N.Avail N.NonZero N.Unique N.Same Frc.Avail Frc.NonZero Frc.Unique Frc.Same
#> 1       51        51       51      1     1.000       1.000     1.0000   0.0196
#> 2       51        51       51      1     1.000       1.000     1.0000   0.0196
#> 3       51        42       43      9     1.000       0.824     0.8430   0.1760
#> 4       51        45       30      6     1.000       0.882     0.5880   0.1180
#> 5       51        47       46      4     1.000       0.922     0.9020   0.0784
#> 6       51        51       51      1     1.000       1.000     1.0000   0.0196
#> 7       43        43       40      2     0.843       1.000     0.9300   0.0465
#> 8       51        49       37      4     1.000       0.961     0.7250   0.0784
#> 9       51        51       51      1     1.000       1.000     1.0000   0.0196
#> 10      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 11      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 12      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 13      48        48       48      1     0.941       1.000     1.0000   0.0208
#> 14      51        51       33      4     1.000       1.000     0.6470   0.0784
#> 15      51        51       46      3     1.000       1.000     0.9020   0.0588
#> 16      51        51       49      3     1.000       1.000     0.9610   0.0588
#> 17      51        33       29     18     1.000       0.647     0.5690   0.3530
#> 18      48        46       20     28     0.941       0.958     0.4170   0.5830
#> 19      50        50       42      4     0.980       1.000     0.8400   0.0800
#> 20      51        38        2     38     1.000       0.745     0.0392   0.7450
#> 21      51        51       14     28     1.000       1.000     0.2750   0.5490
#> 22      51        51       22     11     1.000       1.000     0.4310   0.2160
#> 23      51        51       50      2     1.000       1.000     0.9800   0.0392
#> 24      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 25      45        45       45      1     0.882       1.000     1.0000   0.0222
#> 26      47        47       47      1     0.922       1.000     1.0000   0.0213
#> 27      44        44       44      1     0.863       1.000     1.0000   0.0227
#> 28      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 29      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 30      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 31      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 32      50        50       50      1     0.980       1.000     1.0000   0.0200
#> 33      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 34      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 35      51        51       51      1     1.000       1.000     1.0000   0.0196
#> 36      44        30       17     14     0.863       0.682     0.3860   0.3180
#> 37      45        45       35      3     0.882       1.000     0.7780   0.0667
#> 38      44        44       44      1     0.863       1.000     1.0000   0.0227
#> 39      51        51       38      3     1.000       1.000     0.7450   0.0588
#> 40      51        51       42      3     1.000       1.000     0.8240   0.0588
#> 41      51        50       47      3     1.000       0.980     0.9220   0.0588
#> 42      51        51       39      3     1.000       1.000     0.7650   0.0588
#> 43      49        49       49      1     0.961       1.000     1.0000   0.0204
#> 44      51        51       49      2     1.000       1.000     0.9610   0.0392
#> 45      50        50       50      1     0.980       1.000     1.0000   0.0200
#> 46      50        50       50      1     0.980       1.000     1.0000   0.0200
#> 47      51        49       50      2     1.000       0.961     0.9800   0.0392
#> 48      47        47       47      1     0.922       1.000     1.0000   0.0213
#> 49      49        49       40      3     0.961       1.000     0.8160   0.0612
#>    Flag.Avail Flag.NonZero Flag.Unique Flag.SkewKurt
#> 1          ok           ok          ok            ok
#> 2          ok           ok          ok           OUT
#> 3          ok           ok          ok            ok
#> 4          ok           ok          ok           OUT
#> 5          ok           ok          ok           OUT
#> 6          ok           ok          ok           OUT
#> 7          ok           ok          ok            ok
#> 8          ok           ok          ok            ok
#> 9          ok           ok          ok           OUT
#> 10         ok           ok          ok            ok
#> 11         ok           ok          ok           OUT
#> 12         ok           ok          ok            ok
#> 13         ok           ok          ok            ok
#> 14         ok           ok          ok            ok
#> 15         ok           ok          ok            ok
#> 16         ok           ok          ok            ok
#> 17         ok           ok          ok           OUT
#> 18         ok           ok         LOW           OUT
#> 19         ok           ok          ok            ok
#> 20         ok           ok         LOW            ok
#> 21         ok           ok         LOW            ok
#> 22         ok           ok         LOW            ok
#> 23         ok           ok          ok           OUT
#> 24         ok           ok          ok           OUT
#> 25         ok           ok          ok           OUT
#> 26         ok           ok          ok           OUT
#> 27         ok           ok          ok           OUT
#> 28         ok           ok          ok           OUT
#> 29         ok           ok          ok            ok
#> 30         ok           ok          ok            ok
#> 31         ok           ok          ok            ok
#> 32         ok           ok          ok            ok
#> 33         ok           ok          ok            ok
#> 34         ok           ok          ok            ok
#> 35         ok           ok          ok           OUT
#> 36         ok           ok         LOW           OUT
#> 37         ok           ok          ok            ok
#> 38         ok           ok          ok            ok
#> 39         ok           ok          ok            ok
#> 40         ok           ok          ok            ok
#> 41         ok           ok          ok           OUT
#> 42         ok           ok          ok            ok
#> 43         ok           ok          ok           OUT
#> 44         ok           ok          ok            ok
#> 45         ok           ok          ok            ok
#> 46         ok           ok          ok            ok
#> 47         ok           ok          ok            ok
#> 48         ok           ok          ok            ok
#> 49         ok           ok          ok            ok
```
