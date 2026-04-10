# Treat a data frame for outliers

Operates a two-stage data treatment process, based on two data treatment
functions, and a pass/fail function which detects outliers. The method
of data treatment can be either specified by the `global_specs` argument
(which applies the same specifications to all columns in `x`), or else
(additionally) by the `indiv_specs` argument which allows different
methods to be applied for each column. See details. For a simpler
function for data treatment, see the wrapper function
[`qTreat()`](https://bluefoxr.github.io/COINr/reference/qTreat.md).

## Usage

``` r
# S3 method for class 'data.frame'
Treat(x, global_specs = NULL, indiv_specs = NULL, combine_treat = FALSE, ...)
```

## Arguments

- x:

  A data frame. Can have both numeric and non-numeric columns.

- global_specs:

  A list specifying the treatment to apply to all columns. This will be
  applied to all columns, except any that are specified in the
  `indiv_specs` argument. Alternatively, set to `"none"` to apply no
  treatment. See details.

- indiv_specs:

  A list specifying any individual treatment to apply to specific
  columns, overriding `global_specs` for those columns. See details.

- combine_treat:

  By default, if `f1` fails to pass `f_pass`, then `f2` is applied to
  the original `x`, rather than the treated output of `f1`. If
  `combine_treat = TRUE`, `f2` will instead be applied to the output of
  `f1`, so the two treatments will be combined.

- ...:

  arguments passed to or from other methods.

## Value

A treated data frame of data

## Global specifications

If the same method of data treatment should be applied to all the
columns, use the `global_specs` argument. This argument takes a
structured list which looks like this:

    global_specs = list(f1 = .,
                        f1_para = list(.),
                        f2 = .,
                        f2_para = list(.),
                        f_pass = .,
                        f_pass_para = list()
                        )

The entries in this list correspond to arguments in
[`Treat.numeric()`](https://bluefoxr.github.io/COINr/reference/Treat.numeric.md),
and the meanings of each are also described in more detail here below.
In brief, `f1` is the name of a function to apply at the first round of
data treatment, `f1_para` is a list of any additional parameters to pass
to `f1`, `f2` and `f2_para` are equivalently the function name and
parameters of the second round of data treatment, and `f_pass` and
`f_pass_para` are the function and additional arguments to check for the
existence of outliers.

The default values for `global_specs` are as follows:

    global_specs = list(f1 = "winsorise",
                         f1_para = list(na.rm = TRUE,
                                        winmax = 5,
                                        skew_thresh = 2,
                                        kurt_thresh = 3.5,
                                        force_win = FALSE),
                         f2 = "log_CT",
                         f2_para = list(na.rm = TRUE),
                         f_pass = "check_SkewKurt",
                         f_pass_para = list(na.rm = TRUE,
                                            skew_thresh = 2,
                                            kurt_thresh = 3.5))

This shows that by default (i.e. if `global_specs` is not specified),
each column is checked for outliers by the
[`check_SkewKurt()`](https://bluefoxr.github.io/COINr/reference/check_SkewKurt.md)
function, which uses skew and kurtosis thresholds as its parameters.
Then, if outliers exist, the first function
[`winsorise()`](https://bluefoxr.github.io/COINr/reference/winsorise.md)
is applied, which also uses skew and kurtosis parameters, as well as a
maximum number of winsorised points. If the Winsorisation function does
not satisfy `f_pass`, the
[`log_CT()`](https://bluefoxr.github.io/COINr/reference/log_CT.md)
function is invoked.

To change the global specifications, you don't have to supply the whole
list. If, for example, you are happy with all the defaults but want to
simply change the maximum number of Winsorised points, you could specify
e.g. `global_specs = list(f1_para = list(winmax = 3))`. In other words,
a subset of the list can be specified, as long as the structure of the
list is correct.

## Individual specifications

The `indiv_specs` argument allows different specifications for each
column in `x`. This is done by wrapping multiple lists of the format of
the list described in `global_specs` into one single list, named
according to the column names of `x`. For example, if `x` has column
names "x1", "x2" and "x3", we could specify individual treatment as
follows:

    indiv_specs = list(x1 = list(.),
                       x2 = list(.)
                       x3 = list(.))

where each `list(.)` is a specifications list of the same format as
`global_specs`. Any columns that are not named in `indiv_specs` are
treated using the specifications from `global_specs` (which will be the
defaults if it is not specified). As with `global_specs`, a subset of
the `global_specs` list may be specified for each entry. Additionally,
as a special case, specifying a list entry as e.g. `x1 = "none"` will
apply no data treatment to the column "x1". See
[`vignette("treat")`](https://bluefoxr.github.io/COINr/articles/treat.md)
for examples of individual treatment.

## Function methodology

This function is set up to allow any functions to be passed as the data
treatment functions (`f1` and `f2`), as well as any function to be
passed as the outlier detection function `f_pass`, as specified in the
`global_specs` and `indiv_specs` arguments.

The arrangement of this function is inspired by a fairly standard data
treatment process applied to indicators, which consists of checking skew
and kurtosis, then if the criteria are not met, applying Winsorisation
up to a specified limit. Then if Winsorisation still does not bring skew
and kurtosis within limits, applying a nonlinear transformation such as
log or Box-Cox.

This function generalises this process by using the following general
steps:

1.  Check if variable passes or fails using `f_pass`

2.  If `f_pass` returns `FALSE`, apply `f1`, else return `x` unmodified

3.  Check again using \*`f_pass`

4.  If `f_pass` still returns `FALSE`, apply `f2`

5.  Return the modified `x` as well as other information.

For the "typical" case described above `f1` is a Winsorisation function,
`f2` is a nonlinear transformation and `f_pass` is a skew and kurtosis
check. Parameters can be passed to each of these three functions in a
named list, for example to specify a maximum number of points to
Winsorise, or Box-Cox parameters, or anything else. The constraints are
that:

- All of `f1`, `f2` and `f_pass` must follow the format
  `function(x, f_para)`, where `x` is a numerical vector, and `f_para`
  is a list of other function parameters to be passed to the function,
  which is specified by `f1_para` for `f1` and similarly for the other
  functions. If the function has no parameters other than `x`, then
  `f_para` can be omitted.

- `f1` and `f2` should return either a list with `.$x` as the modified
  numerical vector, and any other information to be attached to the
  list, OR, simply `x` as the only output.

- `f_pass` must return a logical value, where `TRUE` indicates that the
  `x` passes the criteria (and therefore doesn't need any (more)
  treatment), and `FALSE` means that it fails to meet the criteria.

See also
[`vignette("treat")`](https://bluefoxr.github.io/COINr/articles/treat.md).

## Examples

``` r
# select three indicators
df1 <- ASEM_iData[c("Flights", "Goods", "Services")]

# treat the data frame using defaults
l_treat <- Treat(df1)

# details of data treatment for each column
l_treat$Dets_Table
#>      iCode check_SkewKurt0.Pass check_SkewKurt0.Skew check_SkewKurt0.Kurt
#> 1  Flights                FALSE             2.103287             4.508879
#> 2    Goods                FALSE             2.649973             8.266610
#> 3 Services                 TRUE             1.701085             2.375656
#>   winsorise.nwin check_SkewKurt1.Pass check_SkewKurt1.Skew check_SkewKurt1.Kurt
#> 1              1                 TRUE             1.900658            3.3360647
#> 2              2                 TRUE             1.140608            0.1572047
#> 3             NA                   NA                   NA                   NA
```
