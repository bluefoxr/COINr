# Screen units based on data availability

Screens units based on a data availability threshold and presence of
zeros. Units can be optionally "forced" to be included or excluded,
making exceptions for the data availability threshold.

## Usage

``` r
# S3 method for class 'purse'
Screen(
  x,
  dset,
  unit_screen,
  dat_thresh = NULL,
  nonzero_thresh = NULL,
  Force = NULL,
  write_to = NULL,
  ...
)
```

## Arguments

- x:

  A purse object

- dset:

  The data set to be checked/screened

- unit_screen:

  Specifies whether and how to screen units based on data availability
  or zero values.

  - If set to `"byNA"`, screens units with data availability below
    `dat_thresh`

  - If set to `"byzeros"`, screens units with non-zero values below
    `nonzero_thresh`

  - If set to `"byNAandzeros"`, screens units based on either of the
    previous two criteria being true.

- dat_thresh:

  A data availability threshold (`>= 1` and `<= 0`) used for flagging
  low data and screening units if `unit_screen != "none"`. Default 0.66.

- nonzero_thresh:

  As `dat_thresh` but for non-zero values. Defaults to 0.05, i.e. it
  will flag any units with less than 5% non-zero values (equivalently
  more than 95% zero values).

- Force:

  A data frame with any additional countries to force inclusion or
  exclusion. Required columns `uCode` (unit code(s)) and `Include`
  (logical: `TRUE` to include and `FALSE` to exclude). Specifications
  here override exclusion/inclusion based on data rules.

- write_to:

  If specified, writes the aggregated data to `.$Data[[write_to]]`.
  Default `write_to = "Screened"`.

- ...:

  arguments passed to or from other methods.

## Value

An updated purse with coins screened and updated.

## Details

The two main criteria of interest are `NA` values, and zeros. The
summary table gives percentages of `NA` values for each unit, across
indicators, and percentage zero values (*as a percentage of non-`NA`
values*). Each unit is flagged as having low data or too many zeros
based on thresholds.

See also
[`vignette("screening")`](https://bluefoxr.github.io/COINr/articles/screening.md).

## Examples

``` r
# see vignette("screening") for an example.
```
