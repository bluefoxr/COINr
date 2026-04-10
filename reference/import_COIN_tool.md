# Import data directly from COIN Tool

The [COIN
Tool](https://knowledge4policy.ec.europa.eu/composite-indicators/coin-tool_en)
is an Excel-based tool for building composite indicators. This function
provides a direct interface for reading a COIN Tool input deck and
converting it to COINr. You need to provide a COIN Tool file, with the
"Database" sheet properly compiled.

## Usage

``` r
import_coin_tool(fname, makecodes = FALSE, oldtool = FALSE, out2 = "list")
```

## Arguments

- fname:

  The file name and path to read, e.g.
  `"C:/Documents/COINToolFile.xlsx"`.

- makecodes:

  Logical: if `TRUE`, will generate short indicator codes based on
  indicator names, otherwise if `FALSE`, will use COIN Tool indicator
  codes `"Ind.01"`, etc. Currently only does this for indicators, not
  aggregation groups.

- oldtool:

  Logical: if `TRUE`, compatible with old COIN Tool (pre-release, early
  2019 or earlier). There are some minor differences on where the
  elements are found.

- out2:

  Either `"list"` (default) to output a list with `iData` and `iMeta`
  entries (for input into
  [`new_coin()`](https://bluefoxr.github.io/COINr/reference/new_coin.md)),
  else `"coin"` to output a coin.

## Value

Either a list or a coin, depending on `out2`

## Details

This function replaces the now-defunct `COINToolIn()` from COINr \<
v1.0.

## Examples

``` r
if (FALSE) { # \dontrun{
## This example downloads a COIN Tool spreadsheet containing example data,
## saves it to a temporary directory, unzips, and reads into R. Finally it
## assembles it into a COIN.

# Make temp zip filename in temporary directory
tmpz <- tempfile(fileext = ".zip")
# Download an example COIN Tool file to temporary directory
# NOTE: the download.file() command may need its "method" option set to a
# specific value depending on the platform you run this on. You can also
# choose to download/unzip this file manually.
download.file("https://knowledge4policy.ec.europa.eu/sites/default/
files/coin_tool_v1_lite_exampledata.zip", tmpz)
# Unzip
CTpath <- unzip(tmpz, exdir = tempdir())
# Read COIN Tool into R
l <- import_coin_tool(CTpath, makecodes = TRUE) } # }
```
