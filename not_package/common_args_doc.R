# This is a list of documentation of common arguments in COINr. At the moment to manually copy and paste,
# maybe one day I figure out how to do this in roxygen2.

#' @param x The coin object
#' @param x A purse object
#'
#' @param dset The name of the data set to apply the function to, which should be accessible in `.$Data`.
#'
#'
#' @param iCodes Optional indicator codes to retrieve. If `NULL` (default), returns all iCodes found in
#' the selected data set. Can also refer to indicator groups. See details.
#' @param Level Optionally, the level in the hierarchy to extract data from. See details.
#'
#' @param uCodes Optional unit codes to filter rows of the resulting data set. Can also be used in conjunction
#' with groups. See details.
#' @param use_group Optional group to filter rows of the data set. Specified as `list(Group_Var = Group)`,
#' where `Group_Var` is a Group_ column that must be present in the selected data set, and `Group` is a specified group
#' inside that grouping variable. This filters the selected data to only include rows from the specified group. Can
#' also be used in conjunction with `uCodes` -- see details.
#'
#' @param also_get A character vector specifying any columns to attach to the data set that are *not*
#' indicators or aggregates. These will be e.g. `uName`, groups, denominators or columns labelled as "Other"
#' in `iMeta`. These columns are stored in `.$Meta$Unit` to avoid repetition. Set `also_get = "all"` to
#' attach all columns, or set `also_get = "none"` to return only numeric columns, i.e. no `uCode` column.
#'
#' @param Time Optional time index to extract from a subset of the coins present in the purse. Should be a
#' vector containing one or more entries in `x$Time` or `NULL` to return all (default).
#'
#' @param out2 Either `"coin"` (default) to return updated coin or `"df"` to output the aggregated data set.
#' @param write_to If specified, writes the aggregated data to `.$Data[[write_to]]`. Default `write_to = "Aggregated"`.
#' @param ... arguments passed to or from other methods
