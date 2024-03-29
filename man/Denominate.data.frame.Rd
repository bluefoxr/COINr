% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/denominate.R
\name{Denominate.data.frame}
\alias{Denominate.data.frame}
\title{Denominate data sets by other variables}
\usage{
\method{Denominate}{data.frame}(
  x,
  denoms,
  denomby,
  x_ID = NULL,
  denoms_ID = NULL,
  f_denom = NULL,
  ...
)
}
\arguments{
\item{x}{A data frame of data to be denominated. Columns to be denominated must be numeric, but any columns not
specified in \code{denomby} will be ignored. \code{x} must also contain an ID column specified by \code{x_ID} to match rows with
\code{denoms}.}

\item{denoms}{A data frame of denominator data. Columns should be denominator data, with column names corresponding
to entries in \code{denomby}. This must also include an ID column identified by \code{denoms_ID} to match rows.}

\item{denomby}{A data frame which specifies which denominators to use for each indicator, and any scaling factors
to apply. Should have columns \code{iCode}, \code{Denominator}, \code{ScaleFactor}. \code{iCode} specifies a column name from \code{x},
\code{Denominator} specifies a column name from \code{denoms} to use to denominate the corresponding column from \code{x}.
\code{ScaleFactor} allows the possibility to scale
denominators if needed, and specifies a factor to multiply the resulting values by. For example, if GDP is a denominator and is measured in
dollars, dividing will create very small numbers (order 1e-10 and smaller) which could cause problems with numerical precision.}

\item{x_ID}{A column name of \code{x} to use to match rows with \code{denoms}. Default is \code{"uCode"}.}

\item{denoms_ID}{A column name of \code{denoms} to use to match rows with \code{x}. Default is \code{"uCode"}.}

\item{f_denom}{A function which takes two numeric vector arguments and is used to perform the denomination for each
column. By default, this is division, i.e. \code{x[[col]]/denoms[[col]]} for given columns, but any function can be passed
that takes two numeric vectors as inputs and returns a single numeric vector. See details.}

\item{...}{arguments passed to or from other methods.}
}
\value{
A data frame of the same size as \code{x}, with any specified columns denominated according to specifications.
}
\description{
"Denominates" or "scales" variables by other variables. Typically this is done by dividing extensive variables such as
GDP by a scaling variable such as population, to give an intensive variable (GDP per capita).
}
\details{
A data frame \code{x} is denominated by variables found in another data frame \code{denoms}, according to specifications in
\code{denomby}. \code{denomby} specifies which columns in \code{x} are to be denominated, and by which columns in \code{denoms}, and
any scaling factors to apply to each denomination.

Both \code{x} and \code{denomby} must contain an ID column which matches the rows of \code{x} to \code{denomby}. If not specified, this
is assumed to be \code{uCode},  but can also be specified using the \code{x_ID} and \code{denoms_ID} arguments. All entries in
\code{x[[x_ID]]} must be present in \code{denoms[[denoms_ID]]}, although extra rows are allowed in \code{denoms}. This is because
the rows of \code{x} are matched to the rows of \code{denoms} using these ID columns, to ensure that units (rows) are correctly
denominated.

By default, columns of \code{x} are divided by columns of \code{denoms}. This can be generalised by setting \code{f_denom} to another
function which takes two numeric vector arguments. I.e. setting \verb{denoms = ``*`` } will multiply columns of \code{x} and
denoms together.
}
\examples{
# Get a sample of indicator data (note must be indicators plus a "UnitCode" column)
iData <- ASEM_iData[c("uCode", "Goods", "Flights", "LPI")]
# Also get some denominator data
denoms <- ASEM_iData[c("uCode", "GDP", "Population")]
# specify how to denominate
denomby <- data.frame(iCode = c("Goods", "Flights"),
Denominator = c("GDP", "Population"),
ScaleFactor = c(1, 1000))
# Denominate one by the other
iData_den <- Denominate(iData, denoms, denomby)

}
\seealso{
\itemize{
\item \link{WorldDenoms} A data set of some common national-level denominators.
}
}
