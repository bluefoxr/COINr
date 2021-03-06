% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coin_resultstable.R
\name{getUnitReport}
\alias{getUnitReport}
\title{Generate unit report}
\usage{
getUnitReport(
  COIN,
  usel,
  out_type = ".html",
  outdir = NULL,
  rmd_template = NULL
)
}
\arguments{
\item{COIN}{A COIN}

\item{usel}{A selected unit code, or a character vector of unit codes (for multiple reports)}

\item{out_type}{A string specifying the output type. Can be either ".docx" (Word), ".pdf" or ".html". IMPORTANT: if the
rmd template includes interactive plots (e.g. the iplot functions from COINr), writing to docx or pdf will not work
\emph{unless} you have installed the webshot package. To do this, run:
\code{install.packages("webshot")}
\code{webshot::install_phantomjs()}}

\item{outdir}{Character string specifying the output directory (defaults to current working directory).}

\item{rmd_template}{A character string specifying the full file path to an rmd template which is used to generate the report. If this is not specified,
defaults to COINr's inbuilt template example.}
}
\value{
Markdown document rendered to Word.
}
\description{
Generates a scorecard for a given unit using an R Markdown template. Most likely you will want to customise the template
which can be found in the COINr installed package directory under /inst. Currently, a few examples are given, such as some charts and basic summary statistics.
}
\details{
This function will render the unit report to either pdf, html or word doc. As mentioned below, if you have html widgets
such as interactive plotly plots, or COINr iplot functions, you will need to install the webshot package to be able to
render to pdf or word formats.

To customise the template, copy the .rmd template found in /inst and alter it, then point the rmd_template argument to
your new template.

Note that this function is particularly useful for generating a large number of reports, e.g. we can generate reports for
all units at once using a for loop, purrr::map or apply or similar.
}
\examples{
\dontrun{"test")}

}
