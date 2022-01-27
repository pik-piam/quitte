% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.mif.R
\name{write.mif}
\alias{write.mif}
\title{Write .mif file}
\usage{
write.mif(x, path, comment_header = NULL, comment = "#")
}
\arguments{
\item{x}{A \code{\link{quitte}} data frame.}

\item{path}{Path or connection to write to.}

\item{comment_header}{Comment header to be written to the \code{.mif} file.}

\item{comment}{A character to prepend to comment header lines.}
}
\description{
A wrapper around \href{https://redmine.pik-potsdam.de/projects/mo/wiki/Model_Intercomparison_File_Format_(mif)}{\verb{readr::write_lines] for writing files conforming to the [}.mif` standard}.
}
\examples{
write.mif(quitte_example_data, tempfile())

}
\author{
Michaja Pehl
}