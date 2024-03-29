% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.IAMCxlsx.R
\name{write.IAMCxlsx}
\alias{write.IAMCxlsx}
\title{Write .xlsx file}
\usage{
write.IAMCxlsx(x, path, append = FALSE)
}
\arguments{
\item{x}{A \code{\link{quitte}} data frame.}

\item{path}{Path or connection to write to.}

\item{append}{Overwrite existing files (\code{FALSE}, default), or append to them
(\code{TRUE}).}
}
\description{
Write a \code{.xlsx} file in line with IAMC standard.
}
\examples{
write.IAMCxlsx(quitte_example_data, tempfile())

}
\author{
Michaja Pehl, Oliver Richters
}
