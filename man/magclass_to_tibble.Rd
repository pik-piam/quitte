% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/magclass_to_tibble.R
\name{magclass_to_tibble}
\alias{magclass_to_tibble}
\title{Make a Tibble of a Magclass}
\usage{
magclass_to_tibble(m, colnames = NULL)
}
\arguments{
\item{m}{A \code{\link[magclass:magclass-package]{magpie}} object.}

\item{colnames}{Column names for the returned \code{tibble}.  Must match the
number of columns.}
}
\value{
A \code{\link[tibble:tibble]{tibble}}.
}
\description{
Sensible \code{magclass} to \code{tibble} conversion.
}
\examples{
magclass_to_tibble(magclass::maxample('pop'))
}
