% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_countrycode.R
\name{add_countrycode}
\alias{add_countrycode}
\alias{add_countrycode_}
\title{Add country code}
\usage{
add_countrycode(data, ..., warn = TRUE, na.rm = FALSE)

add_countrycode_(data, origin, destination, warn = TRUE, na.rm = FALSE)
}
\arguments{
\item{data}{A data frame.}

\item{...}{Key-value pairs for NSE of \code{origin} and \code{destination}
parameters.}

\item{warn}{Prints unique elements from sourcevar for which no match was
found.}

\item{na.rm}{If \code{TRUE}, remove ambiguously matched rows.}

\item{origin}{Named scalar linking source column to source coding scheme. See
\code{\link[countrycode:countrycode]{countrycode::countrycode()}} for details.}

\item{destination}{Named scalar linking destination column name to
destination coding scheme. See \code{\link[countrycode:countrycode]{countrycode::countrycode()}}
for details.}
}
\value{
A data frame.
}
\description{
Wrapper function for \code{\link[countrycode:countrycode]{countrycode::countrycode()}} enabling piped
execution.
}
\examples{
library(dplyr)
data <- tibble(
    country = c('Belgium', 'Narnia', 'Russia', 'Botswana'),
    data    = 1:4)

data \%>\% add_countrycode(country = country.name, m49.code = un)
data \%>\% add_countrycode_(c('country' = 'country.name'), 'iso3c',
                          warn = FALSE, na.rm = TRUE)
}
\author{
Michaja Pehl
}
