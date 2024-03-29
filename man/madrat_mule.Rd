% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madrat_mule.R
\name{madrat_mule}
\alias{madrat_mule}
\title{Madrat Mule}
\usage{
madrat_mule(x)
}
\arguments{
\item{x}{Anything.}
}
\value{
A \code{\link[magclass:magclass-package]{magpie}} object containing \code{x} (in unusable
form), or the original \code{x} if a \code{\link[magclass:magclass-package]{magpie}} object was
passed.
}
\description{
Convert \emph{anything} to a \code{\link[magclass:magclass-package]{magpie}} object and back to
traffic data across \code{madrat} borders.
}
\examples{
str(x <- madrat_mule(quitte_example_data))
madrat_mule(x)

}
\author{
Michaja Pehl
}
