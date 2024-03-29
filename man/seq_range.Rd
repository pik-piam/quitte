% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_range.R
\name{seq_range}
\alias{seq_range}
\title{Sequence from a Range}
\usage{
seq_range(range, by = NA, length.out = NULL)
}
\arguments{
\item{range}{Vector with starting and end values of the sequence. Only first
two elements are considered.}

\item{by}{Number; increment of the sequence.}

\item{length.out}{Desired length of the sequence. A non-negative number,
which will be rounded up if fractional.}
}
\value{
Returns a vector of type "integer" or "double": programmers should
not rely on which.
}
\description{
Generate regular sequence from a range. Wrapper function for
\code{\link[=seq]{seq()}}.
}
\examples{
seq_range(range(1:13), by = 3)
}
\seealso{
\code{\link[=seq]{seq()}}, \code{\link[=range]{range()}}
}
\author{
Michaja Pehl
}
