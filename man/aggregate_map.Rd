% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_map.R
\name{aggregate_map}
\alias{aggregate_map}
\title{Aggregates or disaggregates a data frame}
\usage{
aggregate_map(
  data,
  mapping,
  by,
  subset2agg = NULL,
  only.new = TRUE,
  na.rm = TRUE,
  weights = NULL,
  forceAggregation = FALSE,
  autodetect = "auto",
  scaleWeights = TRUE,
  variable = "variable",
  value = "value",
  unit = "unit",
  weight_val_col = "weight_val_col",
  weight_item_col = NULL,
  fun = sum
)
}
\arguments{
\item{data}{a data frame.}

\item{mapping}{a data frame connecting the resolution in \code{"data"} and the
wished resolution}

\item{by}{(named) vector giving the correspondence between the column name
of \code{"data"} and of the \code{"mapping"}}

\item{subset2agg}{subset of variables for which the (dis)aggregation is
applied. If \code{NULL} (the default), the (dis)aggregation is applied to all
variables.}

\item{only.new}{If \code{FALSE} (the default), add the (dis)aggregated data frame
to existing ones. If \code{TRUE}, return only the (dis)aggregated data frame.}

\item{na.rm}{If \code{TRUE} (the default), remove items calculated as \code{NA}.}

\item{weights}{a data frame, a variable name as a character vector, or
\code{NULL}. See details.}

\item{forceAggregation}{binary. If \code{TRUE}, (dis)aggregation will be applied
even though the items contained contained in the data and in the data do
not fully match. The data is reduced to the items covered both by the
mapping and the data.}

\item{autodetect}{this parameter takes the values \code{'auto'}, \code{'aggregate'} or
\code{'disaggregate'}. If \code{'auto'} (the default) the function tries to
auto-detect whether this is an aggregation or a disaggregation. If
\code{'aggregate'}, it will aggregate, if \code{'disaggregate'}, it will
disaggregate.}

\item{scaleWeights}{logical. If \code{TRUE}, weights are scaled so that the sum of
the components equals the value of the larger category.}

\item{variable}{Column name of variables. Defaults to \code{"variable"}.}

\item{value}{Column name of values. Defaults to \code{"value"}.}

\item{unit}{Column name of units. Defaults to \code{"unit"}.}

\item{weight_val_col}{name of the value column in the \code{"weigths"} data frame,
if \code{"weigths"} is a data frame}

\item{weight_item_col}{name of the item column in the \code{"weigths"} data frame,
if \code{"weigths"} is a data frame. The item column is the column corresponding
to the mapping}

\item{fun}{aggregation function to use.  Defaults to \code{sum}.}
}
\value{
A data frame.
}
\description{
Aggregates or disaggregates the values of a data frame according to a mapping
}
\details{
By default \code{"weights"} is set to \code{NULL}. For aggregations, this means that
values will be summed as they are. For disaggregations, each component of the
larger category will take the same value as the larger category, or
\code{"scaleWeights"} is \code{TRUE}, each component will be given an even weight. For
aggregations, \code{"weights"} can also be the name of a variable contained in
\code{"data"}. \code{"weights"} may also be a data frame.
}
\examples{
library(tidyr)
library(dplyr)
data <- inline.data.frame(c(
  "model;    scenario;   region;   variable;           unit;         period;   value",
  "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2010;     40000",
  "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2020;     50000",
  "REMIND;   Baseline;   USA;      Population;         million;      2010;     300",
  "REMIND;   Baseline;   USA;      Population;         million;      2020;     350",
  "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2010;     7000",
  "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2020;     8000",
  "REMIND;   Baseline;   CHN;      Population;         million;      2010;     1300",
  "REMIND;   Baseline;   CHN;      Population;         million;      2020;     1400"))

mapping = inline.data.frame(c(
  "region;      New_region",
  "USA;         GLO",
  "CHN;         GLO"
))

mapping2 = inline.data.frame(c(
  "Item      ;         Item_new",
  "Population;         Urban Population ",
  "Population;         Rural Population"
))

weights = inline.data.frame(c(
  "region; itemI           ;   weight",
  "USA   ; Urban Population;      0.5",
  "USA   ; Rural Population;      0.2",
  "CHN   ; Urban Population;      2",
  "CHN   ; Rural Population;      1"
))

#Regional Aggregation
aggregate_map(data,mapping, by = "region", subset2agg = c("Population"))

#Regional Weighted Aggregation
aggregate_map(data,mapping, by = "region", subset2agg = "GDP per Capita|MER",
              weights = "Population")

#Variable Weigthed Disaggregation
aggregate_map(data,mapping2, by = c("variable" = "Item"),
              subset2agg = c("Population"),weights = weights,
              weight_val_col = "weight", weight_item_col = "itemI")


}
\author{
Antoine Levesque
}
