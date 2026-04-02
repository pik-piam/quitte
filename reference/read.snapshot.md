# Reads IAMC-style .csv or .xlsx files obtained as a IIASA snapshot into a quitte data frame, or data from rds file, allowing to filter the loaded data. If you're using a non-Windows system with head, tail and grep, a pre-filtering improves performance for csv files.

Reads IAMC-style .csv or .xlsx files obtained as a IIASA snapshot into a
quitte data frame, or data from rds file, allowing to filter the loaded
data. If you're using a non-Windows system with head, tail and grep, a
pre-filtering improves performance for csv files.

## Usage

``` r
read.snapshot(file, keep = list(), filter.function = identity)
```

## Arguments

- file:

  Path of single IAMC-style .csv/.mif/.xlsx/.rds file

- keep:

  list with quitte columns as names and data points that should be kept.
  If head, tail and grep are available and a csv/mif file is read, this
  list is used to extract the data before reading it into R. The more
  you restrict the data here, the faster the data is read.

- filter.function:

  A function used to filter data during read, see read.quitte
  description. This allows for more complex filtering, but no
  performance-enhancing pre-filtering using grep is used. The 'keep'
  list and the 'filter.function' can be combined.

## Value

A quitte data frame.

## Author

Oliver Richters

## Examples

``` r
if (FALSE) { # \dontrun{
read.filter.snapshot("snapshot.csv", list(scenario = c("CurPol", "NDC"), region = "World"))
} # }
```
