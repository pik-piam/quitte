# Allows to interactively filter data from quitte object

Allows to interactively filter data from quitte object

## Usage

``` r
chooseFilter(
  data,
  types = c("model", "scenario", "region", "variable", "period"),
  keep = list()
)
```

## Arguments

- data:

  A quitte object or something that can be transformed into one by
  as.quitte

- types:

  vector of quitte columns for user to select data if more than one
  option available

- keep:

  list with quitte columns as names and data points that should always
  be kept. If the column is not also in types, only the elements in that
  list are kept

## Author

Oliver Richters

## Examples

``` r
if (FALSE) { # \dontrun{
  qe <- chooseFilter(quitte_example_dataAR6, types = c("model"),
                    keep = list(region = "World"))
} # }
```
