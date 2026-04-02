# Does a data frame match the quitte definition?

Checks if `df` fulfills all criteria of a "quitte data frame", namely:

- Must be of class "data.frame".

- Must have the mandatory columns "model", "scenario", "region",
  "variable", "unit", "period", and "value".

- The "period" column must be of class "integer" or "POSIXct".

- The "value" column must be of type "numeric".

## Usage

``` r
looks_like_quitte(df, verbose = NULL)
```

## Arguments

- df:

  An object to test for `quitte` structure.

- verbose:

  Any function like [`message()`](https://rdrr.io/r/base/message.html),
  [`warning()`](https://rdrr.io/r/base/warning.html), or
  [`stop()`](https://rdrr.io/r/base/stop.html) to report if any of the
  criteria of the quitte definition are not fulfilled.

## Value

`TRUE` if all criteria are met, `FALSE` otherwise.

## Examples

``` r
library(dplyr)

quitte_example_data %>%
    looks_like_quitte()
#> [1] TRUE

quitte_example_data %>%
    select(-'model') %>%
    mutate(period = as.character(period)) %>%
    looks_like_quitte(verbose = message)
#> Mandatory columns `model` missing from `df`.
#> Column `period` must be of class "integer" or "POSIXct", but has class `character`.
#> [1] FALSE
```
