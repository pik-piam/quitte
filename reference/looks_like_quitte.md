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

  Either `NULL`, or one of `'abort'`, `'warn'`, or `'inform`', to report
  if any of the criteria of the quitte definition are not fulfilled
  using `[cli::cli_abort]`, `[cli::cli_warn]`, or `[cli::cli_inform]`.

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
    looks_like_quitte(verbose = 'inform')
#> `df` must have all mandatory columns.
#> ℹ "model" is missing.
#> Column period must be of class <integer> or <POSIXct>.
#> ℹ It is of class <character>.
#> [1] FALSE
```
