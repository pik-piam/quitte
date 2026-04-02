# Duplicate rows

Duplicate rows in a data frame, modifying a specified column.

## Usage

``` r
duplicate(data, ...)

duplicate_(data, column)
```

## Arguments

- data:

  A `data frame` or `quitte` object.

- ..., column:

  A key-value pair of the column to modify.

## Value

A `data frame` or `quitte` object, same as input.

## Examples

``` r
require(dplyr)
(data <- tibble(region   = rep(c('AFR', 'CHN'), 2),
                    variable = paste('Var', c(1, 1, 2, 2)),
                    value    = 1:4))
#> # A tibble: 4 × 3
#>   region variable value
#>   <chr>  <chr>    <int>
#> 1 AFR    Var 1        1
#> 2 CHN    Var 1        2
#> 3 AFR    Var 2        3
#> 4 CHN    Var 2        4

data %>% duplicate(region = 'World')
#> # A tibble: 8 × 3
#>   region variable value
#>   <chr>  <chr>    <int>
#> 1 AFR    Var 1        1
#> 2 CHN    Var 1        2
#> 3 AFR    Var 2        3
#> 4 CHN    Var 2        4
#> 5 World  Var 1        1
#> 6 World  Var 1        2
#> 7 World  Var 2        3
#> 8 World  Var 2        4
```
