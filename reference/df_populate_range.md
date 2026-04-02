# Populate Data With Sequence Along Range

Generate sequences of n equidistant data points for a column in a data
frame.

## Usage

``` r
df_populate_range(df, column, n = 100)
```

## Arguments

- df:

  A data frame.

- column:

  The column with data to populate. Uses
  [`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  statements

- n:

  Length of the sequence to return. Defaults to 100.

## Value

A data frame.

## Examples

``` r
require(dplyr, warn.conflicts = FALSE, quietly = TRUE)
require(tidyr, warn.conflicts = FALSE, quietly = TRUE)

tibble(A = (1:3) ^ 2,
       B = exp(0:2)) %>%
    pivot_longer(everything()) %>%
    arrange(name, value) %>%
    print() %>%
    df_populate_range(value, n = 6)
#> # A tibble: 6 × 2
#>   name  value
#>   <chr> <dbl>
#> 1 A      1   
#> 2 A      4   
#> 3 A      9   
#> 4 B      1   
#> 5 B      2.72
#> 6 B      7.39
#> # A tibble: 12 × 2
#>    name  value
#>    <chr> <dbl>
#>  1 A      1   
#>  2 A      2.6 
#>  3 A      4.2 
#>  4 A      5.8 
#>  5 A      7.4 
#>  6 A      9   
#>  7 B      1   
#>  8 B      2.28
#>  9 B      3.56
#> 10 B      4.83
#> 11 B      6.11
#> 12 B      7.39
```
