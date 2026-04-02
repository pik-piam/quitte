# Add country code

Wrapper function for
[`countrycode::countrycode()`](https://vincentarelbundock.github.io/countrycode/man/countrycode.html)
enabling piped execution.

## Usage

``` r
add_countrycode(data, ..., warn = TRUE, na.rm = FALSE)

add_countrycode_(data, origin, destination, warn = TRUE, na.rm = FALSE)
```

## Arguments

- data:

  A data frame.

- ...:

  Key-value pairs for NSE of `origin` and `destination` parameters.

- warn:

  Prints unique elements from sourcevar for which no match was found.

- na.rm:

  If `TRUE`, remove ambiguously matched rows.

- origin:

  Named scalar linking source column to source coding scheme. See
  [`countrycode::countrycode()`](https://vincentarelbundock.github.io/countrycode/man/countrycode.html)
  for details.

- destination:

  Named scalar linking destination column name to destination coding
  scheme. See
  [`countrycode::countrycode()`](https://vincentarelbundock.github.io/countrycode/man/countrycode.html)
  for details.

## Value

A data frame.

## Author

Michaja Pehl

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data <- tibble(
    country = c('Belgium', 'Narnia', 'Russia', 'Botswana'),
    data    = 1:4)

data %>% add_countrycode(country = country.name, m49.code = un)
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `m49.code = countrycode(...)`.
#> Caused by warning:
#> ! Some values were not matched unambiguously: Narnia
#> To fix unmatched values, please use the `custom_match` argument. If you think the default matching rules should be improved, please file an issue at https://github.com/vincentarelbundock/countrycode/issues
#> # A tibble: 4 × 3
#>   country   data m49.code
#>   <chr>    <int>    <dbl>
#> 1 Belgium      1       56
#> 2 Narnia       2       NA
#> 3 Russia       3      643
#> 4 Botswana     4       72
data %>% add_countrycode_(c('country' = 'country.name'), 'iso3c',
                          warn = FALSE, na.rm = TRUE)
#> # A tibble: 3 × 3
#>   country   data iso3c
#>   <chr>    <int> <chr>
#> 1 Belgium      1 BEL  
#> 2 Russia       3 RUS  
#> 3 Botswana     4 BWA  
```
