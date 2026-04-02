# Sum over one dimension of a data frame

`sum_total()` is a short-hand function to calculate and insert the
(weighted) sum of a extensive (intensive) category in a data frame.

## Usage

``` r
sum_total(data, group, value = NA, name = "Total", na.rm = TRUE, weight = NA)

sum_total_(data, group, value = NA, name = "Total", na.rm = TRUE, weight = NA)
```

## Arguments

- data:

  a data frame

- group:

  column for which the sum is to be calculated

- value:

  column of the numbers to be summed

- name:

  entry in column `group` for the sum; defaults to `"Total"`

- na.rm:

  `logical.` Should missing values (including NaN) be removed (default)?

- weight:

  column of the weights to be applied, if any

## Value

a data frame

## Author

Michaja Pehl

## Examples

``` r
require(dplyr)

(d <- expand.grid(
    UPPER  = LETTERS[1:2],
    lower  = letters[24:26],
    number = 1:2
) %>%
        arrange(UPPER, lower, number) %>%
        mutate(value = c(1:6, NA, 8:12)))
#>    UPPER lower number value
#> 1      A     x      1     1
#> 2      A     x      2     2
#> 3      A     y      1     3
#> 4      A     y      2     4
#> 5      A     z      1     5
#> 6      A     z      2     6
#> 7      B     x      1    NA
#> 8      B     x      2     8
#> 9      B     y      1     9
#> 10     B     y      2    10
#> 11     B     z      1    11
#> 12     B     z      2    12

sum_total(d, UPPER)
#>    UPPER lower number value
#> 1      A     x      1     1
#> 2      B     x      1    NA
#> 3  Total     x      1     1
#> 4      A     x      2     2
#> 5      B     x      2     8
#> 6  Total     x      2    10
#> 7      A     y      1     3
#> 8      B     y      1     9
#> 9  Total     y      1    12
#> 10     A     y      2     4
#> 11     B     y      2    10
#> 12 Total     y      2    14
#> 13     A     z      1     5
#> 14     B     z      1    11
#> 15 Total     z      1    16
#> 16     A     z      2     6
#> 17     B     z      2    12
#> 18 Total     z      2    18

sum_total(d, lower, name = 'sum over lower', na.rm = FALSE)
#>    UPPER          lower number value
#> 1      A              x      1     1
#> 2      A              y      1     3
#> 3      A              z      1     5
#> 4      A sum over lower      1     9
#> 5      A              x      2     2
#> 6      A              y      2     4
#> 7      A              z      2     6
#> 8      A sum over lower      2    12
#> 9      B              x      1    NA
#> 10     B              y      1     9
#> 11     B              z      1    11
#> 12     B sum over lower      1    NA
#> 13     B              x      2     8
#> 14     B              y      2    10
#> 15     B              z      2    12
#> 16     B sum over lower      2    30

(e <- tibble(
    item = c('large', 'medium', 'small'),
    specific.value = c(1, 10, 100),
    size = c(1000, 100, 1)))
#> # A tibble: 3 × 3
#>   item   specific.value  size
#>   <chr>           <dbl> <dbl>
#> 1 large               1  1000
#> 2 medium             10   100
#> 3 small             100     1

sum_total(e, item, value = specific.value, name = 'Average', weight = size)
#> # A tibble: 4 × 3
#>   item    specific.value  size
#>   <chr>            <dbl> <dbl>
#> 1 Average           1.91  1101
#> 2 large             1     1000
#> 3 medium           10      100
#> 4 small           100        1
```
