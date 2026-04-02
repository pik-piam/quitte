# Replace rows in a data frame with new values

`overwrite()` [`rbind()`](https://rdrr.io/r/base/cbind.html)s the data
frames `lhs` and `rhs`, removing any duplicate lines, which are
determined without regard to the columns in `except`.

## Usage

``` r
overwrite(lhs, rhs, except = "value")
```

## Arguments

- lhs:

  data frame with values that will replace others

- rhs:

  data frame with values that will be replaced

- except:

  names of columns that will not be considered in determining which
  columns to replace; defaults to "value"

## Value

data frame in which rows from rhs have been replaced with rows from lhs

## Author

Michaja Pehl

## Examples

``` r
require(dplyr)
data <- data.frame(expand.grid(UPPER = LETTERS[1:2],
                               lower = letters[24:26]),
                   value = 1:6)
data
#>   UPPER lower value
#> 1     A     x     1
#> 2     B     x     2
#> 3     A     y     3
#> 4     B     y     4
#> 5     A     z     5
#> 6     B     z     6
data %>%
  filter(lower == "y") %>%
  mutate(value = value * 10) %>%
  overwrite(data)
#>   UPPER lower value
#> 1     A     y    30
#> 2     B     y    40
#> 3     A     x     1
#> 4     B     x     2
#> 5     A     z     5
#> 6     B     z     6
```
