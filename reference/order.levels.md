# Order data frame factor levels.

Arranges the levels of data frame columns in a given order. Non-factor
columns are silently converted.

## Usage

``` r
order.levels(df, ..., drop.extra.levels = TRUE)

order.levels_(df, dots, drop.extra.levels = TRUE)
```

## Arguments

- df:

  A data frame (or quitte object).

- ...:

  Name-value pairs assigning level order to factor columns.

- drop.extra.levels:

  If `TRUE` (default) levels not present in the factor are silently
  dropped.

- dots:

  A named list of factor columns and corresponding levels.

## Value

A data frame (or quitte object, same as `data`).

## Author

Michaja Pehl

## Examples

``` r
require(dplyr)
str(df <- tibble(UPPER = LETTERS[3:1], lower = factor(letters[24:26]),
                     value = 1:3))
#> tibble [3 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ UPPER: chr [1:3] "C" "B" "A"
#>  $ lower: Factor w/ 3 levels "x","y","z": 1 2 3
#>  $ value: int [1:3] 1 2 3
str(order.levels(df, UPPER = LETTERS[1:3], lower = letters[26:20]))
#> tibble [3 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ UPPER: Factor w/ 3 levels "A","B","C": 3 2 1
#>  $ lower: Factor w/ 3 levels "z","y","x": 3 2 1
#>  $ value: int [1:3] 1 2 3
str(order.levels_(df, list(UPPER = LETTERS[1:3], lower = letters[26:23]),
                  drop.extra.levels = FALSE))
#> tibble [3 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ UPPER: Factor w/ 3 levels "A","B","C": 3 2 1
#>  $ lower: Factor w/ 4 levels "z","y","x","w": 3 2 1
#>  $ value: int [1:3] 1 2 3
```
