# (Re-) Factorise factor and character columns in data frame

`factor.data.frame()` turns character columns in a data frame into
factor columns and refactorises factor columns, silently dropping unused
levels.

## Usage

``` r
factor.data.frame(df, ...)

factor.data.frame_(df, .dots)
```

## Arguments

- df:

  A data frame.

- ...:

  Column names to factorise.

- .dots:

  Character vector of column names to factorise.

## Value

A data frame.

## See also

[`character.data.frame()`](character.data.frame.md)

## Author

Michaja Pehl

## Examples

``` r
require(dplyr)
(df <- tibble(
    char = letters[1:3],
    fact = factor(LETTERS[24:26], levels = LETTERS[c(1:3, 24:26)]),
    num  = (1:3) ^ 2))
#> # A tibble: 3 × 3
#>   char  fact    num
#>   <chr> <fct> <dbl>
#> 1 a     X         1
#> 2 b     Y         4
#> 3 c     Z         9

str(factor.data.frame(df))
#> tibble [3 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ char: Factor w/ 3 levels "a","b","c": 1 2 3
#>  $ fact: Factor w/ 3 levels "X","Y","Z": 1 2 3
#>  $ num : num [1:3] 1 4 9
str(factor.data.frame_(df, 'num'))
#> tibble [3 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ char: chr [1:3] "a" "b" "c"
#>  $ fact: Factor w/ 6 levels "A","B","C","X",..: 4 5 6
#>  $ num : Factor w/ 3 levels "1","4","9": 1 2 3
```
