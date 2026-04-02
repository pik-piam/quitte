# Changes factor columns of a data frame into character columns

`character.data.frame()` turns factor columns of a data frame into
character columns.

## Usage

``` r
character.data.frame(df, ...)

character.data.frame_(df, .dots)
```

## Arguments

- df:

  a data frame

- ...:

  Column names to convert to characters.

- .dots:

  Character vector of column names to turn into characters.

## Value

A data frame.

## See also

[`factor.data.frame()`](factor.data.frame.md)

## Author

Antoine Levesque

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

character.data.frame(df)
#> # A tibble: 3 × 3
#>   char  fact    num
#>   <chr> <chr> <dbl>
#> 1 a     X         1
#> 2 b     Y         4
#> 3 c     Z         9
character.data.frame_(df, 'num')
#> # A tibble: 3 × 3
#>   char  fact  num  
#>   <chr> <fct> <chr>
#> 1 a     X     1    
#> 2 b     Y     4    
#> 3 c     Z     9    
```
