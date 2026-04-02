# Removes all NA columns of a data frame

`removeColNa()` Removes all columns of a data frame for which all
entries are NA, or the default of fct_explict_na

## Usage

``` r
removeColNa(df)
```

## Arguments

- df:

  a data frame

## Value

a data frame

## Author

Antoine Levesque

## Examples

``` r
df <- data.frame(
    character = letters[1:5],
    factor = as.factor(LETTERS[1:5]),
    value = 1:5,
    unit = NA,
    unit2 = forcats::fct_na_value_to_level(factor(NA), level = '(Missing)'),
    stringsAsFactors = FALSE)
str(df)
#> 'data.frame':    5 obs. of  5 variables:
#>  $ character: chr  "a" "b" "c" "d" ...
#>  $ factor   : Factor w/ 5 levels "A","B","C","D",..: 1 2 3 4 5
#>  $ value    : int  1 2 3 4 5
#>  $ unit     : logi  NA NA NA NA NA
#>  $ unit2    : Factor w/ 1 level "(Missing)": 1 1 1 1 1
str(removeColNa(df))
#> 'data.frame':    5 obs. of  3 variables:
#>  $ character: chr  "a" "b" "c" "d" ...
#>  $ factor   : Factor w/ 5 levels "A","B","C","D",..: 1 2 3 4 5
#>  $ value    : int  1 2 3 4 5
```
