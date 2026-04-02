# Uses mutate based on a character vector#'

Uses mutate based on a character vector#'

## Usage

``` r
mutate_text(df, s)
```

## Arguments

- df:

  a data frame

- s:

  a character string containing the formula to be applied in mutate

## Value

A data frame transformed with the mutate function

## Author

Antoine Levesque

## Examples

``` r
df = data.frame(x = c(1,2),
                y = c(3,4))
form = "z = x + y"
mutate_text(df,form)
#>   x y z
#> 1 1 3 4
#> 2 2 4 6
```
