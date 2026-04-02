# Factorise

Generate a factor with levels in prescribed order.

## Usage

``` r
factorise(x)
```

## Arguments

- x:

  A character vector.

## Value

A factor from `x`, with levels in the same order as they appear in
within `x`.

## Author

Michaja Pehl

## Examples

``` r
factor(c('a', 'c', 'b'))
#> [1] a c b
#> Levels: a b c
factorise(c('a', 'c', 'b'))
#> [1] a c b
#> Levels: a c b
```
