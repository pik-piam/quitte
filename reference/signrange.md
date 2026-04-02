# Sign Range

Returns the range of signs in a numerical vector as a character string.

## Usage

``` r
signrange(x, na.rm = TRUE)
```

## Arguments

- x:

  A numerical vector.

- na.rm:

  Should `NA`s be ignored?

## Value

A character string of signs found in `x`.

## Author

Michaja Pehl

## Examples

``` r
signrange(-1)
#> [1] "-"
signrange(0)
#> [1] ""
signrange(1)
#> [1] "+"
signrange(c(-1, 0))
#> [1] "-"
signrange(c(0, 1))
#> [1] "+"
signrange(c(-1, 1))
#> [1] "+/-"
```
