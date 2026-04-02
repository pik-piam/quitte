# Convert a vector of stings to a data frame

`inline.data.frame()` converts a vector of strings that contain
separated items into a data frame.

## Usage

``` r
inline.data.frame(..., sep = ";", quote = "")
```

## Arguments

- ...:

  string, or a vector of strings

- sep:

  Item separator within strings, defaults to ";"

- quote:

  Quote character for masking separators, empty by default

## Value

a data frame

## Author

Michaja Pehl

## Examples

``` r
inline.data.frame(
    "letters; numbers",
    "A;       1",
    "B;       2",
    NULL)   # this last line allows for easy switching of line order
#> # A tibble: 2 × 2
#>   letters numbers
#>   <chr>     <int>
#> 1 A             1
#> 2 B             2
inline.data.frame(c("letters; numbers", "A; 1", "B; 2"))
#> # A tibble: 2 × 2
#>   letters numbers
#>   <chr>     <int>
#> 1 A             1
#> 2 B             2
```
