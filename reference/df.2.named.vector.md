# Data frame to named vector

Turns the two first columns of a data frame into a named vector, taking
the values from the second and the names from the first column.

## Usage

``` r
df.2.named.vector(.data)
```

## Arguments

- .data:

  A data frame with at least two columns.

## Value

A named vector.

## Author

Michaja Pehl

## Examples

``` r
data <- data.frame(names = c("one", "two", "three"), values = 1:3)
data
#>   names values
#> 1   one      1
#> 2   two      2
#> 3 three      3
df.2.named.vector(data)
#>   one   two three 
#>   "1"   "2"   "3" 
```
