# Generate Cartesian Product from Vectors

Generate Cartesian Product from Vectors

## Usage

``` r
cartesian(..., sep = ".")
```

## Arguments

- ...:

  Vectors to be combined

- sep:

  If a character string, that string will separate the elements of `...`
  (if necessary cast to `character`) in the output. If `NULL`, elements
  of `...` will not be cast. Defaults to `'.'`.

## Value

If `sep` is a character string, then a character vector of concatenated
elements. If `sep` is `NULL`, then a list of concatenated elements.

## Examples

``` r
cartesian(c('a', 'b'), 1:3, c('X', 'Y', 'Z'))
#>  [1] "a.1.X" "a.1.Y" "a.1.Z" "a.2.X" "a.2.Y" "a.2.Z" "a.3.X" "a.3.Y"
#>  [9] "a.3.Z" "b.1.X" "b.1.Y" "b.1.Z" "b.2.X" "b.2.Y" "b.2.Z" "b.3.X"
#> [17] "b.3.Y" "b.3.Z"
#  [1] "a.1.X" "a.1.Y" "a.1.Z" "a.2.X" "a.2.Y" "a.2.Z" "a.3.X" "a.3.Y"
#  [9] "a.3.Z" "b.1.X" "b.1.Y" "b.1.Z" "b.2.X" "b.2.Y" "b.2.Z" "b.3.X"
# [17] "b.3.Y" "b.3.Z"

str(cartesian(c('a', 'b'), 17:19, sep = NULL))
#> List of 6
#>  $ :List of 2
#>   ..$ : chr "a"
#>   ..$ : int 17
#>  $ :List of 2
#>   ..$ : chr "a"
#>   ..$ : int 18
#>  $ :List of 2
#>   ..$ : chr "a"
#>   ..$ : int 19
#>  $ :List of 2
#>   ..$ : chr "b"
#>   ..$ : int 17
#>  $ :List of 2
#>   ..$ : chr "b"
#>   ..$ : int 18
#>  $ :List of 2
#>   ..$ : chr "b"
#>   ..$ : int 19
# List of 6
#  $ :List of 2
#   ..$ : chr "a"
#   ..$ : int 17
#  $ :List of 2
#   ..$ : chr "a"
#   ..$ : int 18
#  $ :List of 2
#   ..$ : chr "a"
#   ..$ : int 19
#  $ :List of 2
#   ..$ : chr "b"
#   ..$ : int 17
#  $ :List of 2
#   ..$ : chr "b"
#   ..$ : int 18
#  $ :List of 2
#   ..$ : chr "b"
#   ..$ : int 19
```
