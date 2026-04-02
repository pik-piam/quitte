# Set Xor

Performs \\(x \cup y) \setminus (x \cap y)\\ on parameters, returning
all elements that are in either x or y, but not both.

## Usage

``` r
setXor(x, y)
```

## Arguments

- x, y:

  Objects to perform set function on.

## Author

Michaja Pehl

## Examples

``` r
x <- c('a', 'b', 'c')
y <- c('b', 'c', 'd')
setXor(x, y)
#> [1] "a" "d"
```
