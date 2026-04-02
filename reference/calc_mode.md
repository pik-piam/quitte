# Calculate the mode of a sample

Calculate the mode of a sample

## Usage

``` r
calc_mode(v)
```

## Arguments

- v:

  A vector.

## Value

The mode, or a vector of modes if the sample is multi-modal.

## Author

Michaja Pehl

## Examples

``` r
calc_mode(c(1, 1, 100))
#> [1] 1
calc_mode(c('a', 'a', 'b', 'c', 'c'))
#> [1] "a" "c"
```
