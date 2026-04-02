# Distribute into Equal Bins

`bin_distribute(x, binsize)` distributes the items in `x` into the
minimum number of bins whose sizes differ at maximum by one and do not
exceed `binsize`. `bin_distribute_sizes(count, binsize)` calculates the
sizes of such bins for `count` items.

## Usage

``` r
bin_distribute(x, binsize)

bin_distribute_sizes(count, binsize)
```

## Arguments

- x:

  A character vector to be distributed into bins.

- binsize:

  The maximum bin size.

- count:

  The number of items to be binned.

## Value

`bin_distribute()` returns a list with the sub-vectors of `x`.
`bin_distribute_sizes()` returns a vector of sizes.

## Author

Michaja Pehl

## Examples

``` r
regions <- c('CAZ', 'CHA', 'EUR', 'IND', 'JPN', 'LAM', 'MEA', 'NEU', 'OAS',
             'REF', 'SSA', 'USA', 'World')

bin_distribute(regions, 5)
#> [[1]]
#> [1] "CAZ" "CHA" "EUR" "IND" "JPN"
#> 
#> [[2]]
#> [1] "LAM" "MEA" "NEU" "OAS"
#> 
#> [[3]]
#> [1] "REF"   "SSA"   "USA"   "World"
#> 
bin_distribute_sizes(length(regions), 5)
#> [1] 5 4 4

bin_distribute(regions, 6)
#> [[1]]
#> [1] "CAZ" "CHA" "EUR" "IND" "JPN"
#> 
#> [[2]]
#> [1] "LAM" "MEA" "NEU" "OAS"
#> 
#> [[3]]
#> [1] "REF"   "SSA"   "USA"   "World"
#> 
bin_distribute_sizes(length(regions), 6)
#> [1] 5 4 4

bin_distribute(regions, 7)
#> [[1]]
#> [1] "CAZ" "CHA" "EUR" "IND" "JPN" "LAM" "MEA"
#> 
#> [[2]]
#> [1] "NEU"   "OAS"   "REF"   "SSA"   "USA"   "World"
#> 
bin_distribute_sizes(length(regions), 7)
#> [1] 7 6
```
