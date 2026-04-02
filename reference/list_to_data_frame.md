# Convert mapping list to data frame

Convert mapping list to data frame

## Usage

``` r
list_to_data_frame(l, ...)

list_to_data_frame_(l, category = "category", item = "item")
```

## Arguments

- l:

  A named list of character vectors.

- ...:

  Unquoted names of category and item columns. Defaults to 'category'
  and 'item'.

- category:

  Name of category column. Defaults to 'category'.

- item:

  Name of item column. Defaults to 'item'.

## Value

A data frame.

## Author

Michaja Pehl

## Examples

``` r
l <- list(Africa = c('Egypt', 'Tanzania'),
          Europe = c('Portugal', 'Ukraine', 'Denmark'))
list_to_data_frame(l, region, country)
#> # A tibble: 5 × 2
#>   region country 
#>   <chr>  <chr>   
#> 1 Africa Egypt   
#> 2 Africa Tanzania
#> 3 Europe Portugal
#> 4 Europe Ukraine 
#> 5 Europe Denmark 
```
