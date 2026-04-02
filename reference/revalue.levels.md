# Revalue data frame factor levels.

Revalue the names of a level or character column in a dataframe,
according to a named vector given as an input

## Usage

``` r
revalue.levels(df, ...)

revalue.levels_(df, dots)
```

## Arguments

- df:

  A data frame (or quitte object).

- ...:

  Name-value pairs assigning a named vector with new names to a column
  from the dataframe.

- dots:

  A named list of columns containing the named vector with the old and
  new names for each column

## Value

A data frame (or quitte object, same as `data`).

## Author

Antoine Levesque

## Examples

``` r
data <- inline.data.frame(c(
  "model;    scenario;   region;   variable;           unit;         period;   value",
  "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2010;     40000",
  "REMIND;   Baseline;   USA;      Population;         million;      2010;     300",
  "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2010;     7000"))

reg_vec = c(USA = "United States")
var_vec = c("GDP per Capita|MER" = "gdp",
           Population = "pop")

revalue.levels(data,region = reg_vec)
#> # A tibble: 3 × 7
#>   model  scenario region        variable           unit    period value
#>   <chr>  <chr>    <chr>         <chr>              <chr>    <int> <int>
#> 1 REMIND Baseline United States GDP per Capita|MER US$200…   2010 40000
#> 2 REMIND Baseline United States Population         million   2010   300
#> 3 REMIND Baseline CHN           GDP per Capita|MER US$200…   2010  7000
revalue.levels_(data,list(region = reg_vec, variable = var_vec))
#> # A tibble: 3 × 7
#>   model  scenario region        variable unit       period value
#>   <chr>  <chr>    <chr>         <chr>    <chr>       <int> <int>
#> 1 REMIND Baseline United States gdp      US$2005/yr   2010 40000
#> 2 REMIND Baseline United States pop      million      2010   300
#> 3 REMIND Baseline CHN           gdp      US$2005/yr   2010  7000
```
