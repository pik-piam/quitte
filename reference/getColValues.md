# Retrieves values from one column of a quitte object

Retrieves values from one column of a quitte object

## Usage

``` r
getColValues(df, colVar)
```

## Arguments

- df:

  quitte object

- colVar:

  name of the column of interest

## Value

a vector containing the values of the column without duplicates. The
class of the returned vector is either numeric or character

## Author

Antoine Levesque

## Examples

``` r
data(mtcars)
getColValues(mtcars,"mpg")
#>  [1] 21.0 22.8 21.4 18.7 18.1 14.3 24.4 19.2 17.8 16.4 17.3 15.2 10.4
#> [14] 14.7 32.4 30.4 33.9 21.5 15.5 13.3 27.3 26.0 15.8 19.7 15.0
```
