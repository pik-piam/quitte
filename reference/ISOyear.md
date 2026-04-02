# speedily converting years to POSIXct values

Converts integer years (e.g. `2023`) to lubridate::POSIXct date/time
values (e.g. `2023-07-02 12:00:00 GMT`) corresponding to July 2, noon,
which is the middle of the (non-leap) year. The function keeps a cache
of already converted values, as the underlying function
[`ISOdate()`](https://rdrr.io/r/base/ISOdatetime.html) is rather slow.

## Usage

``` r
ISOyear(year)
```

## Arguments

- year:

  Vector of years to convert to lubridate::POSIXct.

## Value

A vector of lubridate::POSIXct values.

## Author

Michaja Pehl

## Examples

``` r
ISOyear(c(2005, 2010, 2100, 1900))
#> [1] "2005-07-02 12:00:00 GMT" "2010-07-02 12:00:00 GMT"
#> [3] "2100-07-02 12:00:00 GMT" "1900-07-02 12:00:00 GMT"
```
