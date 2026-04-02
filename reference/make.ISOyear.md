# speedily converting years to POSIXct values

**\[deprecated\]**

## Usage

``` r
make.ISOyear(years)
```

## Arguments

- years:

  ignored.

## Value

The [`ISOyear()`](ISOyear.md) function.

## Details

This function was deprecated because the [`ISOyear()`](ISOyear.md)
function can be used directly.

## Examples

``` r
ISOyear <- make.ISOyear()
#> Warning: `make.ISOyear()` was deprecated in quitte 0.3108.0.
#> ℹ The function ISOyear() can be used directly.
ISOyear(c(2005, 2010, 2100, 1900))
#> [1] "2005-07-02 12:00:00 GMT" "2010-07-02 12:00:00 GMT"
#> [3] "2100-07-02 12:00:00 GMT" "1900-07-02 12:00:00 GMT"
# ->
ISOyear(c(2005, 2010, 2100, 1900))
#> [1] "2005-07-02 12:00:00 GMT" "2010-07-02 12:00:00 GMT"
#> [3] "2100-07-02 12:00:00 GMT" "1900-07-02 12:00:00 GMT"
```
