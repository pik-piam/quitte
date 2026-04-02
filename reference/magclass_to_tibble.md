# Make a Tibble of a Magclass

Sensible `magclass` to `tibble` conversion.

## Usage

``` r
magclass_to_tibble(m, colnames = NULL)
```

## Arguments

- m:

  A [`magpie`](https://rdrr.io/pkg/magclass/man/magclass-package.html)
  object.

- colnames:

  Column names for the returned `tibble`. Must match the number of
  columns.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html).

## Examples

``` r
magclass_to_tibble(magclass::maxample('pop'))
#> # A tibble: 320 × 4
#>    i         t scenario value
#>    <chr> <int> <chr>    <dbl>
#>  1 AFR    1995 A2        553.
#>  2 CPA    1995 A2       1281.
#>  3 EUR    1995 A2        554.
#>  4 FSU    1995 A2        276.
#>  5 LAM    1995 A2        452.
#>  6 MEA    1995 A2        278.
#>  7 NAM    1995 A2        292.
#>  8 PAO    1995 A2        134.
#>  9 PAS    1995 A2        383.
#> 10 SAS    1995 A2       1270.
#> # ℹ 310 more rows
```
