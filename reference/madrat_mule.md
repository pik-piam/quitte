# Madrat Mule

Convert *anything* to a
[`magpie`](https://rdrr.io/pkg/magclass/man/magclass-package.html)
object and back to traffic data across `madrat` borders.

## Usage

``` r
madrat_mule(x)
```

## Arguments

- x:

  Anything.

## Value

A [`magpie`](https://rdrr.io/pkg/magclass/man/magclass-package.html)
object containing `x` (in unusable form), or the original `x` if a
[`magpie`](https://rdrr.io/pkg/magclass/man/magclass-package.html)
object was passed.

## Author

Michaja Pehl

## Examples

``` r
str(x <- madrat_mule(quitte_example_data))
#> A magpie object (package: magclass)
#>  @ .Data:  int [1:614147] 88 10 0 0 0 3 0 4 5 3 ...
#>  $ dimnames:List of 3
#>   ..$ fake: chr "GLO"
#>   ..$ NA  : NULL
#>   ..$ NA  : NULL
madrat_mule(x)
#> # A tibble: 19,152 × 7
#>    model  scenario              region variable    unit    period value
#>    <fct>  <fct>                 <fct>  <fct>       <fct>    <int> <dbl>
#>  1 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2005  618.
#>  2 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2010  735.
#>  3 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2015  978.
#>  4 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2020  990.
#>  5 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2025 1248.
#>  6 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2030 1555.
#>  7 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2035 1900.
#>  8 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2040 2284.
#>  9 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2045 2703.
#> 10 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billio…   2050 3185.
#> # ℹ 19,142 more rows
```
