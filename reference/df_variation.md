# Data Frame Variation

Removes all columns from a data frame which have only identical data, to
facilitate a quick overview.

## Usage

``` r
df_variation(x)
```

## Arguments

- x:

  A data frame.

## Value

A data frame.

## Examples

``` r
(quitte_example_data['Consumption' == quitte_example_data$variable,] -> x)
#> # A tibble: 1,140 × 7
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
#> # ℹ 1,130 more rows

df_variation(x)
#> # A tibble: 1,140 × 4
#>    scenario              region period value
#>    <fct>                 <fct>   <int> <dbl>
#>  1 r7552c_1p5C_Def-rem-5 AFR      2005  618.
#>  2 r7552c_1p5C_Def-rem-5 AFR      2010  735.
#>  3 r7552c_1p5C_Def-rem-5 AFR      2015  978.
#>  4 r7552c_1p5C_Def-rem-5 AFR      2020  990.
#>  5 r7552c_1p5C_Def-rem-5 AFR      2025 1248.
#>  6 r7552c_1p5C_Def-rem-5 AFR      2030 1555.
#>  7 r7552c_1p5C_Def-rem-5 AFR      2035 1900.
#>  8 r7552c_1p5C_Def-rem-5 AFR      2040 2284.
#>  9 r7552c_1p5C_Def-rem-5 AFR      2045 2703.
#> 10 r7552c_1p5C_Def-rem-5 AFR      2050 3185.
#> # ℹ 1,130 more rows
```
