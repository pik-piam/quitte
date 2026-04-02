# Pivot Data Frame on Periods

Pivot data frame on periods either to wide or long format.

## Usage

``` r
pivot_periods(df, direction = NULL, fill = NA, drop_na = TRUE)

pivot_periods_wider(df, fill = NA)

pivot_periods_longer(df, drop_na = TRUE)
```

## Arguments

- df:

  A data frame.

- direction:

  Direction in which to pivot. Either `'wider'` or `'longer'`.
  Determined automatically for the default `NULL`.

- fill:

  A scalar value that fills in missing values when pivoting wider.
  Defaults to `NA`.

- drop_na:

  If `TRUE` (the default), will drop rows that contain only `NA`s in the
  `value` column when pivoting longer.

## Value

The pivoted data frame.

## See also

[`pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html),
[`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

## Examples

``` r
library(dplyr)

(x <- quitte_example_data %>%
        filter(first(scenario) == scenario,
               first(region) == region,
               variable %in% head(unique(variable), n = 7),
               period <= 2030) %>%
        slice_sample(prop = 0.9) %>%
        pivot_periods_wider())
#> # A tibble: 7 × 11
#>   model  scenario  region variable unit   `2005`  `2010`  `2015` `2020`
#>   <fct>  <fct>     <fct>  <fct>    <fct>   <dbl>   <dbl>   <dbl>  <dbl>
#> 1 REMIND r7552c_1… AFR    PE|Coal  EJ/yr   0.183  NA        1.24 1.29e0
#> 2 REMIND r7552c_1… AFR    GDP|PPP  bill… 878.     NA     1723.   2.20e3
#> 3 REMIND r7552c_1… AFR    PE|Biom… EJ/yr  11.7    12.4     NA    1.28e1
#> 4 REMIND r7552c_1… AFR    PE|Gas   EJ/yr   0.387   0.627    1.40 2.11e0
#> 5 REMIND r7552c_1… AFR    Consump… bill… 618.    735.     978.   9.90e2
#> 6 REMIND r7552c_1… AFR    Populat… mill… 710.    815.     918.   1.02e3
#> 7 REMIND r7552c_1… AFR    PE|Oil   EJ/yr   1.95    2.30     3.41 4.86e0
#> # ℹ 2 more variables: `2025` <dbl>, `2030` <dbl>

x %>%
    pivot_periods_longer()
#> # A tibble: 37 × 7
#>    model  scenario              region variable unit     period   value
#>  * <fct>  <fct>                 <fct>  <fct>    <fct>     <int>   <dbl>
#>  1 REMIND r7552c_1p5C_Def-rem-5 AFR    PE|Coal  EJ/yr      2005 1.83e-1
#>  2 REMIND r7552c_1p5C_Def-rem-5 AFR    PE|Coal  EJ/yr      2015 1.24e+0
#>  3 REMIND r7552c_1p5C_Def-rem-5 AFR    PE|Coal  EJ/yr      2020 1.29e+0
#>  4 REMIND r7552c_1p5C_Def-rem-5 AFR    PE|Coal  EJ/yr      2025 7.09e-1
#>  5 REMIND r7552c_1p5C_Def-rem-5 AFR    PE|Coal  EJ/yr      2030 1.68e-1
#>  6 REMIND r7552c_1p5C_Def-rem-5 AFR    GDP|PPP  billion…   2005 8.78e+2
#>  7 REMIND r7552c_1p5C_Def-rem-5 AFR    GDP|PPP  billion…   2015 1.72e+3
#>  8 REMIND r7552c_1p5C_Def-rem-5 AFR    GDP|PPP  billion…   2020 2.20e+3
#>  9 REMIND r7552c_1p5C_Def-rem-5 AFR    GDP|PPP  billion…   2025 2.88e+3
#> 10 REMIND r7552c_1p5C_Def-rem-5 AFR    GDP|PPP  billion…   2030 3.82e+3
#> # ℹ 27 more rows
```
