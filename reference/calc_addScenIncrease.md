# Add Scenario Change

These functions add the absolute increase over, the ratio relative to,
or the percent change relative to a reference scenario. Variable names
have `|Int over <reference_short_name>`,
`|Ratio to <reference_short_name>`, or
`|% Change to <reference_short_name>` appended, units are either
unmodified, or set to `ratio` or `%`, respectively.

## Usage

``` r
calc_addScenIncrease(
  d,
  variable,
  reference_scenario,
  reference_short_name = reference_scenario
)

calc_addScenRatio(
  d,
  variable,
  reference_scenario,
  reference_short_name = reference_scenario
)

calc_addScenPercentChange(
  d,
  variable,
  reference_scenario,
  reference_short_name = reference_scenario
)
```

## Arguments

- d:

  A quitte-like data frame.

- variable:

  A vector of variable names.

- reference_scenario:

  The scenario changes are calculated to.

- reference_short_name:

  Short name of reference scenario (appended to the variable name).

## Value

A quitte-like data frame with the calculated changes.

## Examples

``` r
quitte_example_data |>
    calc_addScenIncrease(c('Consumption', 'GDP|PPP'),
                         'r7552c_REF_Def05-rem-5', 'REF_Def05')
#> # A tibble: 1,824 × 7
#>    model  scenario              region variable     unit  period  value
#>    <fct>  <fct>                 <fct>  <chr>        <fct>  <int>  <dbl>
#>  1 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2005    0  
#>  2 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2010    0  
#>  3 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2015   11.2
#>  4 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2020 -240. 
#>  5 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2025 -322. 
#>  6 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2030 -431. 
#>  7 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2035 -549. 
#>  8 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2040 -667. 
#>  9 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2045 -790. 
#> 10 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption… bill…   2050 -910. 
#> # ℹ 1,814 more rows
```
