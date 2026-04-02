# Recreate a data frame as a tribble

Prints a representation of `df` using
[`tribble()`](https://tibble.tidyverse.org/reference/tribble.html) for
constructing examples or configuration data.

## Usage

``` r
tribbelise(df)
```

## Arguments

- df:

  A data frame.

## Examples

``` r
quitte_example_data |>
    head() |>
    print() |>
    tribbelise()
#> # A tibble: 6 × 7
#>   model  scenario              region variable    unit     period value
#>   <fct>  <fct>                 <fct>  <fct>       <fct>     <int> <dbl>
#> 1 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billion…   2005  618.
#> 2 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billion…   2010  735.
#> 3 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billion…   2015  978.
#> 4 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billion…   2020  990.
#> 5 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billion…   2025 1248.
#> 6 REMIND r7552c_1p5C_Def-rem-5 AFR    Consumption billion…   2030 1555.
#> tribble(
#>     ~model,     ~scenario,                 ~region,   ~variable,       ~unit,                  ~period,   ~value,
#>     'REMIND',   'r7552c_1p5C_Def-rem-5',   'AFR',     'Consumption',   'billion US$2005/yr',   2005,       617.8270,
#>     'REMIND',   'r7552c_1p5C_Def-rem-5',   'AFR',     'Consumption',   'billion US$2005/yr',   2010,       734.7417,
#>     'REMIND',   'r7552c_1p5C_Def-rem-5',   'AFR',     'Consumption',   'billion US$2005/yr',   2015,       978.2943,
#>     'REMIND',   'r7552c_1p5C_Def-rem-5',   'AFR',     'Consumption',   'billion US$2005/yr',   2020,       989.5978,
#>     'REMIND',   'r7552c_1p5C_Def-rem-5',   'AFR',     'Consumption',   'billion US$2005/yr',   2025,      1247.8070,
#>     'REMIND',   'r7552c_1p5C_Def-rem-5',   'AFR',     'Consumption',   'billion US$2005/yr',   2030,      1555.4722)
```
