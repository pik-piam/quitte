# REMIND time steps

A data frame containing the weights with which years contribute to
specific periods.

## Usage

``` r
remind_timesteps
```

## Author

Michaja Pehl

## Examples

``` r
require(tidyverse)
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ─────────────── tidyverse 2.0.0 ──
#> ✔ forcats   1.0.1     ✔ readr     2.2.0
#> ✔ ggplot2   4.0.2     ✔ stringr   1.6.0
#> ✔ lubridate 1.9.5     ✔ tibble    3.3.1
#> ✔ purrr     1.2.1     
#> ── Conflicts ───────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

remind_timesteps %>%
    filter(period %in% c(2055, 2060, 2070)) %>%
    spread(period, weight, fill = 0) %>%
    print(n = Inf)
#> # A tibble: 23 × 4
#>     year `2055` `2060` `2070`
#>    <int>  <dbl>  <dbl>  <dbl>
#>  1  2053      1    0      0  
#>  2  2054      1    0      0  
#>  3  2055      1    0      0  
#>  4  2056      1    0      0  
#>  5  2057      1    0      0  
#>  6  2058      0    1      0  
#>  7  2059      0    1      0  
#>  8  2060      0    1      0  
#>  9  2061      0    1      0  
#> 10  2062      0    1      0  
#> 11  2063      0    1      0  
#> 12  2064      0    1      0  
#> 13  2065      0    0.5    0.5
#> 14  2066      0    0      1  
#> 15  2067      0    0      1  
#> 16  2068      0    0      1  
#> 17  2069      0    0      1  
#> 18  2070      0    0      1  
#> 19  2071      0    0      1  
#> 20  2072      0    0      1  
#> 21  2073      0    0      1  
#> 22  2074      0    0      1  
#> 23  2075      0    0      0.5
```
