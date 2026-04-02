# Interpolate missing periods

Adds missing periods to data frame and interpolates missing values
linearly or using splines from adjacent existing ones. Values for
periods smaller/bigger than the existing ones can be filled with the
values for the first/last available period in the case of linear
interpolation.

## Usage

``` r
interpolate_missing_periods(
  data,
  ...,
  value = "value",
  expand.values = FALSE,
  method = "linear",
  combinations = "nesting"
)

interpolate_missing_periods_(
  data,
  periods,
  value = "value",
  expand.values = FALSE,
  method = "linear",
  combinations = "nesting"
)
```

## Arguments

- data:

  A data frame or a quitte object.

- ...:

  A name-value pair of periods to fill. If unnamed, defaults to
  `'period'`. If empty (but possibly named) uses only periods present in
  `data`.

- value:

  Name of the column to fill, defaults to `'value'`.

- expand.values:

  If `FALSE` (the default), values are not expanded beyond the range of
  available data. If `TRUE` values at the closest extreme is used for
  linear interpolation. Results for spline interpolation are possibly
  nonsensical.

- method:

  Specifies the interpolation method. Either `'linear'` for linear
  interpolation or `'spline'`, `'spline_fmm'`, or `'spline_natural'` for
  spline interpolation. `'spline'` is an alias for `'spline_fmm'`. See
  [`spline()`](https://rdrr.io/r/stats/splinefun.html) for details.

- combinations:

  Specifies the method with which other columns are treated. They are
  either preserved as-is (`'nesting'`, the default), or are expanded to
  all unique combinations (`'crossing'`). See
  [`tidyr::expand()`](https://tidyr.tidyverse.org/reference/expand.html)
  for details.

- periods:

  A named list of periods to fill.

## Value

A data frame or a quitte object, the same as `data`.

## Author

Michaja Pehl

## Examples

``` r
require(dplyr)

# generate some test data with explicit (A-y-2025) and implicit (B-x-2030)
# missing values
(data <- tibble(
    group  = rep(c('A', 'B'), c(8, 4)),
    item   = c(rep('x', 4), rep('y', 4), rep('x', 4)),
    period = rep(c(2015, 2025, 2030, 2035), 3),
    value  = c(2, 4, 5, 6, 20, NA, 50, 60, NA, 400, 500, NA)))
#> # A tibble: 12 × 4
#>    group item  period value
#>    <chr> <chr>  <dbl> <dbl>
#>  1 A     x       2015     2
#>  2 A     x       2025     4
#>  3 A     x       2030     5
#>  4 A     x       2035     6
#>  5 A     y       2015    20
#>  6 A     y       2025    NA
#>  7 A     y       2030    50
#>  8 A     y       2035    60
#>  9 B     x       2015    NA
#> 10 B     x       2025   400
#> 11 B     x       2030   500
#> 12 B     x       2035    NA

# fill values for already existing periods
interpolate_missing_periods(data)
#> # A tibble: 12 × 4
#>    group item  period value
#>    <chr> <chr>  <dbl> <dbl>
#>  1 A     x       2015     2
#>  2 A     x       2025     4
#>  3 A     x       2030     5
#>  4 A     x       2035     6
#>  5 A     y       2015    20
#>  6 A     y       2025    40
#>  7 A     y       2030    50
#>  8 A     y       2035    60
#>  9 B     x       2015    NA
#> 10 B     x       2025   400
#> 11 B     x       2030   500
#> 12 B     x       2035    NA

# fill values for existing periods, with full combinations of other columns
interpolate_missing_periods(data, combinations = 'crossing')
#> # A tibble: 16 × 4
#>    group item  period value
#>    <chr> <chr>  <dbl> <dbl>
#>  1 A     x       2015     2
#>  2 A     x       2025     4
#>  3 A     x       2030     5
#>  4 A     x       2035     6
#>  5 A     y       2015    20
#>  6 A     y       2025    40
#>  7 A     y       2030    50
#>  8 A     y       2035    60
#>  9 B     x       2015    NA
#> 10 B     x       2025   400
#> 11 B     x       2030   500
#> 12 B     x       2035    NA
#> 13 B     y       2015    NA
#> 14 B     y       2025    NA
#> 15 B     y       2030    NA
#> 16 B     y       2035    NA

# add additional periods and fill values
interpolate_missing_periods(data, period = seq(2010, 2035, 5))
#> # A tibble: 18 × 4
#>    group item  period value
#>    <chr> <chr>  <dbl> <dbl>
#>  1 A     x       2010    NA
#>  2 A     x       2015     2
#>  3 A     x       2020     3
#>  4 A     x       2025     4
#>  5 A     x       2030     5
#>  6 A     x       2035     6
#>  7 A     y       2010    NA
#>  8 A     y       2015    20
#>  9 A     y       2020    30
#> 10 A     y       2025    40
#> 11 A     y       2030    50
#> 12 A     y       2035    60
#> 13 B     x       2010    NA
#> 14 B     x       2015    NA
#> 15 B     x       2020    NA
#> 16 B     x       2025   400
#> 17 B     x       2030   500
#> 18 B     x       2035    NA

# also fill values outside the original data range
interpolate_missing_periods(data, seq(2010, 2035, 5), expand.values = TRUE)
#> # A tibble: 18 × 4
#>    group item  period value
#>    <chr> <chr>  <dbl> <dbl>
#>  1 A     x       2010     2
#>  2 A     x       2015     2
#>  3 A     x       2020     3
#>  4 A     x       2025     4
#>  5 A     x       2030     5
#>  6 A     x       2035     6
#>  7 A     y       2010    20
#>  8 A     y       2015    20
#>  9 A     y       2020    30
#> 10 A     y       2025    40
#> 11 A     y       2030    50
#> 12 A     y       2035    60
#> 13 B     x       2010   400
#> 14 B     x       2015   400
#> 15 B     x       2020   400
#> 16 B     x       2025   400
#> 17 B     x       2030   500
#> 18 B     x       2035   500

# works on data frames with different column names
(data <- data %>%
        rename(year = period, coeff = value))
#> # A tibble: 12 × 4
#>    group item   year coeff
#>    <chr> <chr> <dbl> <dbl>
#>  1 A     x      2015     2
#>  2 A     x      2025     4
#>  3 A     x      2030     5
#>  4 A     x      2035     6
#>  5 A     y      2015    20
#>  6 A     y      2025    NA
#>  7 A     y      2030    50
#>  8 A     y      2035    60
#>  9 B     x      2015    NA
#> 10 B     x      2025   400
#> 11 B     x      2030   500
#> 12 B     x      2035    NA

interpolate_missing_periods(data, year, value = 'coeff')
#> # A tibble: 12 × 4
#>    group item   year coeff
#>    <chr> <chr> <dbl> <dbl>
#>  1 A     x      2015     2
#>  2 A     x      2025     4
#>  3 A     x      2030     5
#>  4 A     x      2035     6
#>  5 A     y      2015    20
#>  6 A     y      2025    40
#>  7 A     y      2030    50
#>  8 A     y      2035    60
#>  9 B     x      2015    NA
#> 10 B     x      2025   400
#> 11 B     x      2030   500
#> 12 B     x      2035    NA

# works on quitte objects too
(quitte <- data %>%
        rename(model = group, scenario = item, period = year, value = coeff) %>%
        mutate(variable = 'Var 1', unit = 'u1') %>%
        as.quitte())
#>    model scenario region variable unit period value
#> 1      A        x    GLO    Var 1   u1   2015     2
#> 2      A        x    GLO    Var 1   u1   2025     4
#> 3      A        x    GLO    Var 1   u1   2030     5
#> 4      A        x    GLO    Var 1   u1   2035     6
#> 5      A        y    GLO    Var 1   u1   2015    20
#> 6      A        y    GLO    Var 1   u1   2025    NA
#> 7      A        y    GLO    Var 1   u1   2030    50
#> 8      A        y    GLO    Var 1   u1   2035    60
#> 9      B        x    GLO    Var 1   u1   2015    NA
#> 10     B        x    GLO    Var 1   u1   2025   400
#> 11     B        x    GLO    Var 1   u1   2030   500
#> 12     B        x    GLO    Var 1   u1   2035    NA

interpolate_missing_periods(quitte, expand.values = TRUE)
#> # A tibble: 12 × 7
#>    model scenario region variable unit  period value
#>  * <fct> <fct>    <fct>  <fct>    <fct>  <int> <dbl>
#>  1 A     x        GLO    Var 1    u1      2015     2
#>  2 A     x        GLO    Var 1    u1      2025     4
#>  3 A     x        GLO    Var 1    u1      2030     5
#>  4 A     x        GLO    Var 1    u1      2035     6
#>  5 A     y        GLO    Var 1    u1      2015    20
#>  6 A     y        GLO    Var 1    u1      2025    40
#>  7 A     y        GLO    Var 1    u1      2030    50
#>  8 A     y        GLO    Var 1    u1      2035    60
#>  9 B     x        GLO    Var 1    u1      2015   400
#> 10 B     x        GLO    Var 1    u1      2025   400
#> 11 B     x        GLO    Var 1    u1      2030   500
#> 12 B     x        GLO    Var 1    u1      2035   500

# and works with POSIXct periods
(quitte <- quitte %>%
        mutate(period = ISOyear(period)))
#>    model scenario region variable unit              period value
#> 1      A        x    GLO    Var 1   u1 2015-07-02 12:00:00     2
#> 2      A        x    GLO    Var 1   u1 2025-07-02 12:00:00     4
#> 3      A        x    GLO    Var 1   u1 2030-07-02 12:00:00     5
#> 4      A        x    GLO    Var 1   u1 2035-07-02 12:00:00     6
#> 5      A        y    GLO    Var 1   u1 2015-07-02 12:00:00    20
#> 6      A        y    GLO    Var 1   u1 2025-07-02 12:00:00    NA
#> 7      A        y    GLO    Var 1   u1 2030-07-02 12:00:00    50
#> 8      A        y    GLO    Var 1   u1 2035-07-02 12:00:00    60
#> 9      B        x    GLO    Var 1   u1 2015-07-02 12:00:00    NA
#> 10     B        x    GLO    Var 1   u1 2025-07-02 12:00:00   400
#> 11     B        x    GLO    Var 1   u1 2030-07-02 12:00:00   500
#> 12     B        x    GLO    Var 1   u1 2035-07-02 12:00:00    NA

interpolate_missing_periods(quitte, period = ISOyear(seq(2010, 2035, 5)))
#> # A tibble: 18 × 7
#>    model scenario region variable unit      period  value
#>  * <fct> <fct>    <fct>  <fct>    <fct>      <int>  <dbl>
#>  1 A     x        GLO    Var 1    u1    1278072000  NA   
#>  2 A     x        GLO    Var 1    u1    1435838400   2   
#>  3 A     x        GLO    Var 1    u1    1593691200   3.00
#>  4 A     x        GLO    Var 1    u1    1751457600   4   
#>  5 A     x        GLO    Var 1    u1    1909224000   5   
#>  6 A     x        GLO    Var 1    u1    2066990400   6   
#>  7 A     y        GLO    Var 1    u1    1278072000  NA   
#>  8 A     y        GLO    Var 1    u1    1435838400  20   
#>  9 A     y        GLO    Var 1    u1    1593691200  30.0 
#> 10 A     y        GLO    Var 1    u1    1751457600  40.0 
#> 11 A     y        GLO    Var 1    u1    1909224000  50   
#> 12 A     y        GLO    Var 1    u1    2066990400  60   
#> 13 B     x        GLO    Var 1    u1    1278072000  NA   
#> 14 B     x        GLO    Var 1    u1    1435838400  NA   
#> 15 B     x        GLO    Var 1    u1    1593691200  NA   
#> 16 B     x        GLO    Var 1    u1    1751457600 400   
#> 17 B     x        GLO    Var 1    u1    1909224000 500   
#> 18 B     x        GLO    Var 1    u1    2066990400  NA   

# standard evaluation example
interpolate_missing_periods_(data, periods = list(year = seq(2010, 2035, 5)),
                             value = 'coeff', expand.values = TRUE)
#> # A tibble: 18 × 4
#>    group item   year coeff
#>    <chr> <chr> <dbl> <dbl>
#>  1 A     x      2010     2
#>  2 A     x      2015     2
#>  3 A     x      2020     3
#>  4 A     x      2025     4
#>  5 A     x      2030     5
#>  6 A     x      2035     6
#>  7 A     y      2010    20
#>  8 A     y      2015    20
#>  9 A     y      2020    30
#> 10 A     y      2025    40
#> 11 A     y      2030    50
#> 12 A     y      2035    60
#> 13 B     x      2010   400
#> 14 B     x      2015   400
#> 15 B     x      2020   400
#> 16 B     x      2025   400
#> 17 B     x      2030   500
#> 18 B     x      2035   500
```
