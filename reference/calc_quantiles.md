# Sample Quantiles

This is a wrapper function for
[quantile](https://rdrr.io/r/stats/quantile.html) for easy use with data
frames.

## Usage

``` r
calc_quantiles(
  .data,
  value = NA,
  probs = c(q0 = 0, q25 = 0.25, q50 = 0.5, q75 = 0.75, q100 = 1),
  na.rm = TRUE,
  type = 7
)

calc_quantiles_(
  .data,
  value = "value",
  probs = c(q0 = 0, q25 = 0.25, q50 = 0.5, q75 = 0.75, q100 = 1),
  na.rm = TRUE,
  type = 7
)
```

## Arguments

- .data:

  a data frame, possibly grouped

- value:

  column name for which sample quantiles should be calculated

- probs:

  named numeric vector of probabilities with values in \\\[0, 1\]\\.

- na.rm:

  logical; if `TRUE`, any [NA](https://rdrr.io/r/base/NA.html) and
  [NaN](https://rdrr.io/r/base/is.finite.html)s are removed from `data`
  before the quantiles are computed.

- type:

  an integer between 1 and 9 select one of the nine quantile algorithms
  detailed in [quantile](https://rdrr.io/r/stats/quantile.html) to be
  used.

## Value

A data frame.

## Author

Michaja Pehl

## Examples

``` r
require(dplyr)
require(tidyr)

tibble(group = rep(c("A", "B"), 10),
               value = 1:20) %>%
    group_by(group) %>%
    calc_quantiles() %>%
    pivot_wider(names_from = 'quantile')
#> # A tibble: 2 × 6
#> # Groups:   group [2]
#>   group    q0   q25   q50   q75  q100
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 A         1   5.5    10  14.5    19
#> 2 B         2   6.5    11  15.5    20
```
