# Calculates the growth rate in '%/yr' for variables

Calculates the growth rate in '%/yr' for variables

## Usage

``` r
calc_growthrate(x, only.new = FALSE, filter.function = identity)
```

## Arguments

- x:

  anything with an as.quitte method (data.frame, quitte or magclass
  object, mif file, ...)

- only.new:

  If `FALSE` (the default), add new variables to existing ones. If
  variable names already exist in the data.frame, they are replaced. If
  `TRUE`, return only new variables.

- filter.function:

  A function used to filter data before calculating growth rates. If
  instead a character vector is passed, only variables matching this
  vector are used.

## Value

data as a quitte object

## Details

If, for example, your data contains the data in 2070 and 2060, the
growth rate returned for 2070 is calculated as 100 \*
((d2070/d2060)^(1/10) - 1). No growth rate can be calculated for the
first year of the data. Infinite or undefined values (for example if
d2060 = 0) are dropped.

## Author

Oliver Richters

## Examples

``` r
if (FALSE) { # \dontrun{
GDPgrowth <- calc_growthrate(quitte_example_data, only.new = TRUE, filter.function = "GDP|PPP")
alldata <- calc_growthrate(quitte_example_data, only.new = FALSE,
                           filter.function = function(x) filter(x, grepl("GDP", .data$variable)))
} # }
```
