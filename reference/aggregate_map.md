# Aggregates or disaggregates a data frame

Aggregates or disaggregates the values of a data frame according to a
mapping

## Usage

``` r
aggregate_map(
  data,
  mapping,
  by,
  subset2agg = NULL,
  only.new = TRUE,
  na.rm = TRUE,
  weights = NULL,
  forceAggregation = FALSE,
  autodetect = "auto",
  scaleWeights = TRUE,
  variable = "variable",
  value = "value",
  unit = "unit",
  weight_val_col = "weight_val_col",
  weight_item_col = NULL,
  fun = sum
)
```

## Arguments

- data:

  a data frame.

- mapping:

  a data frame connecting the resolution in `"data"` and the wished
  resolution

- by:

  (named) vector giving the correspondence between the column name of
  `"data"` and of the `"mapping"`

- subset2agg:

  subset of variables for which the (dis)aggregation is applied. If
  `NULL` (the default), the (dis)aggregation is applied to all
  variables.

- only.new:

  If `FALSE` (the default), add the (dis)aggregated data frame to
  existing ones. If `TRUE`, return only the (dis)aggregated data frame.

- na.rm:

  If `TRUE` (the default), remove items calculated as `NA`.

- weights:

  a data frame, a variable name as a character vector, or `NULL`. See
  details.

- forceAggregation:

  binary. If `TRUE`, (dis)aggregation will be applied even though the
  items contained contained in the data and in the data do not fully
  match. The data is reduced to the items covered both by the mapping
  and the data.

- autodetect:

  this parameter takes the values `'auto'`, `'aggregate'` or
  `'disaggregate'`. If `'auto'` (the default) the function tries to
  auto-detect whether this is an aggregation or a disaggregation. If
  `'aggregate'`, it will aggregate, if `'disaggregate'`, it will
  disaggregate.

- scaleWeights:

  logical. If `TRUE`, weights are scaled so that the sum of the
  components equals the value of the larger category.

- variable:

  Column name of variables. Defaults to `"variable"`.

- value:

  Column name of values. Defaults to `"value"`.

- unit:

  Column name of units. Defaults to `"unit"`.

- weight_val_col:

  name of the value column in the `"weigths"` data frame, if `"weigths"`
  is a data frame

- weight_item_col:

  name of the item column in the `"weigths"` data frame, if `"weigths"`
  is a data frame. The item column is the column corresponding to the
  mapping

- fun:

  aggregation function to use. Defaults to `sum`.

## Value

A data frame.

## Details

By default `"weights"` is set to `NULL`. For aggregations, this means
that values will be summed as they are. For disaggregations, each
component of the larger category will take the same value as the larger
category, or `"scaleWeights"` is `TRUE`, each component will be given an
even weight. For aggregations, `"weights"` can also be the name of a
variable contained in `"data"`. `"weights"` may also be a data frame.

## Author

Antoine Levesque

## Examples

``` r
library(tidyr)
library(dplyr)
data <- inline.data.frame(c(
  "model;    scenario;   region;   variable;           unit;         period;   value",
  "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2010;     40000",
  "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2020;     50000",
  "REMIND;   Baseline;   USA;      Population;         million;      2010;     300",
  "REMIND;   Baseline;   USA;      Population;         million;      2020;     350",
  "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2010;     7000",
  "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2020;     8000",
  "REMIND;   Baseline;   CHN;      Population;         million;      2010;     1300",
  "REMIND;   Baseline;   CHN;      Population;         million;      2020;     1400"))

mapping = inline.data.frame(c(
  "region;      New_region",
  "USA;         GLO",
  "CHN;         GLO"
))

mapping2 = inline.data.frame(c(
  "Item      ;         Item_new",
  "Population;         Urban Population ",
  "Population;         Rural Population"
))

weights = inline.data.frame(c(
  "region; itemI           ;   weight",
  "USA   ; Urban Population;      0.5",
  "USA   ; Rural Population;      0.2",
  "CHN   ; Urban Population;      2",
  "CHN   ; Rural Population;      1"
))

#Regional Aggregation
aggregate_map(data,mapping, by = "region", subset2agg = c("Population"))
#> # A tibble: 2 × 7
#>   model  scenario variable   unit    period region value
#>   <chr>  <chr>    <chr>      <chr>    <int> <chr>  <dbl>
#> 1 REMIND Baseline Population million   2010 GLO     1600
#> 2 REMIND Baseline Population million   2020 GLO     1750

#Regional Weighted Aggregation
aggregate_map(data,mapping, by = "region", subset2agg = "GDP per Capita|MER",
              weights = "Population")
#> # A tibble: 2 × 7
#>   model  scenario variable           unit       period region  value
#>   <chr>  <chr>    <chr>              <chr>       <int> <chr>   <dbl>
#> 1 REMIND Baseline GDP per Capita|MER US$2005/yr   2010 GLO    13188.
#> 2 REMIND Baseline GDP per Capita|MER US$2005/yr   2020 GLO    16400 

#Variable Weigthed Disaggregation
aggregate_map(data,mapping2, by = c("variable" = "Item"),
              subset2agg = c("Population"),weights = weights,
              weight_val_col = "weight", weight_item_col = "itemI")
#> # A tibble: 8 × 7
#>   model  scenario region unit    period value variable        
#>   <chr>  <chr>    <chr>  <chr>    <int> <dbl> <chr>           
#> 1 REMIND Baseline USA    million   2010 214.  Urban Population
#> 2 REMIND Baseline USA    million   2010  85.7 Rural Population
#> 3 REMIND Baseline USA    million   2020 250   Urban Population
#> 4 REMIND Baseline USA    million   2020 100   Rural Population
#> 5 REMIND Baseline CHN    million   2010 867.  Urban Population
#> 6 REMIND Baseline CHN    million   2010 433.  Rural Population
#> 7 REMIND Baseline CHN    million   2020 933.  Urban Population
#> 8 REMIND Baseline CHN    million   2020 467.  Rural Population

```
