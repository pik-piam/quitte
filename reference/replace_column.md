# Replace data frame column

Replaces the column of a data frame with that from a mask data frame.

## Usage

``` r
replace_column(
  data,
  mask,
  ...,
  drop.extra = FALSE,
  ignore.ambiguous.match = FALSE
)

replace_column_(
  data,
  mask,
  old_column,
  match_column,
  new_column,
  drop.extra = FALSE,
  ignore.ambiguous.match = FALSE
)
```

## Arguments

- data:

  A data frame or a quitte object.

- mask:

  A data frame containing the `match_column` and the `new_column`.

- ...:

  Definition of *old*, *match*, and *new* columns, see details.

- drop.extra:

  Drop rows not present in *match* column?

- ignore.ambiguous.match:

  `replace_column()` will issue a warning if the *match* column in
  `mask` does not map unambiguously to `data`, unless it is suppressed
  (`TRUE`). Using ambiguous matches can be desired for duplicating
  specific rows.

- old_column:

  *old* column name, see details.

- match_column:

  *match* column name, see details.

- new_column:

  *new* column name, see details.

## Value

A data frame or a quitte object, same as `data`.

## Details

Replaces the *old* column in data frame `data` by the *new* column from
data frame `mask` based on the matching between *old* (`data`) and
*match* (`mask`) columns.

This can be used to replace columns based on a mapping to, e.g., rename
scenarios, regions, etc. in model data.

## Author

Michaja Pehl

## Examples

``` r
# simple example with matching old and match column names
(model_data <- data.frame(
    model  = c('Model1', '2ndModel', 'Model Three'),
    region = c('Region 1', 'Region 2', 'Region 1'),
    value  = 1:3))
#>         model   region value
#> 1      Model1 Region 1     1
#> 2    2ndModel Region 2     2
#> 3 Model Three Region 1     3

(mask <- data.frame(
    model  = c('Model1', '2ndModel', 'Model Three', 'fourth Model'),
    clear_name = paste('Model', 1:4)))
#>          model clear_name
#> 1       Model1    Model 1
#> 2     2ndModel    Model 2
#> 3  Model Three    Model 3
#> 4 fourth Model    Model 4

replace_column(model_data, mask, model, clear_name)
#>     model   region value
#> 1 Model 1 Region 1     1
#> 2 Model 2 Region 2     2
#> 3 Model 3 Region 1     3

# mismatched column names
(model_data <- data.frame(
    model  = c('Model1', '2ndModel', 'Model Three', 'fourth Model'),
    region = c('Region 1', 'Region 2', 'Region 1', 'Region 2'),
    value  = 1:4))
#>          model   region value
#> 1       Model1 Region 1     1
#> 2     2ndModel Region 2     2
#> 3  Model Three Region 1     3
#> 4 fourth Model Region 2     4

(mask <- data.frame(
    ugly_name  = c('Model1', '2ndModel', 'Model Three'),
    clear_name = paste('Model', 1:3)))
#>     ugly_name clear_name
#> 1      Model1    Model 1
#> 2    2ndModel    Model 2
#> 3 Model Three    Model 3

replace_column(model_data, mask, model = ugly_name, clear_name)
#>     model   region value
#> 1 Model 1 Region 1     1
#> 2 Model 2 Region 2     2
#> 3 Model 3 Region 1     3
#> 4    <NA> Region 2     4

# SE example
replace_column_(model_data, mask, 'model', 'ugly_name', 'clear_name')
#>     model   region value
#> 1 Model 1 Region 1     1
#> 2 Model 2 Region 2     2
#> 3 Model 3 Region 1     3
#> 4    <NA> Region 2     4

# dropping the extra entries in model
replace_column(model_data, mask, model = ugly_name, clear_name,
               drop.extra = TRUE)
#>     model   region value
#> 1 Model 1 Region 1     1
#> 2 Model 2 Region 2     2
#> 3 Model 3 Region 1     3

# also works on quitte objects
require(dplyr)
(quitte <- tibble(
    model    = c('Model1', '2ndModel'),
    scenario = 'Scenario',
    region   = 'Region',
    variable = 'Variable',
    unit     = 'Unit',
    period   = 2010,
    value    = 1:2) %>%
        as.quitte())
#> # A tibble: 2 × 7
#>   model    scenario region variable unit  period value
#> * <fct>    <fct>    <fct>  <fct>    <fct>  <int> <int>
#> 1 Model1   Scenario Region Variable Unit    2010     1
#> 2 2ndModel Scenario Region Variable Unit    2010     2
replace_column(quitte, mask, model = ugly_name, clear_name)
#> # A tibble: 2 × 7
#>   model   scenario region variable unit  period value
#> * <fct>   <fct>    <fct>  <fct>    <fct>  <int> <int>
#> 1 Model 1 Scenario Region Variable Unit    2010     1
#> 2 Model 2 Scenario Region Variable Unit    2010     2
str(.Last.value)
#> List of 3
#>  $ repos        : NULL
#>  $ Ncpus        : NULL
#>  $ HTTPUserAgent: chr "R/4.5.3 (ubuntu-24.04) R (4.5.3 x86_64-pc-linux-gnu x86_64 linux-gnu)"
```
