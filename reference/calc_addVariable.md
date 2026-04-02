# Calculate new variables

Calculate new variables from existing ones, using generic formulas.

## Usage

``` r
calc_addVariable(
  data,
  ...,
  units = NA,
  na.rm = TRUE,
  completeMissing = FALSE,
  only.new = FALSE,
  variable = variable,
  unit = unit,
  value = value,
  overwrite = TRUE,
  skip.missing.rhs = FALSE
)

calc_addVariable_(
  data,
  .dots,
  na.rm = TRUE,
  completeMissing = FALSE,
  only.new = FALSE,
  variable = "variable",
  unit = "unit",
  value = "value",
  overwrite = TRUE,
  skip.missing.rhs = FALSE
)
```

## Arguments

- data:

  A data frame.

- ...:

  Formulas to calculate new variables. Either name-value pairs, the path
  to a .csv file, the content of a .csv file as a string, or a data
  frame. See details.

- units:

  Character vector of units corresponding to new variables. Must be of
  length equal to `...` or of length one (in which case all new
  variables receive the same unit).

- na.rm:

  If `TRUE` (the default), remove items calculated as `NA`. This is
  generally the case for all calculations involving `NA` values, and all
  calculations involving missing variables. See `completeMissing`
  parameter.

- completeMissing:

  If `TRUE`, implicitly missing data, i.e. missing combinations of input
  data, are filled up with 0 before the calculation, and they are
  therefore not computed as `NA` (and potentially removed from the
  output). Make sure `0` is a sensible value for your calculations, else
  complete missing values manually. Defaults to `FALSE`.

- only.new:

  If `FALSE` (the default), add new variables to existing ones. If
  `TRUE`, return only new variables.

- variable:

  Column name of variables. Defaults to `"variable"`.

- unit:

  Column name of units. Defaults to `"unit"`. Ignored if no column with
  the same name is in `data` (e.g. data frames without unit column).

- value:

  Column name of values. Defaults to `"value"`.

- overwrite:

  If `TRUE` (the default), values are overwritten if they already exist.
  If `FALSE` values are discarded and not overwritten if they already
  exist

- skip.missing.rhs:

  If `FALSE` (the default), fail if any right-hand-side variable is
  missing. If `TRUE`, warn, and skip that calculation. If `"silent"`,
  skip without warning.

- .dots:

  Used to work around non-standard evaluation. See details.

  If `...` is a list of name-value pairs, it has the general format

      "lhs" = "rhs + calculations - formula", "`lhs 2`" = "lhs / `rhs 2`"

  where `lhs` are the names of new variables to be calculated and `rhs`
  are the variables to calculate from. If `lhs` and `rhs` are no proper
  *identifiers*, they need to be quoted (see
  [Quotes](https://rdrr.io/r/base/Quotes.html) for details). When in
  doubt, just quote.

  If the new variables should have units, set `units` appropriately.

  `.dots` is a named list of strings denoting formulas and optionally
  units. The general format is

      list("`lhs 1`" = "`rhs` / `calculation`",
           "`lhs 2`" = "sin(`rhs 2`)")

  Units are optionally included with the formulas in a vector like

      list("`lhs w/ unit`" = c("`rhs 1` + `rhs 2`", "rhs unit")

  Units do not require quoting.

  As an alternative, the variable, unit and formula can be specified as
  a .csv file in this format:

      variable;                unit;      formula
      Carbon Intensity|Cement; Mt CO2/Mt; `Emi|CO2|Cement` / `Production|Cement`

  or as a single string containing the .csv content (joined by `\n` line
  breaks)

      paste(c(
          "variable;         unit;        formula",
          "Consumption|pCap; US$2005/cap; 0.001 * `Consumption`/`Population`"),
          collapse = '\n')

  or as a data frame with the same columns:

      data.frame(
          variable = 'Consumption|pCap',
          unit     = 'US$2005/cap',
          formula  = '0.001 * `Consumption`/`Population`')

  `...` and `.dots` are processed in order, and variables already
  calculated in the same call can be used for further calculations.
  Other existing columns, including `period`, can be referenced, but
  this is not supported and the results are considered *undefined*.

## Value

A data frame.

## Author

Michaja Pehl

## Examples

``` r
data <- inline.data.frame(c(
    "model;    scenario;   region;   variable;     unit;                 period;   value",
    "REMIND;   Baseline;   USA;      GDP|MER;      billion US$2005/yr;   2010;     12990",
    "REMIND;   Baseline;   USA;      Population;   million;              2010;       310.4",
    "REMIND;   Baseline;   USA;      PE;           EJ/yr;                2010;        91.62",
    "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2020;      8882",
    "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2010;      4119",
    "REMIND;   Baseline;   CHN;      Population;   million;              2020;      1387",
    "REMIND;   Baseline;   CHN;      Population;   million;              2010;      1349"))

calc_addVariable(data, "GDPpC" = "`GDP|MER` / Population * 1e3",
                       "`ln GDPpC`" = "log(GDPpC)",
                       units = c("US$2005/cap", NA))
#> # A tibble: 13 × 7
#>    model  scenario region variable   unit               period    value
#>    <chr>  <chr>    <chr>  <chr>      <chr>               <int>    <dbl>
#>  1 REMIND Baseline USA    GDP|MER    billion US$2005/yr   2010 12990   
#>  2 REMIND Baseline USA    Population million              2010   310.  
#>  3 REMIND Baseline USA    PE         EJ/yr                2010    91.6 
#>  4 REMIND Baseline CHN    GDP|MER    billion US$2005/yr   2020  8882   
#>  5 REMIND Baseline CHN    GDP|MER    billion US$2005/yr   2010  4119   
#>  6 REMIND Baseline CHN    Population million              2020  1387   
#>  7 REMIND Baseline CHN    Population million              2010  1349   
#>  8 REMIND Baseline USA    GDPpC      US$2005/cap          2010 41849.  
#>  9 REMIND Baseline CHN    GDPpC      US$2005/cap          2020  6404.  
#> 10 REMIND Baseline CHN    GDPpC      US$2005/cap          2010  3053.  
#> 11 REMIND Baseline USA    ln GDPpC   NA                   2010    10.6 
#> 12 REMIND Baseline CHN    ln GDPpC   NA                   2020     8.76
#> 13 REMIND Baseline CHN    ln GDPpC   NA                   2010     8.02
calc_addVariable_(
    data,
    list("`GDPpC`"    = c("`GDP|MER` / `Population` * 1e3", "US$/cap"),
         "`ln GDPpC`" = "log(`GDPpC`)")
)
#> # A tibble: 13 × 7
#>    model  scenario region variable   unit               period    value
#>    <chr>  <chr>    <chr>  <chr>      <chr>               <int>    <dbl>
#>  1 REMIND Baseline USA    GDP|MER    billion US$2005/yr   2010 12990   
#>  2 REMIND Baseline USA    Population million              2010   310.  
#>  3 REMIND Baseline USA    PE         EJ/yr                2010    91.6 
#>  4 REMIND Baseline CHN    GDP|MER    billion US$2005/yr   2020  8882   
#>  5 REMIND Baseline CHN    GDP|MER    billion US$2005/yr   2010  4119   
#>  6 REMIND Baseline CHN    Population million              2020  1387   
#>  7 REMIND Baseline CHN    Population million              2010  1349   
#>  8 REMIND Baseline USA    GDPpC      US$/cap              2010 41849.  
#>  9 REMIND Baseline CHN    GDPpC      US$/cap              2020  6404.  
#> 10 REMIND Baseline CHN    GDPpC      US$/cap              2010  3053.  
#> 11 REMIND Baseline USA    ln GDPpC   NA                   2010    10.6 
#> 12 REMIND Baseline CHN    ln GDPpC   NA                   2020     8.76
#> 13 REMIND Baseline CHN    ln GDPpC   NA                   2010     8.02
```
