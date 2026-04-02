# Check IAMC-style data frame for inconsistencies.

Check an IAMC-style data frame to see if values across variables and
regions sum up to the totals specified within the data frame.

## Usage

``` r
check_quitte(quitte, check_variables, check_regions = NULL)
```

## Arguments

- quitte:

  IAMC-style data frame.

- check_variables:

  List, string or file of variables to check.

- check_regions:

  List, string or file of regions to check.

## Value

A data frame of all entries that did not match.

## Details

Checking is performed for all variables and regions in `check_variables`
and `check_regions`, which can be passed as a list of format

    list("sum" = c("summand1", "summand2", ...))

a character string of format

    sum1
    summand1a
    summand1b

    sum2
    summand2a
    ...

or as the path to a text file with this format.

If checking should be performed for variables or regions that are
neither sum nor summand (e.g., the variable 'GDP' should be summed
across regions, but is itself not a sum of other variables), include
them as sum and their only summand in the respective list (i.e.,
`list("GDP" = "GDP")` or as a double line in the character string or
file.

If `check_regions` is `NULL`, variables are check across all regions in
`quitte`.

## Author

Michaja Pehl

## Examples

``` r
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
quitte <- rbind(
    data.frame(
        model    = "REMIND",
        scenario = "Baseline",
        region   = c("World", "USA", "EUR"),
        variable = "GDP",
        unit     = "US$2005",
        period   = 2005,
        value    = c(3, 1, 1)
    ),

    data.frame(
        model    = "REMIND",
        scenario = "Baseline",
        region   = "ROW",
        variable = c("FE|Total", "FE|Solids", "FE|Electricity"),
        unit     = "EJ/a",
        period   = 2005,
        value    = c(3, 1, 1)
    )
)

check_variables <- list(
    "FE|Total" = c("FE|Solids", "FE|Electricity"),
    "GDP"      = "GDP")

check_regions <- paste0("World\nUSA\nEUR\n\nROW\nROW")

print(quitte)
#>    model scenario region       variable    unit period value
#> 1 REMIND Baseline  World            GDP US$2005   2005     3
#> 2 REMIND Baseline    USA            GDP US$2005   2005     1
#> 3 REMIND Baseline    EUR            GDP US$2005   2005     1
#> 4 REMIND Baseline    ROW       FE|Total    EJ/a   2005     3
#> 5 REMIND Baseline    ROW      FE|Solids    EJ/a   2005     1
#> 6 REMIND Baseline    ROW FE|Electricity    EJ/a   2005     1
print(check_variables)
#> $`FE|Total`
#> [1] "FE|Solids"      "FE|Electricity"
#> 
#> $GDP
#> [1] "GDP"
#> 
cat(check_regions)
#> World
#> USA
#> EUR
#> 
#> ROW
#> ROW

check_quitte(quitte, check_variables, check_regions)
#>    model scenario region variable    unit period value sum.value
#> 1 REMIND Baseline    ROW FE|Total    EJ/a   2005     3         2
#> 2 REMIND Baseline  World      GDP US$2005   2005     3         2
```
