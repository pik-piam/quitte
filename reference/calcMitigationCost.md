# Calculate mitigation costs

Calculate mitigation costs

## Usage

``` r
calcMitigationCost(
  data,
  scenBau,
  scenPol,
  yearFrom = 2010,
  yearTo = 2100,
  nameVar = "Consumption",
  nameDisrate = "Interest Rate t/(t-1)|Real",
  discount = 0.05
)
```

## Arguments

- data:

  quitte object

- scenBau:

  baseline scenario name

- scenPol:

  policy scenario name

- yearFrom:

  the startyear

- yearTo:

  the endyear

- nameVar:

  Name of the variable containing consumption. Defaults to "Consumption"

- nameDisrate:

  Name of the variable for the discount rate, only needed if
  discount=endo. Defaults to "Interest Rate t/(t-1)\|Real"

- discount:

  discount rate - exogenous only for now

## Value

regional mitigation costs (quitte object)

## Author

Anselm Schultes

## Examples

``` r
  if (FALSE) { # \dontrun{
    calcMitigationCost(qd,"BAU","POL")
  } # }
```
