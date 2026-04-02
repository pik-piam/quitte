# Calculates the cumulated discounted time series

Discount and cumulated a times series - gives the time series of the net
present value (NPV). Baseyear for the NPV is the first period.

## Usage

``` r
calcCumulatedDiscount(
  data,
  nameVar = "Consumption",
  nameDisrate = "Interest Rate t/(t-1)|Real",
  discount = 0.05,
  fixYear = "none"
)
```

## Arguments

- data:

  a quitte object containing consumption values - consumption has to be
  named "Consumption"

- nameVar:

  name of the variable to be cumulated (and discounted)

- nameDisrate:

  Name of the variable containing the discount rate

- discount:

  The discount rate: either a numeric value, or 'BAU' to choose the
  discount rate supplied in nameDisrate

- fixYear:

  From the discounted time series, substract the value in year fixYear,
  if fixYear is not 'none'

## Value

cumulated discounted values for each scenario, model, region (quitte
object)

## Author

Anselm Schultes

## Examples

``` r
  if (FALSE) { # \dontrun{
    erg <- calcCumulatedDiscount(data, disRate=0.03)
  } # }
```
