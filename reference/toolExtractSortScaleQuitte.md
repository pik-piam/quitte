# prepare data for plots

prepare data for plots

## Usage

``` r
toolExtractSortScaleQuitte(
  x,
  scen,
  vars,
  var.scaling = 1,
  regi = c("World"),
  prd = getPeriods(x)
)
```

## Arguments

- x:

  dataframe to prepare

- scen:

  scenario to select

- vars:

  variables to select

- var.scaling:

  scaling of the variables, default=1

- regi:

  region to select, default="World"

- prd:

  period to select, default=getPeriods(x)

## Author

Gunnar Luderer, Lavinia Baumstark

## Examples

``` r
  if (FALSE) { # \dontrun{
    p <- toolExtractSortScaleQuitte(x,scen=c("BAU"),
                                   vars=c("Emi|CO2","FE|Industry"),
                                   regi=c("EUR","LAM"),prd=c(2005,2030,2050))
  } # }


```
