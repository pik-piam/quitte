# calculate quantiles

calculate quantiles

## Usage

``` r
quitte2quantiles(
  x,
  probs = c(q0 = 0, q25 = 0.25, q50 = 0.5, q75 = 0.75, q100 = 1),
  grouping = c("region", "variable", "period", "scenario")
)
```

## Arguments

- x:

  dataframe to add quantiles

- probs:

  default=c(q0=0,q25=0.25,q50=0.5,q75=0.75,q100=1)

- grouping:

  default=c("region", "variable", "period", "scenario")

## Author

Gunnar Luderer, Lavinia Baumstark

## Examples

``` r
  if (FALSE) { # \dontrun{
    p <- x.minmax = quitte2quantiles(x,probs=c("min"=0,"max"=1))
  } # }
```
