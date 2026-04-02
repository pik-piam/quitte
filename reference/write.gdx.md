# Write quitte data frame to a `.gdx` file

Writes one or more variables from a quitte data frame as GAMS parameters
to a `.gdx` file using
[`gamstransfer::gamstransfer`](https://rdrr.io/pkg/gamstransfer/man/gamstransfer-package.html).

## Usage

``` r
write.gdx(qf, path, varmap, dimCols = c("region", "period"), verbose = FALSE)
```

## Arguments

- qf:

  A quitte data frame.

- path:

  Path to the `.gdx` file to write.

- varmap:

  Named character vector mapping quitte `variable` values to valid GAMS
  parameter names, e.g.
  `c("Emissions|CO2" = "emico2", "GDP|PPP" = "gdpPPP")`. Only variables
  present as names in `varmap` are written; others are dropped with a
  warning.

- dimCols:

  Character vector of quitte columns to use as GAMS parameter
  dimensions. Defaults to `c("region", "period")`.

- verbose:

  If `TRUE`, warn about variables in `qf` not present in `varmap`.
  Defaults to `FALSE`.

## Value

Invisibly returns `qf`.

## Author

Tonn Rueter
