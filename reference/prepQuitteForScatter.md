# selects two variables from a long format Quitte and puts them into a wide format Quitte

QuitteIn contains two variables containedin varNames that should be
plotted in a scatter plot. The functions forms the new QuitteOut with
the variables x and y. QuitteOut can be used in ggplot with plotting x
and y. The unit needs to be replaced by None.

## Usage

``` r
prepQuitteForScatter(quitteIn, varNames)
```

## Arguments

- quitteIn:

  Quitte with original data

- varNames:

  Vector with two variable names that must be contained in
  QuitteIn\$variable

## Value

quitte object

## Author

Nico Bauer, Anselm Schultes, Jerome Hilaire

## Examples

``` r
  if (FALSE) { # \dontrun{
    quitteOut <- prepQuitteForScatter(quitteIn, c('Emissions|CO2', 'Price|Carbon'))
  } # }
```
