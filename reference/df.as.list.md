# Data Frame as List

Data Frame as List

## Usage

``` r
df.as.list(df, names = 1, x = 2)
```

## Arguments

- df:

  A data frame.

- names:

  Index used for naming list items. Integer or character, must work with
  `df[[names]]`. Defaults to the first data frame column.

- x:

  Index used for list items. Integer or character, must work with
  `df[[x]]`. Defaults to the second data frame column.

## Value

A list.

## Examples

``` r
(df <- data.frame(
    modules = c('power', 'macro', 'welfare', 'PE_FE_parameters',
                'initialCap', 'aerosols'),
    `*` = c('IntC', 'singleSectorGr', 'utilitarian', 'iea2014', 'on',
            'exoGAINS'),
    check.names = FALSE
))
#>            modules              *
#> 1            power           IntC
#> 2            macro singleSectorGr
#> 3          welfare    utilitarian
#> 4 PE_FE_parameters        iea2014
#> 5       initialCap             on
#> 6         aerosols       exoGAINS

df.as.list(df, 'modules', '*')
#> $power
#> [1] "IntC"
#> 
#> $macro
#> [1] "singleSectorGr"
#> 
#> $welfare
#> [1] "utilitarian"
#> 
#> $PE_FE_parameters
#> [1] "iea2014"
#> 
#> $initialCap
#> [1] "on"
#> 
#> $aerosols
#> [1] "exoGAINS"
#> 
```
