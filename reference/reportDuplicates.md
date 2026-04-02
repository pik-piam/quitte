# Checks for duplicates in quitte object

Finds duplicates in a quitte object and warns about them per model and
scenario. Also warn if variables are identical but value or units
differ.

## Usage

``` r
reportDuplicates(mifdata, action = "warn")
```

## Arguments

- mifdata:

  object that can be converted with as.quitte

- action:

  if set to 'warn', a warning with duplicate variables is raised

## Value

only the data that is duplicated. Has 0 rows if everything is fine
