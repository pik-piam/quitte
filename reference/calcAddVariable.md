# Calculate new variable

Calculate new variable from existing ones, using a generic formula.

## Usage

``` r
calcAddVariable(data, formula, newUnit = "None", na.act = "no")
```

## Arguments

- data:

  A data frame with columns `"variable"`, `"unit"` and `"value"`.

- formula:

  An object of class formula, as returned by
  [`stats::formula()`](https://rdrr.io/r/stats/formula.html).

- newUnit:

  Character vector with the unit for the newly calculated variable.

- na.act:

  Indicates how NAs in the wide data frame should be handled. Default
  "no" indicates no action is takenl.

## Value

A data frame with the original and the new variables.

## Details

**Obsolete**. This function will be removed in the near future. Use
[`calc_addVariable()`](calc_addVariable.md) instead.

## See also

[`calc_addVariable()`](calc_addVariable.md)

## Author

Anselm Schultes, Michaja Pehl

## Examples

``` r
```
