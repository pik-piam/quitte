# Class "quitte" \~\~~

The quitte class is a more standardized data.frame format. `is.quitte`
tests if `x` is an quitte-object, `as.quitte` transforms `x` to an
quitte-object (if possible).

## Usage

``` r
as.quitte(x, periodClass = "integer", addNA = FALSE, na.rm = FALSE)

is.quitte(x, warn = TRUE)
```

## Arguments

- x:

  An object that should be either tested or transformed as/to an
  quitte-object.

- periodClass:

  integer or POSIXct

- addNA:

  modifies a factor by turning NA into an extra level (so that NA values
  are counted in tables, for instance).

- na.rm:

  if set to TRUE entries with value NA will be removed

- warn:

  display warnings or not

## Functions

- `is.quitte()`: quitte check

## Author

Jan Philipp Dietrich
