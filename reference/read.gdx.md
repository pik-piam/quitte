# Read item from `.gdx` file as quitte data frame

`read.gdx()` is a wrapper function for
[`gamstransfer::readGDX()`](https://rdrr.io/pkg/gamstransfer/man/readGDX.html)
that returns a quitte data frame.

## Usage

``` r
read.gdx(
  gdxName,
  requestList.name,
  fields = "l",
  colNames = NULL,
  squeeze = TRUE
)
```

## Arguments

- gdxName:

  Path to a `.gdx` file.

- requestList.name:

  Name of the item to read.

- fields:

  Fields to read from variables and equations. Any of `l`, `m`, `lo`,
  `up`, `s` (or the long forms `level`, `marginal`, `lower`, `upper`,
  and `scale`). `all` will return all fields. Ignored when reading sets
  or parameters.

- colNames:

  String vector of column names to override dimension and field names.

- squeeze:

  If `TRUE` (the default), drop records whose value/level is a stored
  zero or EPS. Set to `FALSE` to return every stored value, including
  zeros and EPS.

## Value

A quitte data frame.

## Author

Michaja Pehl
