# Read item from `.gdx` file as quitte data frame

`read.gdx()` is a wrapper function for either
[`gdxrrw::rgdx()`](https://rdrr.io/pkg/gdxrrw/man/rgdx.html) or
[`gamstransfer::readGDX()`](https://rdrr.io/pkg/gamstransfer/man/readGDX.html)
that returns a quitte data frame.

## Usage

``` r
read.gdx(
  gdxName,
  requestList.name,
  fields = "l",
  colNames = NULL,
  factors = deprecated(),
  squeeze = TRUE
)
```

## Arguments

- gdxName:

  Path to a `.gdx` file.

- requestList.name:

  Name of the item to read.

- fields:

  Fields to read from variables and equations. When using
  [gdxrrw](gdxrrw-package), any of `l`, `m`, `lo`, `up`, `s`. When using
  using [gamstransfer](gamstransfer-package), `level`, `marginal`,
  `lower`, `upper`, and `scale` are understood as well. `all` will
  return all fields. Ignored when reading sets or parameters.

- colNames:

  String vector of column names to override dimension and field names.

- factors:

  Deprecated. Do not use any more.

- squeeze:

  If `TRUE`, squeeze out any zero or EPS stored in the GDX container.
  Ignored when using [gamstransfer](gamstransfer-package).

## Value

A quitte data frame.

## Details

`read.gdx()` will use
[`gdxrrw::rgdx()`](https://rdrr.io/pkg/gdxrrw/man/rgdx.html) if
[gdxrrw](gdxrrw-package) is installed and the option
`quitte_force_gamstransfer` is not `TRUE`, otherwise it will use
[`gamstransfer::readGDX()`](https://rdrr.io/pkg/gamstransfer/man/readGDX.html).

## Author

Michaja Pehl
