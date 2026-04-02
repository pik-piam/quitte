# Write .xlsx file

Write a `.xlsx` file in line with IAMC standard.

## Usage

``` r
write.IAMCxlsx(x, path, append = FALSE)
```

## Arguments

- x:

  A [`quitte`](quitte-package.md) data frame.

- path:

  Path or connection to write to.

- append:

  Overwrite existing files (`FALSE`, default), or append to them
  (`TRUE`).

## Author

Michaja Pehl, Oliver Richters

## Examples

``` r
write.IAMCxlsx(quitte_example_data, tempfile())
```
