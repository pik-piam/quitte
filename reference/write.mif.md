# Write .mif file

A wrapper around
[`readr::write_lines`](https://readr.tidyverse.org/reference/read_lines.html)
for writing files conforming to the [`.mif`
standard](https://gitlab.pik-potsdam.de/rse/rsewiki/-/wikis/Model-Intercomparison-File-Format-(mif)).

## Usage

``` r
write.mif(
  x,
  path,
  comment_header = NULL,
  comment = "#",
  append = FALSE,
  sep = ";",
  na = "NA"
)

write.IAMCcsv(
  x,
  path,
  comment_header = NULL,
  comment = "#",
  append = FALSE,
  sep = ",",
  na = ""
)
```

## Arguments

- x:

  A [`quitte`](quitte-package.md) data frame.

- path:

  Path or connection to write to.

- comment_header:

  Comment header to be written to the `.mif` file. Ignored if `append`
  is `TRUE`.

- comment:

  A character to prefix comment header lines with. Must match existing
  comment characters in file `path` if `append` is `TRUE`.

- append:

  Overwrite existing files (`FALSE`, default), or append to them
  (`TRUE`).

- sep:

  Single character used to separate fields within a record. Defaults to
  `;`.

- na:

  String used for `NA` elements. Defaults to `'NA'` for `write.mif()`
  and to `''` for `write.IAMCcsv()`.

## Details

`write.IAMCcsv()` uses commas as filed separators instead of
semi-colons.

## Author

Michaja Pehl

## Examples

``` r
write.mif(quitte_example_data, tempfile())
```
