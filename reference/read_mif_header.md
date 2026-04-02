# Read .mif Header

Read .mif Header

## Usage

``` r
read_mif_header(file, sep = ";", comment = "#")
```

## Arguments

- file:

  A path to a `.mif` file.

- sep:

  Column separator, defaults to ";".

- comment:

  A character which at line start signifies the optional comment header
  with metadata at the head of `file`, defaults to "#".

## Value

A `list` with elements `header`, `comment_header`, and
`useless.last.column`.

## Author

Michaja Pehl
