# Read IAMC-style .csv or .xlsx files, or object from rds file.

Reads IAMC-style .csv or .xlsx files or object from rds file into a
quitte data frame.

## Usage

``` r
read.quitte(
  file,
  sep = NULL,
  quote = "",
  na.strings = c("UNDF", "NA", "N/A", "n_a"),
  convert.periods = FALSE,
  check.duplicates = TRUE,
  factors = TRUE,
  drop.na = FALSE,
  comment = "#",
  filter.function = identity,
  chunk_size = 200000L
)
```

## Arguments

- file:

  Path of IAMC-style .csv, .xlsx, or rds file or vector of paths to
  read.

- sep:

  Column separator, defaults to ";" in read_mif_header().

- quote:

  Quote characters, empty by default.

- na.strings:

  Entries to interpret as NA; defaults to
  `c("UNDF", "NA", "N/A", "n_a")`

- convert.periods:

  If `TRUE`, periods are converted to
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html). If `FALSE`
  (the default), periods are numerical.

- check.duplicates:

  If `TRUE` a duplicates check will be performed on the data. For time-
  and memory-critical applications this can be switched off.

- factors:

  Return columns as factors (`TRUE`, the default) or not.

- drop.na:

  Should `NA` values be dropped from the `quitte`?

- comment:

  A character which at line start signifies the optional comment header
  with metadata at the head of `file`. The comment header, if present,
  is returned as a `comment_header` attribute. If multiple files are
  read, the `comment_header` attribute is a list of comment headers with
  file paths as names.

- filter.function:

  A function used to filter data during read. See Details.

- chunk_size:

  Number of lines to read at a time. Defaults to 200000. (REMIND .mif
  files have between 55000 and 105000 lines for H12 and EU21 regional
  settings, respectively.)

## Value

A quitte data frame.

## Details

In order to process large data sets, like IIASA data base snapshots,
`read.quitte()` reads provided files in chunks of `chunk_size` lines
(not for Excel files), and applies `filter.function()` to the chunks.
This allows for filtering data piece-by-piece, without exceeding
available memory. `filter.function` is a function taking one argument, a
quitte data frame of the read chunk, and is expected to return a data
frame. Usually it should simply contain all the filters usually applied
after all the data is read in. Suppose there is a file
`big_IIASA_snapshot.csv`, from which only data for the REMIND and
MESSAGE models between the years 2020 to 2050 is of interest. Normally,
this data would be processed as

    read.quitte(file = 'big_IIASA_snapshot.csv') %>%
        filter(grepl('^(REMIND|MESSAGE)', .data$model),
               between(.data$period, 2020, 2060))

If however `big_IIASA_snapshot.csv` is too large to be read in
completely, it can be read using

    read.quitte(file = 'big_IIASA_snapshot.csv',
                filter.function = function(x) {
                    x %>%
                        filter(grepl('^(REMIND|MESSAGE)', .data$model),
                               between(.data$period, 2020, 2060))
                })

## Author

Michaja Pehl

## Examples

``` r
if (FALSE) { # \dontrun{
read.quitte(c("some/data/file.mif", "some/other/data/file.mif"))
read.quitte("some/data/file.csv", sep = ",", quote = '"')
} # }
```
