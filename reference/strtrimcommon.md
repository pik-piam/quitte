# Trim common portions from both sides of a vector of strings

Trim common portions from both sides of a vector of strings

## Usage

``` r
strtrimcommon(x, split = "", USE.NAMES = FALSE, return.all = FALSE)
```

## Arguments

- x:

  A vector of strings

- split:

  A [`character`](https://rdrr.io/r/base/character.html) to use for
  splitting. If `split` is empty (i.e. `split = ''`), `x` is split into
  single characters. Otherwise, `x` is split on `split` boundaries.

- USE.NAMES:

  logical; if `TRUE` use `x` as
  [`names`](https://rdrr.io/r/base/names.html) for the result.

- return.all:

  logical; if `FALSE` (the default), returns only the striped strings.
  If `TRUE`, returns a list with elements `left`, `strings`, `right`,
  containing left and right common portions, and the trimmed strings,
  respectively.

## Value

A (named) vector of strings, or a list of string vectors (see parameter
`return.all` for details).

## Author

Michaja Pehl

## Examples

``` r
x <- c('/tmp/remind2_test-convGDX2MIF_fulldata.gdx',
       '/tmp/remind2_test-Ariadne_fulldata.gdx',
       '/tmp/remind2_test-NAVIGATE_fulldata.gdx',
       '/tmp/remind2_test-NGFS_fulldata_oneRegi.gdx',
       '/tmp/remind2_test-SHAPE_fulldata.gdx')

strtrimcommon(x, USE.NAMES = TRUE)
#>  /tmp/remind2_test-convGDX2MIF_fulldata.gdx 
#>                      "convGDX2MIF_fulldata" 
#>      /tmp/remind2_test-Ariadne_fulldata.gdx 
#>                          "Ariadne_fulldata" 
#>     /tmp/remind2_test-NAVIGATE_fulldata.gdx 
#>                         "NAVIGATE_fulldata" 
#> /tmp/remind2_test-NGFS_fulldata_oneRegi.gdx 
#>                     "NGFS_fulldata_oneRegi" 
#>        /tmp/remind2_test-SHAPE_fulldata.gdx 
#>                            "SHAPE_fulldata" 

x <- c('Some|name|with|common|text|elements',
       'Some|name|without|extra|text|elements')

strtrimcommon(x, split = '|', return.all = TRUE)
#> $left
#> [1] "Some|name"
#> 
#> $strings
#> [1] "with|common"   "without|extra"
#> 
#> $right
#> [1] "text|elements"
#> 
```
