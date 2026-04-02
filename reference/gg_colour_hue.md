# Get n Colours

Get n colours, evenly spaced along the colour wheel. Just like the ones
[`ggplot2::scale_colour_hue()`](https://ggplot2.tidyverse.org/reference/scale_hue.html)
is using.

## Usage

``` r
gg_colour_hue(n)
```

## Arguments

- n:

  Either the number of colours to generate, or a character vector which
  will be used as names for the returned colours.

## Value

A vector of character strings which can be used as color specifications
by R graphics functions. The vector is named if n is a character vector.

## Examples

``` r
gg_colour_hue(5)
#> [1] "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"
gg_colour_hue(letters[1:3])
#>         a         b         c 
#> "#F8766D" "#00BA38" "#619CFF" 
```
