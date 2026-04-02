# Sequence from a Range

Generate regular sequence from a range. Wrapper function for
[`seq()`](https://rdrr.io/r/base/seq.html).

## Usage

``` r
seq_range(range, by = NA, length.out = NULL)
```

## Arguments

- range:

  Vector with starting and end values of the sequence. Only first two
  elements are considered.

- by:

  Number; increment of the sequence.

- length.out:

  Desired length of the sequence. A non-negative number, which will be
  rounded up if fractional.

## Value

Returns a vector of type "integer" or "double": programmers should not
rely on which.

## See also

[`seq()`](https://rdrr.io/r/base/seq.html),
[`range()`](https://rdrr.io/r/base/range.html)

## Author

Michaja Pehl

## Examples

``` r
seq_range(range(1:13), by = 3)
#> [1]  1  4  7 10 13
```
