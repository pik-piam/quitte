# Round fractions only

Round numbers to significant digits, preserving integer parts.

## Usage

``` r
signif_frac(x, digits = 3)
```

## Arguments

- x:

  A numeric vector.

- digits:

  Number of significant digits, defaults to 3.

## Value

A numeric vector.

## Examples

``` r
set.seed(0)
x <- runif(8, -1, 1) * 10 ^ seq.int(5, -2)

data.frame(number           = x,
           formatted        = sprintf('%g', x),
           `w/ signif`      = sprintf('%g', signif(x, 3)),
           `w/ signif_frac` = sprintf('%g', signif_frac(x)),
           check.names = FALSE)
#>          number  formatted w/ signif w/ signif_frac
#> 1  7.933944e+04    79339.4     79300          79339
#> 2 -4.689827e+03   -4689.83     -4690          -4690
#> 3 -2.557522e+02   -255.752      -256           -256
#> 4  1.457067e+01    14.5707      14.6           14.6
#> 5  8.164156e+00    8.16416      8.16           8.16
#> 6 -5.966361e-01  -0.596636    -0.597         -0.597
#> 7  7.967794e-02  0.0796779    0.0797         0.0797
#> 8  8.893505e-03 0.00889351   0.00889        0.00889
```
