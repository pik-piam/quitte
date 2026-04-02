# Remove quitte class atribute

Remove quitte class atribute

## Usage

``` r
unquitte(x)
```

## Arguments

- x:

  Any object.

## Value

`x`, with the `quitte` class attribute removed (if any).

## Examples

``` r
quitte_example_data |> class()
#> [1] "quitte"     "tbl_df"     "tbl"        "data.frame"
quitte_example_data |> unquitte() |> class()
#> [1] "tbl_df"     "tbl"        "data.frame"
mtcars |> class()
#> [1] "data.frame"
mtcars |> unquitte() |> class()
#> [1] "data.frame"
```
