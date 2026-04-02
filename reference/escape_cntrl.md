# Escape Control Characters

Escapes control characters with back slashes, such that they are safe to
print. Escaped characters are:

|         |        |                 |
|---------|--------|-----------------|
| code    | symbol | description     |
| `\0x07` | `\a`   | bell            |
| `\0x08` | `\b`   | backspace       |
| `\0x09` | `\t`   | horizontal tab  |
| `\0x0a` | `\n`   | line feed       |
| `\0x0b` | `\v`   | vertical tab    |
| `\0x0c` | `\f`   | form feed       |
| `\0x0d` | `\r`   | carriage return |
| `\0x22` | `\"`   | double quote    |
| `\0x27` | `\'`   | single quote    |
| `\0x5c` | `\\`   | back slash      |

## Usage

``` r
escape_cntrl(x, escape_quotes = c("none", "single", "double", "both"))
```

## Arguments

- x:

  A character vector.

- escape_quotes:

  Should quotes be escaped? One of `none`, `single`, `double`, or
  `both`.

## Value

A character vector with escaped control characters.

## Examples

``` r
escape_cntrl(c('foo\nbar\t\tbazz', 'foo\abar'))
#> [1] "foo\\nbar\\t\\tbazz" "foo\\abar"          
```
