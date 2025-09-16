#' Escape Control Characters
#'
#' Escapes control characters with back slashes, such that they are safe to
#' print.  Escaped characters are:
#' |    code | symbol  | description     |
#' |--------:|:-------:|:----------------|
#' | `\0x07` | `\a`    | bell            |
#' | `\0x08` | `\b`    | backspace       |
#' | `\0x09` | `\t`    | horizontal tab  |
#' | `\0x0a` | `\n`    | line feed       |
#' | `\0x0b` | `\v`    | vertical tab    |
#' | `\0x0c` | `\f`    | form feed       |
#' | `\0x0d` | `\r`    | carriage return |
#' | `\0x22` | `\"`    | double quote    |
#' | `\0x27` | `\'`    | single quote    |
#' | `\0x5c` | `\\`    | back slash      |
#'
#' @param x A character vector.
#' @param escape_quotes Should quotes be escaped?  One of `none`, `single`,
#'     `double`, or `both`.
#'
#' @returns A character vector with escaped control characters.
#'
#' @examples
#' escape_cntrl(c('foo\nbar\t\tbazz', 'foo\abar'))

#' @export
escape_cntrl <- Vectorize(
    function(x, escape_quotes = c('none', 'single', 'double', 'both'))
    {
        escape_quotes <- match.arg(escape_quotes)
        cntrl_chars <- list( `7` = '\\a',
                             `8` = '\\b',
                             `9` = '\\t',
                             `10` = '\\n',
                             `11` = '\\v',
                             `12` = '\\f',
                             `13` = '\\r',
                             if (escape_quotes %in% c('double', 'both')) {
                                 `34` = '\\"'
                             },
                             if (escape_quotes %in% c('single', 'both')) {
                                 `39` = '\\\''
                             },
                             `92` = '\\\\')

        # get groups of control (TRUE) and non-control characters
        x_rle <- x |>
            charToRaw() |>
            as.integer() |>
            `%in%`(as.integer(names(cntrl_chars))) |>
            rle()

        # turn lengths into cumulative sequences to get character positions
        x_rle[['lengths']] <- x_rle[['lengths']] |>
            Reduce(f = \(lhs, rhs) c(lhs, list(last(last(lhs)) + seq_len(rhs))),
                   init = list(list(0))) |>
            tail(n = -1)

        # split character vector to index individual characters
        x_split <- unlist(strsplit(x, ''))

        # assemble new character vector from groups
        lapply(seq_along(x_rle[['values']]),
               function(i)
               {
                   if (x_rle[['values']][[i]]) {   # control characters
                       # list cntrl_chars is easiest to index using characters
                       idx <- x_split[x_rle[['lengths']][[i]]] |>
                           Vectorize(charToRaw)() |>
                           as.integer() |>
                           as.character()

                       cntrl_chars[idx] |>
                           unlist()
                   } else {   # non-control characters
                       x_split[x_rle[['lengths']][[i]]]
                   }
               }) |>
            unlist() |>
            paste(collapse = '')
    },
    USE.NAMES = FALSE)
