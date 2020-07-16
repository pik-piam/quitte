
#' Sign Range
#'
#' Returns the range of signs in a numerical vector as a character string.
#'
#' @param x A numerical vector.
#' @param na.rm Should \code{NA}s be ignored?
#'
#' @return A character string of signs found in \code{x}.
#'
#' @author Michaja Pehl
#'
#' @examples
#' signrange(-1)
#' signrange(0)
#' signrange(1)
#' signrange(c(-1, 0))
#' signrange(c(0, 1))
#' signrange(c(-1, 1))

#' @export
signrange <- function(x, na.rm = TRUE) {
    # getting rid of useless note
    . <- . <- NULL
    rm(.)

    x %>%
        range(na.rm = na.rm) %>%
        sign(x = .) %>%
        unique() %>%
        rev() %>%
        as.character() %>%
        sub('-1', '-', x = .) %>%
        sub('1', '+', x = .) %>%
        sub('0', '', x = .) %>%
        paste(collapse = '/') %>%
        sub('(^/|/$)', '', x = .)
}
