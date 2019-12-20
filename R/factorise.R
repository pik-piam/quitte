#' Factorise
#'
#' Generate a factor with levels in prescribed order.
#'
#' @param x A character vector.
#'
#' @return A factor from \code{x}, with levels in the same order as they appear
#'     in within \code{x}.
#'
#' @author Michaja Pehl
#'
#' @examples
#' factor(c('a', 'c', 'b'))
#' factorise(c('a', 'c', 'b'))

#' @export
factorise <- function(x) {
    return(factor(x, levels = unique(x)))
}
