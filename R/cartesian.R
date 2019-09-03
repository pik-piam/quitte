#' Generate cartesian product from to character vectors
#'
#' @param x,y objects that can be coerced to character vectors
#' @param sep a character string that will seperate the elements of \code{x}
#'    and \code{y} in the output. Defaults to \code{'.'}.
#'
#' @return A character vector of the concatenated elements of \code{x} and
#'    \code{y}.
#'
#' @examples
#' cartesian(c('a', 'b'), 1:3)
#' # [1] "a.1" "a.2" "a.3" "b.1" "b.2" "b.3"
#'
#' @export
cartesian <- function(x, y, sep = '.') {
    x <- as.character(x)
    y <- as.character(y)
    paste(rep(x, each = length(y)), rep(y, times = length(x)), sep = sep)
}
