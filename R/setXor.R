#' Set Xor
#'
#' Performs \eqn{(x \cup y) \setminus (x \cap y)} on
#' parameters, returning all elements that are in either x or y, but not both.
#'
#' @param x,y Objects to perform set function on.
#'
#' @examples
#' x <- c('a', 'b', 'c')
#' y <- c('b', 'c', 'd')
#' setXor(x, y)
#'
#' @author Michaja Pehl

#' @export
setXor <- function(x, y) {
    setdiff(union(x, y), intersect(x, y))
}
