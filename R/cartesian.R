#' Generate Cartesian product from character vectors
#'
#' @param ... objects that can be coerced to character vectors
#' @param sep a character string that will separate the elements of `...` in the
#'     output.  Defaults to `'.'`.
#'
#' @return A character vector of the concatenated elements of `...`.
#'
#' @examples
#' cartesian(c('a', 'b'), 1:3, c('X', 'Y', 'Z'))
#' #  [1] "a.1.X" "a.1.Y" "a.1.Z" "a.2.X" "a.2.Y" "a.2.Z" "a.3.X" "a.3.Y"
#' #  [9] "a.3.Z" "b.1.X" "b.1.Y" "b.1.Z" "b.2.X" "b.2.Y" "b.2.Z" "b.3.X"
#' # [17] "b.3.Y" "b.3.Z"
#'
#' @export
cartesian <- function(..., sep = '.') {
    if (0 == length(dots <- list(...)))
        return(NULL)

    v <- as.character(dots[[1]])
    for (i in dots[-1])
        v <- paste(rep(v, each = length(i)),
                   rep(as.character(i), times = length(v)),
                   sep = sep)

    return(v)
}
