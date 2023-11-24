#' Generate cartesian product from to character vectors
#'
#' @param ... objects that can be coerced to character vectors
#' @param sep a character string that will separate the elements of `...` in the
#'     output.  Defaults to `'.'`.
#'
#' @return A character vector of the concatenated elements of `...`.
#'
#' @examples
#' cartesian(c('a', 'b'), 1:3, c('X', 'Y', 'Z'))
#' #  [1] "a.1.X" "b.2.Y" "a.3.Z" "b.1.X" "a.2.Y" "b.3.Z" "a.1.X" "b.2.Y"
#' #  [9] "a.3.Z" "b.1.X" "a.2.Y" "b.3.Z" "a.1.X" "b.2.Y" "a.3.Z" "b.1.X"
#' # [17] "a.2.Y" "b.3.Z"
#'
#' @export
cartesian <- function(..., sep = '.') {
    dots <- list(...)

    reps <- prod(lengths(dots)) / lengths(dots)

    apply(
        X = sapply(
            X = seq_along(dots),
            FUN = function(i) {
                rep(dots[[i]], times = reps[[i]])
            }
        ),
        MARGIN = 1,
        FUN = paste,
        collapse = '.'
    )
}
