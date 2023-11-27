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
#' #  [1] "a.1.X" "a.1.Y" "a.1.Z" "a.2.X" "a.2.Y" "a.2.Z" "a.3.X" "a.3.Y"
#' #  [9] "a.3.Z" "b.1.X" "b.1.Y" "b.1.Z" "b.2.X" "b.2.Y" "b.2.Z" "b.3.X"
#' # [17] "b.3.Y" "b.3.Z"
#'
#' @export
cartesian <- function(..., sep = '.') {
    dots <- list(...)

    L <- lengths(dots)

    apply(
        X = sapply(
            X = seq_along(dots),
            FUN = function(i) {
                # lengths of dots elements before i (excluding)
                times_slice <- L[seq_len(i - 1)]
                # lengths of dot elements after i (excluding)
                each_slice  <- L[setdiff(seq_along(dots), seq_len(i))]

                rep(rep(dots[[i]], times = prod(times_slice)),
                    each = prod(each_slice))
            }
        ),
        MARGIN = 1,
        FUN = paste,
        collapse = sep
    )
}
