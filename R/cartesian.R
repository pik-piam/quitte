#' Generate Cartesian Product from Vectors
#'
#' @param ... Vectors to be combined
#' @param sep If a character string, that string will separate the elements of
#'     `...` (if necessary cast to `character`) in the output.  If `NULL`,
#'     elements of `...` will not be cast.  Defaults to `'.'`.
#'
#' @return If `sep` is a character string, then a character vector of
#'     concatenated elements.  If `sep` is `NULL`, then a list of concatenated
#'     elements.
#'
#' @examples
#' cartesian(c('a', 'b'), 1:3, c('X', 'Y', 'Z'))
#' #  [1] "a.1.X" "a.1.Y" "a.1.Z" "a.2.X" "a.2.Y" "a.2.Z" "a.3.X" "a.3.Y"
#' #  [9] "a.3.Z" "b.1.X" "b.1.Y" "b.1.Z" "b.2.X" "b.2.Y" "b.2.Z" "b.3.X"
#' # [17] "b.3.Y" "b.3.Z"
#'
#' str(cartesian(c('a', 'b'), 17:19, sep = NULL))
#' # List of 6
#' #  $ :List of 2
#' #   ..$ : chr "a"
#' #   ..$ : int 17
#' #  $ :List of 2
#' #   ..$ : chr "a"
#' #   ..$ : int 18
#' #  $ :List of 2
#' #   ..$ : chr "a"
#' #   ..$ : int 19
#' #  $ :List of 2
#' #   ..$ : chr "b"
#' #   ..$ : int 17
#' #  $ :List of 2
#' #   ..$ : chr "b"
#' #   ..$ : int 18
#' #  $ :List of 2
#' #   ..$ : chr "b"
#' #   ..$ : int 19

#' @export
cartesian <- function(..., sep = '.')
{
    stopifnot('`sep` must be a character string or NULL'
              = is.null(sep) || is.character(sep))

    if (0 == length(dots <- list(...)))
        return(NULL)

    dots <- lapply(dots, as.list)

    v <- lapply(dots[[1]], as.list)
    for (i in dots[-1])
        v <- mapply(c, rep(v, each = length(i)), rep(i, times = length(v)),
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)

    if (is.character(sep))
        v <- vapply(v, paste, collapse = sep, FUN.VALUE = character(1))

    return(v)
}
