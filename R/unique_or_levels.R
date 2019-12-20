
#' Unique or levels
#'
#' Abstract the differences between character vectors and factors.
#'
#' @param x A character vector or a factor.
#'
#' @return A character vector with the unique elements of \code{x} if it is a
#'     character vector, or the levels of \code{x} if it is a factor.
#'
#' @author Michaja Pehl

#' @export
unique_or_levels <- function(x) {
    if (is.factor(x)) {
        return(levels(x))
    } else {
        return(unique(x))
    }
}
