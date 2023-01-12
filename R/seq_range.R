
#' Sequence from a Range
#'
#' Generate regular sequence from a range. Wrapper function for
#' [seq()].
#'
#' @param range Vector with starting and end values of the sequence. Only first
#'        two elements are considered.
#' @param by Number; increment of the sequence.
#' @param length.out Desired length of the sequence. A non-negative number,
#'        which will be rounded up if fractional.
#'
#' @return Returns a vector of type "integer" or "double": programmers should
#'         not rely on which.
#'
#' @author Michaja Pehl
#'
#' @seealso [seq()], [range()]
#'
#' @examples
#' seq_range(range(1:13), by = 3)

#' @export
seq_range <- function(range, by = NA, length.out = NULL) {
    if (is.na(by)) {
        if (is.null(length.out)) {
            seq(from = range[1], to = range[2])
        } else {
            seq(from = range[1], to = range[2], length.out = length.out)
        }
    } else {
        if (is.null(length.out)) {
            seq(from = range[1], to = range[2], by = by)
        } else {
            stop("too many arguments")
        }
    }
}
