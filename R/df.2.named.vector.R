
#' Data frame to named vector
#'
#' Turns the two first columns of a data frame into a named vector, taking the
#' values from the second and the names from the first column.
#'
#' @param .data A data frame with at least two columns.
#'
#' @return A named vector.
#'
#' @author Michaja Pehl
#'
#' @export
#'
#' @examples
#' data <- data.frame(names = c("one", "two", "three"), values = 1:3)
#' data
#' df.2.named.vector(data)

df.2.named.vector <- function(.data) {
    x <- as.character(getElement(.data, 2))
    names(x) <- as.character(getElement(.data, 1))

    return(x)
}
