#' Convert a vector of stings to a data frame
#'
#' \code{inline.data.frame()} converts a vector of strings that contain
#' separated items into a data frame.
#'
#' @param ...   string, or a vector of strings
#' @param sep   Item separator within strings, defaults to ";"
#' @param quote Quote character for masking separators, empty by default
#'
#' @return a data frame
#'
#' @author Michaja Pehl
#'
#' @examples
#' inline.data.frame(
#'     "letters; numbers",
#'     "A;       1",
#'     "B;       2",
#'     NULL)   # this last line allows for easy switching of line order
#' inline.data.frame(c("letters; numbers", "A; 1", "B; 2"))
#'
#' @import dplyr
#' @import utils
#'
#' @export
inline.data.frame <- function(..., sep = ";", quote = "") {
    .dots <- list(...)

    # .dots is one long character vector?
    if (1 == length(.dots) & "character" == typeof(.dots[[1]])) {
        table <- .dots[[1]] %>%
            paste(sep = "\n")

    # .dots is a list of character vectors or NULL?
    } else if (all(unlist(lapply(.dots, typeof)) %in% c("character", "NULL"))) {
        table <- .dots %>%
            unlist() %>%
            paste0(collapse = "\n")

    # .dots is something else ...
    } else {
        stop("Can't handle parameter. Need string or list of strings.")
    }

    table %>%
        textConnection() %>%
        read.table(header = TRUE, sep = sep, quote = quote, comment.char = "",
                   strip.white = TRUE, stringsAsFactors = FALSE,
                   check.names = FALSE) %>%
        tbl_df()
}
