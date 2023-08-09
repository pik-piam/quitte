#' (Re-) Factorise factor and character columns in data frame
#'
#' `factor.data.frame()` turns character columns in a data frame into
#' factor columns and refactorises factor columns, silently dropping unused
#' levels.
#'
#' @param df A data frame.
#' @param ... Column names to factorise.
#' @param .dots Character vector of column names to factorise.
#' @return A data frame.
#' @author Michaja Pehl
#' @examples
#' require(dplyr)
#' (df <- tibble(
#'     char = letters[1:3],
#'     fact = factor(LETTERS[24:26], levels = LETTERS[c(1:3, 24:26)]),
#'     num  = (1:3) ^ 2))
#'
#' str(factor.data.frame(df))
#' str(factor.data.frame_(df, 'num'))
#'
#' @seealso [character.data.frame()]
#'
#' @importFrom lazyeval lazy_dots
#'
#' @export
factor.data.frame <- function(df, ...) {
    .dots <- sapply(
        lazy_dots(...), USE.NAMES = FALSE,

        FUN = function(x) {
            x %>%
                getElement('expr') %>%
                as.character()
        })

    return(factor.data.frame_(df, .dots))
}

#' @export
#' @rdname factor.data.frame
factor.data.frame_ <- function(df, .dots) {
    if (length(.dots)) {
        cols <- match(.dots, colnames(df))
    } else {
        cols <- sapply(df, is.factor) | sapply(df, is.character)
    }

    df[,cols] <- lapply(df[,cols], factor)

    return(df)
}
