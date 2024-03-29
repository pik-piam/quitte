#' Changes factor columns of a data frame into character columns
#'
#' `character.data.frame()` turns factor columns of a data frame into
#' character columns.
#'
#' @param df a data frame
#' @param ... Column names to convert to characters.
#' @param .dots Character vector of column names to turn into characters.
#' @return A data frame.
#' @author Antoine Levesque
#' @examples
#' require(dplyr)
#' (df <- tibble(
#'     char = letters[1:3],
#'     fact = factor(LETTERS[24:26], levels = LETTERS[c(1:3, 24:26)]),
#'     num  = (1:3) ^ 2))
#'
#' character.data.frame(df)
#' character.data.frame_(df, 'num')
#'
#' @seealso [factor.data.frame()]
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom tibble as_tibble
#'
#' @export
character.data.frame <- function(df, ...) {
    .dots <- sapply(lazy_dots(...),
                    function(x) {
                        x %>%
                            getElement('expr') %>%
                            as.character()
                    })

    return(character.data.frame_(df, .dots))
}

#' @export
#' @rdname character.data.frame
character.data.frame_ <- function(df, .dots) {
    df = as_tibble(df)
    if (length(.dots)) {
        cols <- match(.dots, colnames(df))
    } else {
        cols <- sapply(df, is.factor)
    }

    df[,cols] <- as_tibble(lapply(df[,cols], as.character))

    return(df)
}
