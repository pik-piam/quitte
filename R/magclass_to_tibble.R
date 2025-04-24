#' Make a Tibble of a Magclass
#'
#' Sensible `magclass` to `tibble` conversion.
#'
#' @param m A [`magpie`][magclass::magclass] object.
#' @param colnames Column names for the returned `tibble`.  Must match the
#'     number of columns.
#'
#' @return A [`tibble`][tibble::tibble].
#'
#' @importFrom dplyr mutate select
#' @importFrom magclass as.data.frame getItems
#' @importFrom rlang sym syms
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of
#'
#' @examples
#' magclass_to_tibble(magclass::maxample('pop'))

#' @export
magclass_to_tibble <- function(m, colnames = NULL) {
    if (!inherits(m, 'magpie')) {
        stop('m is not a magclass')
    }

    n <- m %>%
        as.data.frame() %>%
        as_tibble()

    na_columns <- n %>%
        as.list() %>%
        sapply(function(x) { all(is.na(x)) }) %>%
        which(useNames = TRUE) %>%
        names()

    rename_columns <- n %>%
        colnames() %>%
        setdiff(c(na_columns, 'Region', 'Year', 'Value'))

    col_names <- c(unlist(strsplit(names(getItems(m)), '\\.')), 'value')

    if (length(rename_columns) > length(col_names) - 3) {
        col_names <- c(
            col_names[1:2],
            paste(col_names[c(-1, -2, -length(col_names))],
                  seq_along(rename_columns),
                  sep = '.'),
            col_names[length(col_names)])
    }

    n <- n %>%
        select(!all_of(na_columns)) %>%
        `colnames<-`(col_names) %>%
        character.data.frame() %>%
        mutate(!!sym(col_names[2]) := as.integer(!!sym(col_names[2])))

    if (!is.null(colnames)) {
        n <- n %>%
            `colnames<-`(colnames)
    }

    return(n)
}
