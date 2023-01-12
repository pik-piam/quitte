#' Order data frame factor levels.
#'
#' Arranges the levels of data frame columns in a given order. Non-factor
#' columns are silently converted.
#'
#' @param df A data frame (or quitte object).
#' @param ... Name-value pairs assigning level order to factor columns.
#' @param dots A named list of factor columns and corresponding levels.
#' @param drop.extra.levels If `TRUE` (default) levels not present in the
#'                          factor are silently dropped.
#' @return A data frame (or quitte object, same as `data`).
#' @author Michaja Pehl
#' @examples
#' require(dplyr)
#' str(df <- tibble(UPPER = LETTERS[3:1], lower = factor(letters[24:26]),
#'                      value = 1:3))
#' str(order.levels(df, UPPER = LETTERS[1:3], lower = letters[26:20]))
#' str(order.levels_(df, list(UPPER = LETTERS[1:3], lower = letters[26:23]),
#'                   drop.extra.levels = FALSE))
#'
#' @export
order.levels <- function(df, ..., drop.extra.levels = TRUE) {

    dots <- list(...)

    order.levels_(df, dots, drop.extra.levels)
}

#' @export
#' @rdname order.levels
order.levels_ <- function(df, dots, drop.extra.levels = TRUE) {

    # guardians
    if (!is.data.frame(df))
        stop('only works on data frames')

    dot.names <- names(dots)

    for (column in dot.names) {
        if (is.factor(getElement(df, column))) {
            have.levels <- levels(getElement(df, column))
        } else {
            have.levels <- unique(getElement(df, column))
        }

        want.levels <- getElement(dots, column)

        if (drop.extra.levels)
            want.levels <- intersect(want.levels, have.levels)

        df[, column] <- factor(getElement(df, column), levels = want.levels)
    }

    return(df)
}
