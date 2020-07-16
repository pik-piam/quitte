#' Replace data frame column
#'
#' Replaces the column of a data frame with that from a mask data frame.
#'
#' Replaces the \emph{old} column in data frame \code{data} by the \emph{new}
#' column from data frame \code{mask} based on the matching between \emph{old}
#' (\code{data}) and \emph{match} (\code{mask}) columns.
#'
#' This can be used to replace columns based on a mapping to, e.g., rename
#' scenarios, regions, etc. in model data.
#'
#' @param data A data frame or a quitte object.
#' @param mask A data frame containing the \code{match_column} and the
#'             \code{new_column}.
#' @param ... Definition of \emph{old}, \emph{match}, and \emph{new} columns.
#'            See details.
#' @param drop.extra Drop rows not present in \emph{match} column?
#' @param old_column \emph{old} column name, see details.
#' @param match_column \emph{match} column name, see details.
#' @param new_column \emph{new} column name, see details.
#'
#' @return A data frame or a quitte object, same as \code{data}.
#'
#' @examples
#' # simple example with matching old and match column names
#' (model_data <- data.frame(
#'     model  = c('Model1', '2ndModel', 'Model Three'),
#'     region = c('Region 1', 'Region 2', 'Region 1'),
#'     value  = 1:3))
#'
#' (mask <- data.frame(
#'     model  = c('Model1', '2ndModel', 'Model Three', 'fourth Model'),
#'     clear_name = paste('Model', 1:4)))
#'
#' replace_column(model_data, mask, model, clear_name)
#'
#' # mismatched column names
#' (model_data <- data.frame(
#'     model  = c('Model1', '2ndModel', 'Model Three', 'fourth Model'),
#'     region = c('Region 1', 'Region 2', 'Region 1', 'Region 2'),
#'     value  = 1:4))
#'
#' (mask <- data.frame(
#'     ugly_name  = c('Model1', '2ndModel', 'Model Three'),
#'     clear_name = paste('Model', 1:3)))
#'
#' replace_column(model_data, mask, model = ugly_name, clear_name)
#'
#' # SE example
#' replace_column_(model_data, mask, 'model', 'ugly_name', 'clear_name')
#'
#' # dropping the extra entries in model
#' replace_column(model_data, mask, model = ugly_name, clear_name,
#'                drop.extra = TRUE)
#'
#' # also works on quitte objects
#' require(dplyr)
#' (quitte <- tibble(
#'     model    = c('Model1', '2ndModel'),
#'     scenario = 'Scenario',
#'     region   = 'Region',
#'     variable = 'Variable',
#'     unit     = 'Unit',
#'     period   = 2010,
#'     value    = 1:2) %>%
#'         as.quitte())
#' replace_column(quitte, mask, model = ugly_name, clear_name)
#' str(.Last.value)
#'
#' @author Michaja Pehl

#' @export
replace_column <- function(data, mask, ..., drop.extra = FALSE) {

    .dots <- lazyeval::lazy_dots(...)

    old_column   <- names(.dots[1])
    match_column <- as.character(.dots[[1]]$expr)
    new_column   <- as.character(.dots[[2]]$expr)

    if ('' == old_column)
        old_column <- match_column

    replace_column_(data, mask, old_column, match_column, new_column,
                    drop.extra)
}

#' @export
#' @rdname replace_column
replace_column_ <- function(data, mask, old_column, match_column, new_column,
                            drop.extra = FALSE) {
    value <- suppressWarnings(
        left_join(data, mask, stats::setNames(match_column, old_column))
    ) %>%
        select(!!!syms(c(setdiff(colnames(data), old_column), new_column))) %>%
        rename(!!sym(old_column) := !!sym(new_column)) %>%
        select(!!!syms(colnames(data))) %>% # restore original order
        droplevels()

    if (drop.extra)
        value <- value %>%
            filter(!is.na(!!sym(old_column)))

    if (is.quitte(data))
        value <- as.quitte(value)

    return(value)
}
