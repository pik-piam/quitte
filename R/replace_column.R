#' Replace data frame column
#'
#' Replaces the column of a data frame with that from a mask data frame.
#'
#' Replaces the _old_ column in data frame `data` by the _new_ column from data
#' frame `mask` based on the matching between _old_ (`data`) and _match_
#' (`mask`) columns.
#'
#' This can be used to replace columns based on a mapping to, e.g., rename
#' scenarios, regions, etc. in model data.
#'
#' @md
#' @param data A data frame or a quitte object.
#' @param mask A data frame containing the `match_column` and the
#'     `new_column`.
#' @param ... Definition of _old_, _match_, and _new_ columns, see details.
#' @param drop.extra Drop rows not present in _match_ column?
#' @param ignore.ambiguous.match `replace_column()` will issue a warning if the
#'     _match_ column in `mask` does not map unambiguously to `data`, unless it
#'     is suppressed (`TRUE`).  Using ambiguous matches can be desired for
#'     duplicating specific rows.
#' @param old_column _old_ column name, see details.
#' @param match_column _match_ column name, see details.
#' @param new_column _new_ column name, see details.
#'
#' @return A data frame or a quitte object, same as `data`.
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
replace_column <- function(data, mask, ..., drop.extra = FALSE,
                           ignore.ambiguous.match = FALSE)
{

    .dots <- lazyeval::lazy_dots(...)

    old_column   <- names(.dots[1])
    match_column <- as.character(.dots[[1]]$expr)
    new_column   <- as.character(.dots[[2]]$expr)

    if ('' == old_column)
        old_column <- match_column

    replace_column_(data, mask, old_column, match_column, new_column,
                    drop.extra, ignore.ambiguous.match)
}

#' @export
#' @rdname replace_column
replace_column_ <- function(data, mask, old_column, match_column, new_column,
                            drop.extra = FALSE, ignore.ambiguous.match = FALSE)
{
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

    . <- NULL

    ambiguous <- mask %>%
        pull(match_column) %>%
        duplicated() %>%
        which() %>%
        `[`(mask, .,) %>%
        pull(match_column)

    if (!ignore.ambiguous.match && length(ambiguous))
    {
        warning('No unambiguous match for ',
                paste(paste0('\'', ambiguous, '\''), collapse = ', '),
                ' in column \'', match_column, '\'')
    }

    return(value)
}
