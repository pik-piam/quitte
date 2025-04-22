#' Does a data frame match the quitte definition?
#'
#' Checks if `df` fulfills all criteria of a "quitte data frame", namely:
#' - Must be of class "data.frame".
#' - Must have the mandatory columns "model", "scenario", "region", "variable",
#'   "unit", "period", and "value".
#' - The "period" column must be of class "integer" or "POSIXct".
#' - The "value" column must be of type "numeric".
#'
#' @param df An object to test for `quitte` structure.
#' @param verbose Any function like [`message()`][base::message],
#'     [`warning()`][base::warning], or [`stop()`][base::stop] to report if any
#'     of the criteria of the quitte definition are not fulfilled.
#'
#' @returns `TRUE` if all criteria are met, `FALSE` otherwise.
#'
#' @examples
#' library(dplyr)
#'
#' quitte_example_data %>%
#'     looks_like_quitte()
#'
#' quitte_example_data %>%
#'     select(-'model') %>%
#'     mutate(period = as.character(period)) %>%
#'     looks_like_quitte(verbose = message)
#'
#' @importFrom cli pluralize

#' @export
looks_like_quitte <- function(df, verbose = NULL)
{
    if (is.null(verbose)) {
        verbose <- \(...) invisible()
    }

    if (!is.data.frame(df)) {
        verbose('`df` must be a data frame.')
        return(FALSE)
    }

    looks_like_quitte <- TRUE

    if (length(missing_columns <- setdiff(c('model', 'scenario', 'region',
                                            'variable', 'unit', 'period',
                                            'value'), colnames(df)))) {
        missing_columns <- paste0('`', missing_columns, '`')
        verbose(pluralize('Mandatory columns{?s} {missing_columns} missing ',
                          'from `df`.'))
        looks_like_quitte <- FALSE
    }

    if ('period' %in% colnames(df)) {
        class_period <- class(df[['period']])
        if (!any(class_period %in% c('integer', 'POSIXct'))) {
            class_period <- paste0('`', class_period, '`')
            verbose(pluralize(
                'Column `period` must be of class "integer" or "POSIXct", but ',
                'has class{?es} {class_period}.'))
            looks_like_quitte <- FALSE
        }
    }

    if ('value' %in% colnames(df)) {
        if (!is.numeric(df[['value']])) {
            verbose('Column `value` must be of type "numeric".')
            looks_like_quitte <- FALSE
        }
    }

    return(looks_like_quitte)
}
