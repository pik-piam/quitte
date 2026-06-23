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
#' @param verbose Either `NULL`, or one of `'abort'`, `'warn'`, or `'inform`',
#'     to report if any of the criteria of the quitte definition are not
#'     fulfilled using `[cli::cli_abort]`, `[cli::cli_warn]`, or
#'     `[cli::cli_inform]`.
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
#'     looks_like_quitte(verbose = 'inform')
#'
#' @importFrom cli cli_abort cli_warn cli_inform

#' @export
looks_like_quitte <- function(df, verbose = NULL)
{
    if (is.null(verbose)) {
        verbose <- \(...) {}
    } else if (isTRUE(verbose == 'abort')) {
        verbose <- cli_abort
    } else if (isTRUE(verbose == 'warn')) {
        verbose <- cli_warn
    } else if (isTRUE(verbose == 'inform')) {
        verbose <- cli_inform
    } else {
        cli_abort(c(
             paste('{.arg verbose} must be one of {.val abort}, {.val warn},',
                   '{.val inform}, or {.code NULL}.'),
            i = 'It is {.val {verbose}}.'))
    }

    msg <- character(0)

    if (!is.data.frame(df)) {
        msg <- c(msg,
                 '{.arg df} must be a data frame, not {.obj_type_friendly df}.')
        return(FALSE)
    }

    looks_like_quitte <- TRUE

    mandatory_columns <- c('model', 'scenario', 'region', 'variable', 'unit',
                           'period', 'value')
    is_missing <- !(mandatory_columns %in% colnames(df))
    if (any(is_missing)) {
        msg <- c(msg,
                 '{.arg df} must have all mandatory columns.',
                 i = '{.val {mandatory_columns[is_missing]}} {?is/are} missing.'
        )
        looks_like_quitte <- FALSE
    }

    cp <- class(df[['period']])
    if ('period' %in% colnames(df) && !cp %in% c('integer', 'POSIXct')) {
        msg <- c(msg,
                 paste('Column {.field period} must be of class',
                       '{.cls integer} or {.cls POSIXct}.'),
                 i = 'It is of class {.cls {cp}}.')
        looks_like_quitte <- FALSE
    }

    if ('value' %in% colnames(df) && !is.numeric(df[['value']])) {
        msg <- c(msg,
                 'Column {.field value} must be of class {.cls numeric}.',
                 i = 'It is of class {.cls {class(df[["value"]])}}.')
        looks_like_quitte <- FALSE
    }

    if (!looks_like_quitte) {
        verbose(msg)
    }
    return(looks_like_quitte)
}
