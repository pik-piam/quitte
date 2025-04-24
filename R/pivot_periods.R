#' Pivot Data Frame on Periods
#'
#' Pivot data frame on periods either to wide or long format.
#'
#' @param df A data frame.
#' @param direction Direction in which to pivot.  Either `'wider'` or
#'      `'longer'`.  Determined automatically for the default `NULL`.
#' @param fill A scalar value that fills in missing values when pivoting wider.
#'     Defaults to `NA`.
#' @param drop_na If `TRUE` (the default), will drop rows that contain only
#'     `NA`s in the `value` column when pivoting longer.
#'
#' @returns The pivoted data frame.
#'
#' @seealso [`pivot_longer()`][tidyr::pivot_longer],
#'     [`pivot_wider()`][tidyr::pivot_wider].
#'
#' @examples
#' library(dplyr)
#'
#' (x <- quitte_example_data %>%
#'         filter(first(scenario) == scenario,
#'                first(region) == region,
#'                variable %in% head(unique(variable), n = 7),
#'                period <= 2030) %>%
#'         slice_sample(prop = 0.9) %>%
#'         pivot_periods_wider())
#'
#' x %>%
#'     pivot_periods_longer()
#'
#' @importFrom dplyr first
#' @importFrom tidyr pivot_longer pivot_wider


#' @export
pivot_periods <- function(df, direction = NULL, fill = NA, drop_na = TRUE)
{
    stopifnot(is.data.frame(df))

    period_column <- \(df) first(intersect(c('period', 'Period'), colnames(df)))
    value_column  <- \(df) first(intersect(c('value',  'Value'),  colnames(df)))
    integer_columns <- \(df) grep('^[0-9]+$', colnames(df), value = TRUE)

    if (is.null(direction)) {
        if (!is.na(period_column(df))) {
            direction = 'wider'
        } else if (0 < length(integer_columns(df))) {
            direction = 'longer'
        } else {
            stop(paste('Cannot determin pivot direction.  No `period` column',
                       'and no columns with names matching integer numbers in',
                       '`df`.'))
        }
    }

    switch(
        direction,
        'wider' = {
            stopifnot('No `period` column in `df`.' =
                          !is.na(pivot_column <- period_column(df)))
            stopifnot('No `value` column in `df`.' =
                          !is.na(values_column <- value_column(df)))

            df %>%
                pivot_wider(
                    names_from = all_of(pivot_column), names_sort = TRUE,
                    values_from = all_of(values_column), values_fill = fill) %>%
                unquitte()
        },

        'longer' = {
            stopifnot(
                'No columns with names matching integer numbers in `df`.' =
                    0 < length(pivot_columns <- integer_columns(df)))
            stopifnot('`drop_na` must be `TRUE` or `FALSE`.' =
                          (isTRUE(drop_na) | isFALSE(drop_na)))

            df <- df %>%
                pivot_longer(cols = all_of(pivot_columns), names_to = 'period',
                             names_transform = as.integer,
                             values_drop_na = drop_na)

            if (looks_like_quitte(df)) {
                df <- as.quitte(df)
            }

            df
        },

        stop("`direction` must be `NULL`, 'wider', or 'longer'.")
    )
}

#' @export
#' @rdname pivot_periods
pivot_periods_wider <- function(df, fill = NA)
{
    pivot_periods(df = df, direction = 'wider', fill = fill)
}

#' @export
#' @rdname pivot_periods
pivot_periods_longer <- function(df, drop_na = TRUE)
{
    pivot_periods(df = df, direction = 'longer', drop_na = drop_na)
}

