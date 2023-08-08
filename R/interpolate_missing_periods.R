#' Interpolate missing periods
#'
#' Adds missing periods to data frame and interpolates missing values linearly
#' or using splines from adjacent existing ones. Values for periods
#' smaller/bigger than the existing ones can be filled with the values for the
#' first/last available period in the case of linear interpolation.
#'
#' @md
#' @param data A data frame or a quitte object.
#' @param ... A name-value pair of periods to fill. If unnamed, defaults to
#'     `'period'`. If empty (but possibly named) uses only periods present in
#'     `data`.
#' @param periods A named list of periods to fill.
#' @param value Name of the column to fill, defaults to `'value'`.
#' @param expand.values If `FALSE` (the default), values are not expanded
#'     beyond the range of available data.  If `TRUE` values at the closest
#'     extreme is used for linear interpolation.  Results for spline
#'     interpolation are possibly nonsensical.
#' @param method Specifies the interpolation method. Either `'linear'` for
#'     linear interpolation or `'spline'`, `'spline_fmm'`, or `'spline_natural'`
#'     for spline interpolation.  `'spline'` is an alias for `'spline_fmm'`.
#'     See [spline()] for details.
#' @param combinations Specifies the method with which other columns are
#'     treated.  They are either preserved as-is (`'nesting'`, the default), or
#'     are expanded to all unique combinations (`'crossing'`).
#'     See [tidyr::expand()] for details.
#'
#' @return A data frame or a quitte object, the same as `data`.
#' @author Michaja Pehl
#'
#' @importFrom rlang :=
#' @importFrom stats na.omit spline
#' @importFrom tidyr complete nesting crossing
#' @importFrom zoo na.approx
#'
#' @examples
#' require(dplyr)
#'
#' # generate some test data with explicit (A-y-2025) and implicit (B-x-2030)
#' # missing values
#' (data <- tibble(
#'     group  = rep(c('A', 'B'), c(8, 4)),
#'     item   = c(rep('x', 4), rep('y', 4), rep('x', 4)),
#'     period = rep(c(2015, 2025, 2030, 2035), 3),
#'     value  = c(2, 4, 5, 6, 20, NA, 50, 60, NA, 400, 500, NA)))
#'
#' # fill values for already existing periods
#' interpolate_missing_periods(data)
#'
#' # fill values for existing periods, with full combinations of other columns
#' interpolate_missing_periods(data, combinations = 'crossing')
#'
#' # add additional periods and fill values
#' interpolate_missing_periods(data, period = seq(2010, 2035, 5))
#'
#' # also fill values outside the original data range
#' interpolate_missing_periods(data, seq(2010, 2035, 5), expand.values = TRUE)
#'
#' # works on data frames with different column names
#' (data <- data %>%
#'         rename(year = period, coeff = value))
#'
#' interpolate_missing_periods(data, year, value = 'coeff')
#'
#' # works on quitte objects too
#' (quitte <- data %>%
#'         rename(model = group, scenario = item, period = year, value = coeff) %>%
#'         mutate(variable = 'Var 1', unit = 'u1') %>%
#'         as.quitte())
#'
#' interpolate_missing_periods(quitte, expand.values = TRUE)
#'
#' # and works with POSIXct periods
#' (quitte <- quitte %>%
#'         mutate(period = ISOyear(period)))
#'
#' interpolate_missing_periods(quitte, period = ISOyear(seq(2010, 2035, 5)))
#'
#' # standard evaluation example
#' interpolate_missing_periods_(data, periods = list(year = seq(2010, 2035, 5)),
#'                              value = 'coeff', expand.values = TRUE)

#' @export
interpolate_missing_periods <- function(data, ..., value = 'value',
                                        expand.values = FALSE,
                                        method = 'linear',
                                        combinations = 'nesting') {

    # ---- normalise periods to a named list of numerics/POSIXct ----
    periods <- lazyeval::lazy_dots(...)[1]

    if (is.null(periods[[1]])) {
        periods <- stats::setNames(
            list(unique(lazyeval::lazy_eval('period', data))),
            'period')
    } else {
        periods <- stats::setNames(
            list(lazyeval::lazy_eval(periods[[1]], data)),
            ifelse('' == names(periods),
                   ifelse(as.character(periods[[1]]$expr) %in% colnames(data),
                          as.character(periods[[1]]$expr),
                          'period'),
                   names(periods))
        )
    }

    interpolate_missing_periods_(data, periods, value, expand.values, method,
                                 combinations)
}

#' @export
#' @rdname interpolate_missing_periods
interpolate_missing_periods_ <- function(data, periods, value = 'value',
                                         expand.values = FALSE,
                                         method = 'linear',
                                         combinations = 'nesting') {

    period <- names(periods[1])

    # ---- guardians ----
    if (!is.data.frame(data))
        stop("Works only on data frames")

    if (!period %in% colnames(data))
        stop('period column \'', period, '\' not found')

    if (!is.numeric(data[[period]]) && !lubridate::is.POSIXct(data[[period]]))
        stop('period column class must be of either \'numeric\' or \'POSIXct\'')

    if (!value %in% colnames(data))
        stop('value column \'', value, '\' not found')

    return_quitte <- is.quitte(data)

    if (!method %in% c('linear', 'spline', 'spline_fmm', 'spline_natural'))
        stop('method must be one of linear, spline, spline_fmm, or',
             ' spline_natural')

    # ---- convert POSIXct periods to integer ----
    if (return_POSIXct <- inherits(getElement(data, period), 'POSIXct')) {
        data <- data %>%
            mutate(!!sym(period) := as.integer(!!sym(period)))
        periods[[1]] <- as.integer(periods[[1]])
    }

    # ---- expand periods to include missing entries ----
    # columns for which combinations are to be preserved/expanded
    combination_columns <- setdiff(colnames(data), c(period, value))


    # store column names ----
    col_names <- colnames(data)

    if ('nesting' == combinations) {
        data <- data %>%
            complete(nesting(!!!syms(combination_columns)),
                     !!sym(period) := periods[[1]])
    } else if ('crossing' == combinations) {
        data <- data %>%
            complete(crossing(!!!syms(combination_columns)),
                     !!sym(period) := periods[[1]])
    } else {
        stop('combinations must be one of nesting or crossing')
    }

    data <- data %>%
        group_by(!!!syms(combination_columns))

    # ---- fill in missing periods ----
    # choose method
    if ('linear' != method) {
        if ('spline' == method) {
            method <- 'fmm'
        } else {
            method <- sub('spline_', '', method)
        }
    }

     # ---- calculate missing values ----
    .set_na <- function(a, b) {   # !diagnostics suppress=.set_na
        # set a to NA for all NA on the fringes of b
        if (!all(is.na(b))) {
            is.na(a) <- setdiff(
                seq_along(a),
                seq_range(range(which(!is.na(b) | expand.values))))
        } else {
            is.na(a) <- is.na(b)
        }
        return(a)
    }

    if ('linear' == method) {
        data <- data %>%
            arrange(!!!syms(c(combination_columns, period))) %>%
            mutate(!!sym(value) := na.approx(
                object = !!sym(value),
                x = !!sym(period),
                yleft = first(na.omit(!!sym(value))),
                yright = last(na.omit(!!sym(value))),
                na.rm = FALSE) %>%
                    .set_na(!!sym(value)))

        if (expand.values) {
            # if there is only one non-NA value, zoo::na.approx() does nothing,
            # so replace NAs with the one non-NA value
            data <- bind_rows(
                # all groups that don't consist of just one non-NA value
                # (including all NAs)
                data %>%
                    filter(1 != sum(!is.na(!!sym(value)))),

                # all groups consisting of just one non-NA value
                data %>%
                    filter(1 == sum(!is.na(!!sym(value)))) %>%
                    mutate(!!sym(value) := na.omit(!!sym(value)))
            )
        }
    } else {
        data <- bind_rows(
            # if all values are NA, no interpolation is possible
            data %>%
                filter(all(is.na(!!sym(value)))),

            # if not all values are NA, then interpolate
            data %>%
                filter(!all(is.na(!!sym(value)))) %>%
                arrange(!!!syms(c(combination_columns, period))) %>%
                mutate(!!sym(value) := spline(x = !!sym(period),
                                              y = !!sym(value),
                                              method = method,
                                              xout = !!sym(period)) %>%
                           getElement('y') %>%
                           unlist(use.names = FALSE) %>%
                           .set_na(!!sym(value)))
        )
    }

    # ---- return data ----
    data <- data %>%
        ungroup() %>%
        select(all_of(col_names))


    if (return_POSIXct)
        data <- data %>%
        mutate(!!sym(period) := as.POSIXct(!!sym(period),
                                           origin = '1970-01-01 00:00.00'))

    if (return_quitte)
        data <- as.quitte(data)

    # issue warning if splines are extended beyond original data range
    if ('linear' != method && expand.values)
        warning('Expanded values of spline interpolation beyond original data',
                ' range. Results may be nonsensical.')

    return(data)
}
