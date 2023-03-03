#' Bar plots with variable time steps
#'
#' Utility functions for plotting stacked bars with variable widths for
#' displaying time-series data with variable time steps (like REMIND data).
#'
#' `add_timesteps_columns()` calculates the x-axis position and width of bars
#' based on the information in `timesteps` and joins it to `data`.
#' `add_remind_timesteps_columns()` uses the [`remind_timesteps`] data frame.
#' `ggplot_bar_vts()` produces a bar plot with bars positioned according to
#' `timesteps`.  `ggplot_bar_remind_vts()` uses the [`remind_timesteps`] data
#' frame.
#'
#' @md
#' @param data A data frame.
#' @param timesteps A data frame like [`remind_timesteps`] with columns
#'   `period`, `year`, and `weight`, where `weight` determines which share of
#'   `year` belongs to `period`.
#' @param periods The column holding the period information in `data` (either a
#'   string or an object).  Defaults to `'period'`.
#' @param mapping [`aes()`][ggplot2::aes] mapping with aesthetics `x`, `y`, and
#'   optionally `fill`.
#' @param gaps Gaps between bars as a fraction of the smallest bar width.
#'     Defaults to 0.1 (e.g. 0.1 * 5 years = 0.5 years).
#' @param position_fill If `TRUE`, stacks bars and standardises each stack to
#'   have constant height.
#' @param timesteps_period character string giving the column name of the
#'   `period` in the `timesteps` data frame.  Defaults to `'period'`.
#' @param timesteps_interval character string giving the column name of the time
#'   interval in the `timesteps` data frame.  Defaults to `'year'`.
#' @param interval_shift numeric of length 2.  Shifts added to the interval fix
#'   point to obtain the beginning and end of time interval.
#'   If the interval for period 1 should be `[0.5, 1.5]`, `interval_shift`
#'   should be set to `c(-0.5, 0.5)` (default).  If the interval for period 1
#'   should be `[0, 1]`, `interval_shift` should be set to `c(-1, 0)`.
#'
#' @return `add_timesteps_columns()` and `add_remind_timesteps_columns()` return
#'   a data frame.
#'   `ggplot_bar_vts()` and `ggplot_bar_remind_vts()` return a
#'   [`ggplot()`][ggplot2::ggplot]-like object.
#'
#' @author Michaja Pehl
#'
#' @importFrom rlang get_expr is_empty
#'
#' @examples
#' require(tidyverse)
#'
#' # some example data
#' (data <- quitte_example_data %>%
#'     filter(first(scenario) == scenario,
#'            last(region) == region,
#'            first(variable) == variable))
#'
#' # adding individual timesteps
#' add_timesteps_columns(data, remind_timesteps)
#'
#' # adding remind timesteps with gaps
#' add_remind_timesteps_columns(data, gaps = 0.1)
#'
#' # plotting individual timesteps without gaps
#' ggplot_bar_vts(data, remind_timesteps, gaps = 0)
#'
#' # plotting remind timegaps, using further ggplot2 functions
#' ggplot_bar_remind_vts(
#'     data = quitte_example_data %>%
#'         filter(scenario %in% levels(quitte_example_data$scenario)[1:3],
#'                last(region) == region,
#'                grepl('PE\\|', variable),
#'                2100 >= period)) +
#'     scale_fill_manual(
#'         values = mip::plotstyle(grep('^PE\\|',
#'                                      levels(quitte_example_data$variable),
#'                                      value = TRUE))) +
#'     facet_wrap(~ scenario)
#'
#' # another data set with a different time column
#' data2 <- data.frame(variable = c('Wind', 'Solar', 'Wind', 'Solar'),
#'     tau = c(1,1,2,2),
#'     value = 1:4)
#'
#' # some timesteps dataframe with hourly data
#' timesteps <- data.frame(tau = c(rep(1,2),rep(2,4)),
#'                             hour = 1:6,
#'                             weight = 1)
#'
#' # plotting with different timesteps than periods and years
#' ggplot_bar_vts(data2, timesteps,
#'               mapping = aes(tau, value, group = variable, fill = variable),
#'               timesteps_period = 'tau',
#'               timesteps_interval = 'hour',
#'               interval_shift = c(-1,0))

#' @rdname variable_timesteps
#' @export
add_timesteps_columns <- function(data, timesteps, periods = 'period',
                                  gaps = 0,
                                  interval_shift = c(-0.5, 0.5),
                                  timesteps_period = 'period',
                                  timesteps_interval = 'year') {
    # ---- parse `periods` parameter ----
    periods <- substitute(periods)
    # is it a variable to be evaluated in the parent frame?
    if (exists(deparse(periods), parent.frame(), mode = 'character')) {
        periods <- eval(periods, parent.frame())
    # is it a promise to be used literally?
    } else if (is.symbol(periods)) {
        periods <- deparse(periods)
    }
    # must be a string then

    # ---- guardians ----
    if (!is.data.frame(data))
        stop('`data` must be a data frame.')

    if (!is.data.frame(timesteps))
        stop('`timesteps` must be a data frame.')

    if (!periods %in% colnames(data))
        stop('Column `', periods, '` is missing in `data`.')

    if (!all(is.character(timesteps_period), is.character(timesteps_interval)))
        stop('`timesteps_period` and `timesteps_interval` must be characters')

    missing.columns <- setdiff(
        c(timesteps_period, timesteps_interval, 'weight'), colnames(timesteps))

    if (length(missing.columns))
        stop('Column', ifelse(1 < length(missing.columns), 's', ''), ' ',
             paste(paste0('`', missing.columns, '`'), collapse = ', '), ' ',
             ifelse(1 < length(missing.columns), 'are', 'is'),
             ' missing in `timesteps`.')
    rm(missing.columns)

    if (!is.numeric(gaps) | 0 > gaps)
        stop('`gaps` must be a positive numerical.')

    # ---- join data frames ----
    # calculate absolute gap as fraction of smallest bar width
    gaps <- gaps * (timesteps %>%
                        group_by(!!sym(timesteps_period)) %>%
                        summarise(width = sum(!!sym('weight'))) %>%
                        getElement('width') %>%
                        min())
    inner_join(
        data,

        timesteps %>%
            arrange(!!sym(timesteps_interval)) %>%
            group_by(!!sym(timesteps_period)) %>%
            summarise(
                # start at first year belonging to period
                # whole numbers are mid-year, -0.5 is year start
                # add fractional part of the first year, if any
                # half of the gap on the left
                !!sym('start') := first(!!sym(timesteps_interval))
                                + interval_shift[1]
                                + first(!!sym('weight')) %% 1
                                + gaps / 2,

                # end at last year belonging to period
                # whole numbers are mid-year, +0.5 is year end
                # add fractional part of the last year, if any
                # half a gap on the right
                !!sym('end')   := last(!!sym(timesteps_interval))
                                + interval_shift[2]
                                - last(!!sym('weight')) %% 1
                                - gaps / 2,

                !!sym('xpos')  := (!!sym('start') + !!sym('end')) / 2,
                !!sym('width') := !!sym('end') - !!sym('start')) %>%
            ungroup() %>%
            select(all_of(timesteps_period), 'xpos', 'width'),

        setNames(timesteps_period, periods)
    )
}

#' @rdname variable_timesteps
#' @export
add_remind_timesteps_columns <- function(data, periods = 'period', gaps = 0) {
    # ---- parse `periods` parameter ----
    periods <- substitute(periods)
    # is it a variable to be evaluated in the parent frame?
    if (exists(deparse(periods), parent.frame(), mode = 'character')) {
        periods <- eval(periods, parent.frame())
        # is it a promise to be used literally?
    } else if (is.symbol(periods)) {
        periods <- deparse(periods)
    }
    # must be a string then

    # ---- guardians ----
    if (!is.data.frame(data))
        stop('`data` must be a data frame.')

    if (!periods %in% colnames(data))
        stop('Column `', periods, '` is missing in `data`.')

    if (!is.numeric(gaps) | 0 > gaps)
        stop('`gaps` must be a positive numerical.')

    # ---- call add_timestep_columns ----
    add_timesteps_columns(data, quitte::remind_timesteps, periods, gaps)
}

#' @rdname variable_timesteps
#' @export
ggplot_bar_vts <- function(data, timesteps,
                           mapping = aes(x = !!sym('period'),
                                         y = !!sym('value'),
                                         fill = !!sym('variable')),
                           gaps = 0.1, position_fill = FALSE,
                           interval_shift = c(-0.5, 0.5),
                           timesteps_period = 'period',
                           timesteps_interval = 'year') {

    # ---- parse mapping ----
    x    <- as.character(get_expr(mapping$x))
    y    <- as.character(get_expr(mapping$y))
    fill <- as.character(get_expr(mapping$fill))

    # ---- guardians ----
    if (!is.data.frame(data))
        stop('`data` must be a data frame.')

    if (!is.data.frame(timesteps))
        stop('`timesteps` must be a data frame.')

    missing.mappings <- which(sapply(list(x = x, y = y, fill = fill), is_empty))
    if (!is_empty(missing.mappings))
        stop('Mapping', ifelse(1 < length(missing.mappings), 's', ''), ' ',
             paste(names(missing.mappings), collapse = ', '), ' ',
             ifelse(1 < length(missing.mappings), 'are', 'is'), ' missing.')
    rm(missing.mappings)

    missing.columns <- setdiff(c(x, y, fill), colnames(data))
    if (length(missing.columns))
        stop('Column', ifelse(1 < length(missing.columns), 's', ''), ' ',
             paste(paste0('`', missing.columns, '`'), collapse = ', '), ' ',
             ifelse(1 < length(missing.columns), 'are', 'is'),
             ' missing in `data`.')

    # ---- plot ----
    ggplot() +
        geom_col(
            data = data %>%
                add_timesteps_columns(timesteps, x, gaps, interval_shift,
                                      timesteps_period, timesteps_interval),
            mapping = aes(x = !!sym('xpos'), y = !!sym(y),
                          width = !!sym('width'), fill = !!sym(fill)),
            position = ifelse(position_fill, 'fill', 'stack'))
}

#' @rdname variable_timesteps
#' @export
ggplot_bar_remind_vts <- function(data,
                                  mapping = aes(x = !!sym('period'),
                                                y = !!sym('value'),
                                                fill = !!sym('variable')),
                                  gaps = 0.1, position_fill = FALSE) {
    # ---- call ggplot_bar_vts() ----
    ggplot_bar_vts(data, quitte::remind_timesteps, mapping, gaps, position_fill)
}
