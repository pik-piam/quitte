#' Bar plots with variable time steps
#'
#' Utility functions for plotting stacked bars with variable widths for
#' displaying time-series data with variable time steps (like REMIND data).
#'
#' \code{add_timesteps_columns()} calculates the x-axis position and width of
#' bars based on the information in \code{timesteps} and joins it to
#' \code{data}.  \code{add_remind_timesteps_columns()} uses the
#' \code{\link{remind_timesteps}} data frame.  \code{ggplot_bar_vts()} produces
#' a bar plot with bars positioned according to \code{timesteps}.
#' \code{ggplot_bar_remind_vts()} uses the \code{\link{remind_timesteps}} data
#' frame.
#'
#'
#' @param data A data frame.
#' @param timesteps A data frame like \code{\link{remind_timesteps}} with
#'     columns \code{period}, \code{year}, and \code{weight}, where
#'     \code{weight} determines which share of \code{year} belongs to
#'     \code{period}.
#' @param periods The column holding the period information in \code{data}
#'     (either a string or an object).  Defaults to \code{'period'}.
#' @param mapping \code{\link[ggplot2:aes]{aes()}} mapping with aesthetics
#'     \code{x}, \code{y}, and optionally \code{fill}.
#' @param gaps Gaps between bars as a fraction of the smallest bar width.
#'     Defaults to 0.1 (e.g. 0.1 * 5 years = 0.5 years).
#'
#' @return \code{add_timesteps_columns()} and
#'     \code{add_remind_timesteps_columns()} return a data frame.
#'     \code{ggplot_bar_vts()} and \code{ggplot_bar_remind_vts()} return a
#'     \code{\link[ggplot2:ggplot]{ggplot()}}-like object.
#'
#' @author Michaja Pehl
#'
#' @importFrom rlang get_expr
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

#' @rdname variable_timesteps
#' @export
add_timesteps_columns <- function(data, timesteps, periods = 'period',
                                  gaps = 0) {
    # ---- parse `periods` parameter ----
    periods <- substitute(periods)
    # is it a variable to be evaluated in the parent frame?
    if (exists(deparse(periods), parent.frame())) {
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

    missing.columns <- setdiff(c('period', 'year', 'weight'),
                               colnames(timesteps))
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
                        group_by(!!sym('period')) %>%
                        summarise(width = sum(!!sym('weight'))) %>%
                        getElement('width') %>%
                        min())
    inner_join(
        data,

        timesteps %>%
            group_by(!!sym('period')) %>%
            summarise(
                # start at first year belonging to period
                # whole numbers are mid-year, -0.5 is year start
                # add fractional part of the first year, if any
                # half of the gap on the left
                !!sym('start') := first(!!sym('year'))
                                - 0.5
                                + first(!!sym('weight')) %% 1
                                + gaps / 2,

                # end at last year belonging to period
                # whole numbers are mid-year, +0.5 is year end
                # add fractional part of the last year, if any
                # half a gap on the right
                !!sym('end')   := last(!!sym('year'))
                                + 0.5
                                - last(!!sym('weight')) %% 1
                                - gaps / 2,

                !!sym('xpos')  := (!!sym('start') + !!sym('end')) / 2,
                !!sym('width') := !!sym('end') - !!sym('start')) %>%
            ungroup() %>%
            select('period', 'xpos', 'width'),

        setNames(periods, 'period')
    )
}

#' @rdname variable_timesteps
#' @export
add_remind_timesteps_columns <- function(data, periods = 'period', gaps = 0) {
    # ---- parse `periods` parameter ----
    periods <- substitute(periods)
    # is it a variable to be evaluated in the parent frame?
    if (exists(deparse(periods), parent.frame())) {
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
                           mapping = aes_string(x = 'period', y = 'value',
                                                fill = 'variable'),
                           gaps = 0.1) {

    # ---- parse mapping ----
    x    <- as.character(get_expr(mapping$x))
    y    <- as.character(get_expr(mapping$y))
    fill <- as.character(get_expr(mapping$fill))
    if (!length(fill))
        fill <- NULL

    # ---- guardians ----
    if (!is.data.frame(data))
        stop('`data` must be a data frame.')

    if (!is.data.frame(timesteps))
        stop('`timesteps` must be a data frame.')

    missing.mappings <- NULL
    if (!length(x))
        missing.mappings <- 'x'
    if (!length(y))
        missing.mappings <- c(missing.mappings, 'y')
    if (length(missing.mappings))
        stop('Mapping', ifelse(1 < length(missing.mappings), 's', ''), ' ',
             paste(missing.mappings, collapse = ', '), ' ',
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
        geom_col(data = data %>%
                     add_timesteps_columns(timesteps, x, gaps),
                 mapping = aes_string(x = 'xpos', y = y, width = 'width',
                                      fill = fill))
}

#' @rdname variable_timesteps
#' @export
ggplot_bar_remind_vts <- function(data,
                                  mapping = aes_string(x = 'period',
                                                       y = 'value',
                                                       fill = 'variable'),
                                  gaps = 0.1) {
    # ---- call ggplot_bar_vts() ----
    ggplot_bar_vts(data, quitte::remind_timesteps, mapping, gaps)
}
