#' Add Scenario Change
#'
#' These functions add the absolute increase over, the ratio relative to, or the
#' percent change relative to a reference scenario.  Variable names have
#' `|Int over <reference_short_name>`, `|Ratio to <reference_short_name>`, or
#' `|% Change to <reference_short_name>` appended, units are either unmodified,
#' or set to `ratio` or `%`, respectively.
#'
#' @param d A quitte-like data frame.
#' @param variable A vector of variable names.
#' @param reference_scenario The scenario changes are calculated to.
#' @param reference_short_name Short name of reference scenario (appended to the
#'     variable name).
#'
#' @returns A quitte-like data frame with the calculated changes.
#'
#' @examples
#' quitte_example_data |>
#'     calc_addScenIncrease(c('Consumption', 'GDP|PPP'),
#'                          'r7552c_REF_Def05-rem-5', 'REF_Def05')
#'
#' @importFrom cli cli_abort
#' @importFrom dplyr filter left_join mutate select
#' @importFrom rlang .data .env
#' @importFrom tidyselect all_of

#' @export
calc_addScenIncrease <- function(d,
                                 variable,
                                 reference_scenario,
                                 reference_short_name = reference_scenario)
{
    calc_addScen_something(d, variable, reference_scenario,
                           reference_short_name, operation = 'increase')
}

#' @rdname calc_addScenIncrease
#' @export
calc_addScenRatio <- function(d,
                              variable,
                              reference_scenario,
                              reference_short_name = reference_scenario)
{
    calc_addScen_something(d, variable, reference_scenario,
                           reference_short_name, operation = 'ratio')
}

#' @rdname calc_addScenIncrease
#' @export
calc_addScenPercentChange <- function(d,
                                      variable,
                                      reference_scenario,
                                      reference_short_name = reference_scenario)
{
    calc_addScen_something(d, variable, reference_scenario,
                           reference_short_name, operation = 'percent change')
}

calc_addScen_something <- function(d,
                                   variable,
                                   reference_scenario,
                                   reference_short_name = reference_scenario,
                                   operation = c('increase', 'ratio',
                                                 'percent change'))
{
    if (!is.data.frame(d)) {
        cli_abort('{.arg d} must be a data frame, not {.obj_type_friendly d}.')
    }

    if (!'scenario' %in% colnames(d)) {
        cli_abort('{.arg d} must have a column {.field scenario}.')
    }

    if (!'variable' %in% colnames(d)) {
        cli_abort('{.arg d} must have a column {.field variable}.')
    }

    if (!'unit' %in% colnames(d)) {
        cli_abort('{.arg d} must have a column {.field unit}.')
    }

    if (!'value' %in% colnames(d)) {
        cli_abort('{.arg d} must have a column {.field value}.')
    }

    if (!all(is_present <- variable %in% d[['variable']])) {
        cli_abort(c(
            '{.arg variable} must be a value in column {.field variable} of {.arg d}.',
            i = '{.val {variable[!is_present]}} is not present.'
        ))
    }

    if (!reference_scenario %in% d[['scenario']]) {
        cli_abort(c(
            '{.arg reference_scenario} must be a value in column {.field scenario} of {.arg d}.',
            i = '{.val {reference_scenario}} is not present.'
        ))
    }

    operation <- match.arg(operation)

    if (operation == 'increase') {
        value_op    <- \(value, ref)    value - ref
        variable_op <- \(variable, rsn) paste0(variable, '|Inc over ', rsn)
        unit_op     <- \(unit)          unit
    } else if (operation == 'ratio') {
        value_op    <- \(value, ref)    value / ref
        variable_op <- \(variable, rsn) paste0(variable, '|Ratio to ', rsn)
        unit_op     <- \(unit)          'ratio'
    } else if (operation == 'percent change') {
        value_op    <- \(value, ref)   (value / ref - 1) * 100
        variable_op <- \(variable, rsn) paste0(variable, '|% Change to', rsn)
        unit_op     <- \(unit)          '%'
    }

    left_join(
        d |>
            filter(.data$scenario != reference_scenario,
                   .data$variable %in% .env$variable),

        d |>
            filter(.data$scenario == reference_scenario,
                   .data$variable %in% .env$variable) |>
            select(-'scenario', !!reference_short_name := 'value'),

        by = setdiff(colnames(d), c('scenario', 'value'))
    ) |>
        mutate(
            value = value_op(.data$value, .data[[reference_short_name]]),
            variable = variable_op(.data$variable, reference_short_name),
            unit = unit_op(.data$unit)) |>
        select(-all_of(reference_short_name))
}
