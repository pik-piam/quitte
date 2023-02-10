#' Calculate new variables
#'
#' Calculate new variables from existing ones, using generic formulas.
#'
#' `...` is a list of name-value pairs with the general format
#' ```
#' "lhs" = "rhs + calculations - formula", "`lhs 2`" = "lhs / `rhs 2`"
#' ```
#' where `lhs` are the names of new variables to be calculated and
#' `rhs` are the variables to calculate from. If `lhs` and `rhs`
#' are no proper *identifiers*, they need to be quoted (see
#' [Quotes][base::Quotes] for details). When in doubt, just quote.
#'
#' If the new variables should have units, set `units` appropriately.
#'
#' `.dots` is a named list of strings denoting formulas and optionally
#' units. The general format is
#' ```
#' list("`lhs 1`" = "`rhs` / `calculation`",
#'      "`lhs 2`" = "sin(`rhs 2`)")
#' ```
#'
#' Units are optionally included with the formulas in a vector like
#' ```
#' list("`lhs w/ unit`" = c("`rhs 1` + `rhs 2`", "rhs unit")
#' ```
#' Units do not require quoting.
#'
#' `...` and `.dots` are processed in order, and variables already
#' calculated in the same call can be used for further calculations. Other
#' existing columns, including `period`, can be referenced, but this is
#' not supported and the results are considered *undefined*.
#'
#' @param data A data frame.
#' @param ... Name-value pairs of calculation formulas. See details.
#' @param units Character vector of units corresponding to new variables.  Must
#'   be of length equal to `...` or of length one (in which case all new
#'   variables receive the same unit).
#' @param na.rm If `TRUE` (the default), remove items calculated as `NA`.
#' @param completeMissing If `TRUE`, implicitly missing data, i.e. missing
#'   combinations of input data, are filled up with 0 before the calculation.
#'   Alternatively, you can provide a character vector with the names of the
#'   columns to be expanded.  Can interfere with `na.rm`.  Defaults to `FALSE`.
#' @param only.new If `FALSE` (the default), add new variables to existing
#'   ones.  If `TRUE`, return only new variables.
#' @param variable Column name of variables.  Defaults to `"variable"`.
#' @param unit Column name of units.  Defaults to `"unit"`.  Ignored if no
#'   column with the same name is in `data` (e.g. data frames without unit
#'   column).
#' @param value Column name of values.  Defaults to `"value"`.
#' @param .dots Used to work around non-standard evaluation.  See details.
#'
#' @return A data frame.
#'
#' @examples
#' data <- inline.data.frame(c(
#'     "model;    scenario;   region;   variable;     unit;                 period;   value",
#'     "REMIND;   Baseline;   USA;      GDP|MER;      billion US$2005/yr;   2010;     12990",
#'     "REMIND;   Baseline;   USA;      Population;   million;              2010;       310.4",
#'     "REMIND;   Baseline;   USA;      PE;           EJ/yr;                2010;        91.62",
#'     "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2020;      8882",
#'     "REMIND;   Baseline;   CHN;      GDP|MER;      billion US$2005/yr;   2010;      4119",
#'     "REMIND;   Baseline;   CHN;      Population;   million;              2020;      1387",
#'     "REMIND;   Baseline;   CHN;      Population;   million;              2010;      1349"))
#'
#' calc_addVariable(data, "GDPpC" = "`GDP|MER` / Population * 1e3",
#'                        "`ln GDPpC`" = "log(GDPpC)",
#'                        units = c("US$2005/cap", NA))
#' calc_addVariable_(
#'     data,
#'     list("`GDPpC`"    = c("`GDP|MER` / `Population` * 1e3", "US$/cap"),
#'          "`ln GDPpC`" = "log(`GDPpC`)")
#' )
#'
#' @author Michaja Pehl
#'
#' @importFrom dplyr anti_join bind_rows filter mutate select
#' @importFrom glue glue
#' @importFrom lazyeval f_eval interp
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang := is_false is_true sym syms
#' @importFrom stats formula setNames
#' @importFrom tidyr complete pivot_wider replace_na
#' @importFrom tidyselect all_of any_of
#'
#' @export
calc_addVariable <- function(data, ..., units = NA, na.rm = TRUE,
                             completeMissing = FALSE, only.new = FALSE,
                             variable = variable, unit = unit, value = value) {

  .dots    <- list(...)

  if (!all(is.na(units))) {
    if (length(units) == length(.dots)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units[i])
    } else if (1 == length(units)) {
      for (i in 1:length(.dots))
        .dots[i][[1]] <- c(.dots[i][[1]], units)
    } else
      stop('`units` must be of the same length as `...` or of length one.')
  }

  variable <- deparse(substitute(variable))
  unit     <- deparse(substitute(unit))
  value    <- deparse(substitute(value))

  calc_addVariable_(data, .dots, na.rm, completeMissing, only.new, variable,
                    unit, value)
}

#' @export
#' @rdname calc_addVariable
calc_addVariable_ <- function(data, .dots, na.rm = TRUE,
                              completeMissing = FALSE, only.new = FALSE,
                              variable = 'variable', unit = 'unit',
                              value = 'value') {
  . <- NULL

  # guardians ----
  if (!is.data.frame(data))
    stop('Only works with data frames')

  if (!is.list(.dots))
    stop('`.dots` must be a list of formula strings')

  .colnames <- colnames(data)
  for (column in c(variable, value)) {
    if (!column %in% .colnames) {
      stop(glue('No column \'{column}\' found'))
    }
  }

  duplicates <- data %>%
    group_by(!!!syms(setdiff(colnames(.), value))) %>%
    filter(1 < n()) %>%
    ungroup()

  if (nrow(duplicates)) {
    stop(paste(c('Duplicate rows in data.frame', format(duplicates)),
               collapse = '\n'))
  }

  # prepare `.dots` ----
  for (i in seq_along(.dots)) {
    .dots[[i]] <- list(
      name = gsub('`', '', names(.dots[i])),

      formula = paste0("~", .dots[[i]][1]) %>%
        gsub('\\n *', ' ', .) %>%
        formula() %>%
        interp(),

      unit = .dots[[i]][2]
    )

    .dots[[i]]$variables <- .dots[[i]]$formula %>%
      all.vars() %>%
      unique()
  }

  # fill missing data ----
  if (is_true(completeMissing)) {
    .expand_cols <- data %>%
      removeColNa() %>%
      colnames() %>%
      setdiff(value)
  } else if (!is_false(completeMissing)) {
    .expand_cols <- completeMissing
    completeMissing <- TRUE
  }

  if (completeMissing) {
    data %<>%
      droplevels() %>%
      complete(!!!syms(.expand_cols),
               fill = setNames(list(0), value))
  }

  # calculate new variables ----
  for (i in seq_along(.dots)) {
    data %<>%
      filter(.dots[[i]]$name != !!sym(variable)) %>%
      bind_rows(
        data %>%
          filter(!!sym(variable) %in% .dots[[i]]$variables) %>%
          select(!any_of(replace_na(unit, ''))) %>%
          pivot_wider(names_from = variable, values_from = value) %>%
          mutate(!!sym(value) := f_eval(f = .dots[[i]]$formula, data = .),
                 '{variable}' := .dots[[i]]$name,
                 '{unit}' := .dots[[i]]$unit) %>%
          select(all_of(.colnames))
      )
  }

  # clean up ----
  new_variables <- sapply(.dots, getElement, name = 'name')
  if (only.new) {
    data %<>%
      filter(!!sym(variable) %in% new_variables)
  }

  if (na.rm) {
    data %<>%
      filter(!(is.na(!!sym(value)) & !!sym(variable) %in% new_variables))
  }

  return(data)
}
