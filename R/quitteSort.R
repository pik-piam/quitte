#' Sorts a quitte object in a standardized way:
#' model -> scenario -> variable -> unit -> region -> period
#'
#' @param x A quitte object
#'
#' @author Oliver Richters
#'
#' @importFrom dplyr arrange relocate
#'
#' @return the sorted quitte object
#' @export
quitteSort <- function(x) {
  x <- as.quitte(x) %>%
    relocate('model', 'scenario', 'region', 'variable', 'unit', 'period')

  for (col in names(which('factor' == sapply(x, class)))) {
    x <- x %>%
      mutate(
        !!sym(col) := factor(.data[[col]], levels = sort(levels(.data[[col]]))))
  }

  x %>%
    arrange(.data$model, .data$scenario, .data$region, .data$variable,
            .data$unit, .data$period)
}
