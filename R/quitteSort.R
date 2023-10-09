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
  model <- scenario <- region <- variable <- unit <- period <- NULL
  x <- as.quitte(x) %>%
    relocate(model, scenario, region, variable, unit, period) %>%
    arrange(model, scenario, variable, unit, region, period) %>%
    return()
}
