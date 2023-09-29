#' Sorts a quitte object in a standardized way:
#' model -> scenario -> variable -> unit -> region -> period
#'
#' @param x A quitte object
#'
#' @author Oliver Richters
#'
#' @importFrom dplyr arrange relocate
#'
#' @export
quitteSort <- function(x) {
  model <- scenario <- region <- variable <- unit <- period <- NULL
  if (! is.quitte(x)) x <- as.quitte(x)
  x <- x %>%
    relocate(model, scenario, region, variable, unit, period) %>%
    arrange(model, scenario, variable, unit, region, period)
  return(x)
}
