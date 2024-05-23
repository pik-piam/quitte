#' Calculates the growth rate in '%/yr' for variables
#'
#' @param x anything with an as.quitte method (data.frame, quitte or magclass object, mif file, ...)
#' @param only.new If `FALSE` (the default), add new variables to existing
#'   ones.  If `TRUE`, return only new variables.
#' @param filter.function A function used to filter data before calculating growth rates.
#'   If instead a character vector is passed, only variables matching this vector are used.
#'
#' @author Oliver Richters
#'
#' @details
#' If, for example, your data contains the data in 2070 and 2060, the growth rate returned
#' for 2070 is calculated as 100 * ((d2070/d2060)^(1/10) - 1).
#' No growth rate can be calculated for the first year of the data.
#' Infinite or undefined values (for example if d2060 = 0) are dropped.
#'
#' @examples
#' \dontrun{
#' GDPgrowth <- calc_growthrate(quitte_example_data, only.new = TRUE, filter.function = "GDP|PPP")
#' alldata <- calc_growthrate(quitte_example_data, only.new = FALSE,
#'                            filter.function = function(x) filter(x, grepl("GDP", .data$variable)))
#' }
#'
#' @importFrom dplyr filter group_by lag mutate rename select %>%
#' @importFrom rlang .data
#'
#' @return data as a quitte object
#' @export
calc_growthrate <- function(x, only.new = FALSE, filter.function = identity) {
  # transform character vector into filter function
  if (is.character(filter.function)) {
    vars <- filter.function
    filter.function <- function(x) return(filter(x, .data$variable %in% vars))
  }
  # remove na and filter data
  xg <- as.quitte(x, na.rm = TRUE) %>%
    filter.function()
  # rename variables
  levels(xg$variable) <- paste(levels(xg$variable), "[Growth Rate]")
  # calculate growth rates
  xg <- xg %>%
    group_by(.data$model, .data$scenario, .data$region, .data$variable) %>%
    mutate(diffyear = .data$period - lag(.data$period, order_by = .data$period),
           growthrate = 100 * ((.data$value/lag(.data$value, order_by = .data$period))^(1/.data$diffyear) - 1),
           unit = factor("%/yr")) %>%
    filter(is.finite(.data$growthrate)) %>%
    select(-"value", -"diffyear") %>%
    rename(value = "growthrate") %>%
    droplevels()
  # return data
  if (isTRUE(only.new)) {
    return(xg)
  } else {
    return(rbind(x, xg))
  }
}
