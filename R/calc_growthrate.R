#' Calculates the growth rate of all variables in '%/yr' and returns a quitte data.frame
#' where each variable name gets ` [Growth Rate]` appended.
#'
#' @param x anything with an as.quitte method (data.frame, quitte or magclass object, mif file, ...)
#'
#' @author Oliver Richters
#'
#' @details
#' If, for example, your data contains the data in 2070 and 2060, the growth rate returned
#' for 2070 is calculated as 100 * ((d2070/d2060)^(1/10) - 1).
#' No growth rate can be calculated for the first year of the data.
#' Infinite or undefined values (for example if d2060 = 0) are dropped.

#' @importFrom dplyr filter group_by lag mutate rename select %>%
#'
#' @return the sorted quitte object
#' @export
calc_growthrate <- function(x) {
  x <- as.quitte(x, na.rm = TRUE)
  levels(x$variable) <- paste(levels(x$variable), "[Growth Rate]")
  x %>%
    group_by(.data$model, .data$scenario, .data$region, .data$variable) %>%
    mutate(diffyear = .data$period - lag(.data$period, order_by = .data$period),
           growthrate = 100 * ((.data$value/lag(.data$value, order_by = .data$period))^(1/.data$diffyear) - 1),
           unit = factor("%/yr")) %>%
    filter(is.finite(.data$growthrate)) %>%
    select(-"value", -"diffyear") %>%
    rename(value = "growthrate") %>%
    return()
}
