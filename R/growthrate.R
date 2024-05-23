#' Calculates the growth rate of all variables in '%/yr' and returns a sorted data.frame
#' where each variable name gets '|Growth rate' appended.
#' If, for example, your data contains the data in 2070 and 2060, the growth rate returned
#' for 2070 is calculated as 100 * ((d2070/d2060)^(1/10) - 1).
#' No growth rate can be calculated for the first year of the data.
#' Infinite or undefined values (for example if d2060 = 0) are dropped.
#'
#' @param x A quitte object
#'
#' @author Oliver Richters
#'
#' @importFrom dplyr group_by mutate filter select rename
#'
#' @return the sorted quitte object
#' @export
growthrate <- function(x) {
  x %>%
    as.quitte(na.rm = TRUE) %>%
    quitteSort() %>%
    group_by(.data$model, .data$scenario, .data$region, .data$variable) %>%
    mutate(diffyear = .data$period - lag(.data$period)) %>%
    mutate(growthrate = 100 * ((.data$value/lag(.data$value))^(1/.data$diffyear) - 1)) %>%
    filter(! .data$growthrate %in% c(NA, Inf, NaN)) %>%
    mutate(variable = factor(paste0(.data$variable, "|Growth rate"))) %>%
    mutate(unit = factor("%/yr")) %>%
    select(-"value", -"diffyear") %>%
    rename(value = "growthrate") %>% 
    return()
}
