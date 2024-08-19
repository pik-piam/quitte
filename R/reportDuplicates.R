#' Checks for duplicates in quitte object
#'
#' Finds duplicates in a quitte object and warns about them per model and scenario.
#' Also warn if variables are identical but value or units differ.
#'
#' @param mifdata object that can be converted with as.quitte
#' @param action if set to 'warn', a warning with duplicate variables is raised
#' @importFrom dplyr filter mutate select
#' @return only the data that is duplicated. Has 0 rows if everything is fine
#' @export
reportDuplicates <- function(mifdata, action = "warn") {
  mifdata <- as.quitte(mifdata)
  duplicates <- mifdata %>%
    group_by(!!!syms(setdiff(colnames(mifdata), c("unit", "value")))) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    droplevels() %>%
    quitteSort()
  if (nrow(duplicates) > 0 && isTRUE(action == "warn")) {
    warning(paste(c(
      "Duplicated data found for:",
      duplicates %>%
        distinct(.data$model, .data$scenario, .data$variable, .data$unit) %>%
        group_by(.data$model, .data$scenario) %>%
        summarise(
          text = paste0(
            unique(.data$model), " ", unique(.data$scenario), ": ",
            paste0(.data$variable, " (", .data$unit, ")", collapse = ", ")),
          .groups = 'drop') %>%
        pull('text')),
      collapse = '\n  '))
  }
  return(invisible(duplicates))
}
