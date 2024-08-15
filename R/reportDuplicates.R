#' Finds duplicates in a quitte object and warns about them per model and scenario.
#' Also warn if variables are identical but value or units differ.

#' @param mifdata object that can be converted with as.quitte
#' @importFrom dplyr filter mutate select
#' @return only the data that is duplicated. Has 0 rows if everything is fine
#' @export
reportDuplicates <- function(mifdata) {
  mifdata <- as.quitte(mifdata)
  duplicates <- mifdata %>% 
    group_by(.data$model, .data$region, .data$variable, .data$period, .data$scenario) %>% 
    filter(n() > 1) %>%
    droplevels()
  if (nrow(duplicates) > 0) {
    short <- duplicates %>%
      mutate(modelscen = factor(paste(.data$model, .data$scenario)),
             varunit = factor(paste0(.data$variable, " (", .data$unit, ")"))) %>%
      select(-c("period", "model", "scenario", "unit", "variable", "region", "value")) %>%
      unique() %>%
      droplevels()
    for (ms in levels(short$modelscen)) {
      shortms <- droplevels(filter(short, .data$modelscen %in% ms))
      warning("Duplicated data found for ", ms, ":\n",
              "Variables: ", paste(levels(shortms$varunit), collapse = ", "))
    }
  }
  return(duplicates)
}
