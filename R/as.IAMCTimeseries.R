#' Converts an IAMC long format into a wide format
#' 
#' Pivots periods to columns so that values are given as a time series table rather than a long list.
#'
#' @md
#' @param df A [`quitte`] data frame
#' @param df A fill value. Defaults to `NA`
#'
#' @author Michaja Pehl, Tonn Rüter
#'
#' @examples
#' write.IAMCxlsx(quitte_example_data, tempfile())
#'
#' @importFrom dplyr rename_with select
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect matches
#' @importFrom stringr str_to_title
#' @export
as.IAMCTimeseries <- function(df, fill = NA) {
  default_columns <- c("Model", "Scenario", "Region", "Variable", "Unit", "Period", "Value")
  # Assume `quitte` columns are always lower case
  ignored_columns <- setdiff(colnames(df), tolower(default_columns))
  if (length(ignored_columns) > 0) {
    warning("Data frame contains non standard column names. Ignore columns: ", paste(ignored_columns, collapse = ", "))
  }
  return(
    as.quitte(df) %>%
      # Convert lower case column names to Title Case (i.e. capitalize the first letter of each word)
      rename_with(
        .fn = str_to_title,
        # Make sure not to include `y2015` style periods
        .cols = matches("^[A-Za-z][A-Za-z0-9_]*$")
      ) %>%
      # Only consider the default columns
      select(all_of(default_columns)) %>%
      # Drop NAN values
      filter(is.finite(.data$Value), "" != .data$Value) %>%
      # Create new columns based on periods, then fill rows with values
      pivot_wider(names_from = "Period", names_sort = TRUE, values_from = "Value", values_fill = fill)
  )
}
