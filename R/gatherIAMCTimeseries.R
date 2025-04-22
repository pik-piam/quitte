#' gatherIAMCTimeseries
#'
#' Pivots periods columns (as they occur in IAMC wide format/timeseries) to the typical `quitte` long list format
#'
#' @md
#' @param df `quitte` data frame in IAMC wide format (i.e. years/periods are columns)
#' @param quitte_case In IAMC case columns first letters are capitalized, quitte case is lower case. Default is FALSE
#' @return `quitte` data frame in long IAMC (= `quitte`) format
#' @importFrom quitte interpolate_missing_periods as.IAMCTimeseries
#' @importFrom dplyr all_of matches relocate rename_with select
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_title str_to_lower
#' @examples
#' \dontrun{
#' df <- quitte_example_data %>% as.IAMCTimeseries()
#' df %>% gatherIAMCTimeseries()
#' }
#' @author Tonn Rüter
#' @export
gatherIAMCTimeseries <- function(df, quitte_case = FALSE) {
  period_transform <- list("period" = as.integer)
  # IAMC long format default columns. Note: `quitte` uses lower case, IAMC title case
  default_columns <- c("Model", "Scenario", "Region", "Variable", "Unit", "Period", "Value")
  # Check if df might already be a proper `quitte` data frame. Suppress warning in case
  # the vectors have different lengths
  if (suppressWarnings(all(colnames(df) == tolower(default_columns))))
    stop("data frame must be in IAMC wide format")
  # Strip year columns
  periods <- df %>%
    select(all_of(matches("^\\d+$"))) %>%
    colnames()
  # Extract extra column in case there are any. Remember: IAMC uses title case
  # Note: 
  extra_columns <- setdiff(colnames(df), union(default_columns, periods))
  period_and_value <- c("Period", "Value")
  if (any(period_and_value %in% extra_columns))
    stop("data frame cannot contain 'Period', 'Value' as extra columns")
  # Sans "Period" and "Value"
  default_columns <- default_columns[!default_columns %in% period_and_value]
  if (quitte_case) {
    default_columns  <- tolower(default_columns)
    extra_columns    <- tolower(extra_columns)
    period_and_value <- tolower(period_and_value)
  }
  return(
    df %>%
      # Melt the wide format into long format
      pivot_longer(
        periods,
        names_to = if (quitte_case) "period" else "Period",
        names_transform = period_transform,
        values_to = "value"
      ) %>%
      # `quitte` uses lower case col names
      rename_with(
        .fn = if (quitte_case) str_to_lower else str_to_title,
        .cols = matches("^[A-Za-z][A-Za-z0-9_]*$")
      ) %>%
      # Rearange columns in typical order
      relocate(all_of(c(default_columns, extra_columns, period_and_value)))
  )
}
