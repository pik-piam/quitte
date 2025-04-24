#' Converts an IAMC long format into a wide format
#' 
#' Pivots periods to columns so that values are given as a time series table rather than a long list.
#'
#' @param df A [`quitte`] data frame
#' @param fill A fill value. Defaults to `NA`
#'
#' @author Michaja Pehl, Tonn Rüter
#'
#' @examples
#' \dontrun{
#'   qf <- read.quitte("path/to/file") %>% as.IAMCTimeseries()
#' }
#'
#' @importFrom rlang .data
#' @importFrom dplyr rename_with select
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect matches
#' @importFrom stringr str_to_title
#' @export
as.IAMCTimeseries <- function(df, fill = NA) {
  # Silence "no visible binding for global variable '.'" build problems. We'll use colnames(.) later on. When using
  # . (dot) in a magrittr pipe, it refers to whatever is on the left hand side of %>%
  . <- NULL
  default_columns <- c("Model", "Scenario", "Region", "Variable", "Unit", "Period", "Value")
  return(
    as.quitte(df) %>%
      # Convert lower case column names to Title Case (i.e. capitalize the first letter of each word)
      rename_with(
        .fn = str_to_title,
        # Make sure not to include `y2015` style periods
        .cols = matches("^[A-Za-z][A-Za-z0-9_]*$")
      ) %>%
      select(
        # Select all the default columns (sans Value column) first
        setdiff(default_columns, last(default_columns)),
        # Then possible extra columns
        setdiff(colnames(.), default_columns),
        # Then the values column
        last(default_columns)) %>%
      # Drop NAN values
      filter(is.finite(.data$Value), "" != .data$Value) %>%
      # Make sure periods are sorted
      arrange(.data$Period) %>%
      # Create new columns based on periods, then fill rows with values
      pivot_wider(names_from = "Period", names_sort = TRUE, values_from = "Value", values_fill = fill)
  )
}
