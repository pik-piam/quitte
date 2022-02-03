#' Revalue data frame factor levels.
#'
#' Revalue the names of a level or character column in a dataframe, according to
#' a named vector given as an input.
#'
#' @param df A data frame (or quitte object).
#' @param ... Named vectors of named chr, or a list of named vectors of named chr.
#'   The names of the vectors must correspond to column names of df. The vectors themselves
#'   should have as names the the current values in the column to be replaced, and as values the new
#'   (replacement) values.
#' @return A data frame (or quitte object, same as \code{data}).
#' @author Antoine Levesque, Johannes Koch
#' @examples
#'
#' data <- inline.data.frame(c(
#'   "model;    scenario;   region;   variable;           unit;         period;   value",
#'   "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2010;     40000",
#'   "REMIND;   Baseline;   USA;      Population;         million;      2010;     300",
#'   "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2010;     7000"))
#'
#'reg_vec = c(USA = "United States")
#'var_vec = c("GDP per Capita|MER" = "gdp", Population = "pop")
#'
#'revalue.levels(data, region = reg_vec)
#'revalue.levels(data, list(region = reg_vec, variable = var_vec))
#'
#' @importFrom rlang !!!
#' @export
revalue.levels <- function(df, ...) {
  # Check is df is a data frame
  if (!is.data.frame(df)) {
    rlang::abort("Bad argument. Argument 'df' must be a data frame.")
  }

  # ... can be either a named list, or a multiple named vectors.
  # If the second, case, transform to named list.
  myList <- if (...length() == 1 && is.list(..1)) ..1 else list(...)

  for (col in names(myList)) {
    df[, col] <- dplyr::recode(df[[col]], !!!myList[[col]])
  }

  df
}
