#' Revalue data frame factor levels.
#'
#' Revalue the names of a level or character column in a dataframe, according to a named vector given as an input
#'
#' @param df A data frame (or quitte object).
#' @param ... Name-value pairs assigning a named vector with new names to a column from the dataframe.
#' @param dots A named list of columns containing the named vector with the old and new names for each column
#' @return A data frame (or quitte object, same as `data`).
#' @author Antoine Levesque
#' @examples
#'
#' data <- inline.data.frame(c(
#'   "model;    scenario;   region;   variable;           unit;         period;   value",
#'   "REMIND;   Baseline;   USA;      GDP per Capita|MER; US$2005/yr;   2010;     40000",
#'   "REMIND;   Baseline;   USA;      Population;         million;      2010;     300",
#'   "REMIND;   Baseline;   CHN;      GDP per Capita|MER; US$2005/yr;   2010;     7000"))
#'
#'reg_vec = c(USA = "United States")
#'var_vec = c("GDP per Capita|MER" = "gdp",
#'            Population = "pop")
#'
#'revalue.levels(data,region = reg_vec)
#'revalue.levels_(data,list(region = reg_vec, variable = var_vec))
#'
#'@export
revalue.levels <- function(df, ...) {

  dots <- list(...)

  revalue.levels_(df, dots)
}

#' @importFrom plyr revalue
#' @export
#' @rdname revalue.levels
revalue.levels_ <- function(df, dots) {

  # guardians
  if (!is.data.frame(df))
    stop('only works on data frames')

  dot.names <- names(dots)

  for (column in dot.names) {

    reval_vec = getElement(dots, column)
      df[, column] <-revalue(getElement(df, column), replace = reval_vec)

  }

  return(df)
}
