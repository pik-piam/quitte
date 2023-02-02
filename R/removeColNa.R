#' Removes all NA columns of a data frame
#'
#' `removeColNa()` Removes all columns of a data frame for which all entries are
#'  NA, or the default of fct_explict_na
#'
#' @param df a data frame
#' @return a data frame
#' @author Antoine Levesque
#' @examples
#' df <- data.frame(
#'     character = letters[1:5],
#'     factor = as.factor(LETTERS[1:5]),
#'     value = 1:5,
#'     unit = NA,
#'     unit2 = forcats::fct_na_value_to_level(factor(NA), level = '(Missing)'),
#'     stringsAsFactors = FALSE)
#' str(df)
#' str(removeColNa(df))
#' @importFrom forcats fct_na_value_to_level
#'
#' @export
#'

removeColNa <- function(df){

  .fct_default = levels(fct_na_value_to_level(factor(NA), level = '(Missing)'))

  df = df[colSums(!is.na(df)) > 0]
  df = df[colSums(df != .fct_default, na.rm= TRUE) > 0]

return(df)
}
