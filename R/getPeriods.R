#' Retrieves periods from a quitte object
#'
#' Retrieves periods from a quitte object
#'
#'
#' @param df quitte object
#' @return a numeric vector containing the values of the column without
#'   duplicates
#' @author Antoine Levesque
#' @examples
#'
#'   \dontrun{
#'     getPeriods(dataframe)
#'   }
#' @export



getPeriods <- function(df){
  tmp = getColValues(df,"period")
  return(tmp)
}
