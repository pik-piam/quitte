#' Retrieves scenarios from a quitte object
#'
#' Retrieves scenarios from a quitte object
#'
#'
#' @param df quitte object
#' @return a character vector containing the values of the column without
#'   duplicates
#' @author Antoine Levesque
#' @examples
#'
#'   \dontrun{
#'     getPeriods(dataframe)
#'   }
#' @export



getScenarios <- function(df){
  tmp = getColValues(df,"scenario")
  return(tmp)
}
